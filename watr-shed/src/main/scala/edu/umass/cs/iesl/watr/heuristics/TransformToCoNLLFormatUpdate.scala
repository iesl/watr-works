package edu.umass.cs.iesl.watr
package heuristics

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}

import corpora._
import corpora.filesys._
import corpora.database.CorpusAccessDB
import corpora.database.CorpusAccessDBTables
import edu.umass.cs.iesl.watr.geometry.LTBounds
import watrmarks.{Label, StandardLabels => LB}
import GenericHeuristics._
import AuthorNameHeuristics._
import AffiliationsHeuristics._
import Utils._
import textreflow.data._
import scala.util.control.Breaks._

import scala.collection.mutable.ListBuffer

class TransformToCoNLLFormatUpdate(
  corpusAccessApi: CorpusAccessApi
) {

  lazy val textReflowDB = corpusAccessApi.corpusAccessDB
  lazy val docStore: DocumentZoningApi = textReflowDB.docStore
  lazy val corpus = corpusAccessApi.corpus


  def getLabelForTargetRegion(docStore: DocumentZoningApi, targetRegionId: Int@@RegionID, labels: Seq[Label]): Label = {

    for (label <- labels) {
      if (docStore.getZoneForRegion(regionId = targetRegionId, label = label).isDefined) {
        return label
      }
    }
    LB.NullLabel
  }

  def areLabelsPresentInPage(docStore: DocumentZoningApi, pageId: Int@@PageID, labels: Seq[Label]): Boolean = {

    for (targetRegionId <- docStore.getTargetRegions(pageId = pageId)) {
      if (labels.contains(getLabelForTargetRegion(docStore = docStore, targetRegionId = targetRegionId, labels = labels))) {
        return true
      }
    }

    false
  }

  def getPagesWithMetadata(docId: Int@@DocumentID, labels: Seq[Label]): Seq[Int@@PageID] = {

    val pagesWithMetadata: ListBuffer[Int@@PageID] = new ListBuffer[Int@@PageID]()

    for (pageId <- docStore.getPages(docId = docId)) {
      breakable {
        for (targetRegionId <- docStore.getTargetRegions(pageId = pageId)) {
          val targetRegionLabel: Label = getLabelForTargetRegion(docStore = docStore, targetRegionId = targetRegionId, labels = labels)
          if (labels.contains(targetRegionLabel)) {
            pagesWithMetadata += pageId
            break
          }
        }
      }
    }

    pagesWithMetadata

  }

  def getAuthorLabelsForReflow(authorReflow: TextReflow): ListBuffer[(String, String, LTBounds)] = {

    val names: ListBuffer[(String, String, LTBounds)] = new ListBuffer[(String, String, LTBounds)]()

    val tokenizedNames = tokenizeTextReflow(authorReflow)
    if (tokenizedNames.nonEmpty) {
      val separateAuthorNamesByText = getSeparateAuthorNamesByText(tokenizedNames)
      if (separateAuthorNamesByText.nonEmpty) {
        val separateAuthorNamesByGeometry = getSeparateComponentsByGeometry(separateAuthorNamesByText, authorReflow)
        val separateAuthorNameComponents = separateAuthorNamesByGeometry.map {
          authorName => {
            getSeparateAuthorNameComponents(authorName)
          }
        }
        var nameIndex = 0
        while (nameIndex < separateAuthorNamesByGeometry.length) {
          names.++=(getBoundingBoxesForAuthorNames(separateAuthorNameComponents(nameIndex), separateAuthorNamesByGeometry(nameIndex), textReflow = authorReflow))
          nameIndex += 1
        }
      }
    }

    names
  }

  def getAffiliationLabelsForReflows(affiliationReflows: Seq[TextReflow], authorNames: Seq[String]): ListBuffer[(String, ListBuffer[String], LTBounds)] = {


    val separatedComponentsWithClasses: ListBuffer[(String, ListBuffer[String])] = new ListBuffer[(String, ListBuffer[String])]()

    for (affiliationReflow <- affiliationReflows) {
      val tokenizedReflow = tokenizeTextReflow(affiliationReflow)
      if (tokenizedReflow.nonEmpty) {
        val affiliationsSeparatedByText = getSeparateAffiliationComponentsByText(tokenizedTextReflow = tokenizedReflow)
        if (affiliationsSeparatedByText.nonEmpty) {
          val affiliationsSeparatedByGeometry = getSeparateComponentsByGeometry(componentsSeparatedByText = affiliationsSeparatedByText, textReflow = affiliationReflow).map {
            separateComponent => cleanSeparatedComponent(separateComponent)
          }
          if (affiliationsSeparatedByGeometry.nonEmpty) {
            separatedComponentsWithClasses.++=(getCategoryForSeparateAffiliationComponents(affiliationsSeparatedByGeometry))
          }
        }
      }
    }
    getBoundingBoxesForAffiliations(getUpdatedCategoriesForAffiliationComponents(authorNames, separatedComponentsWithClasses), affiliationReflows)

  }

  def getReflowWithLabelsForPage(documentLimit: Int, targetDocumentStableId: Seq[String], labels: Seq[Label]) = {

    val dataFileName: String = "arxiv-sample.txt"
    val exceptionsFileName: String = "arxiv-exceptions.txt"
    val dataFileWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dataFileName)))
    val exceptionsFileWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(exceptionsFileName)))

    val affiliationReflows: ListBuffer[TextReflow] = new ListBuffer[TextReflow]()

    val names: ListBuffer[String] = new ListBuffer[String]()
    var documentNumber: Int = 0

    for {
      docStableId <- docStore.getDocuments(n = documentLimit)
      if targetDocumentStableId.isEmpty || targetDocumentStableId.contains(docStableId.unwrap)
      docId <- docStore.getDocument(docStableId)
    } {
      documentNumber += 1
      println("Document: " + documentNumber + " : " + docStableId)
      for {
        pageId <- getPagesWithMetadata(docId, labels)
      } {
        dataFileWriter.write("\n" + getBoundingBoxAsString(docStore.getPageGeometry(pageId = pageId)) + "\n")
        println("Page Number: " + pageId.toString)
        for (targetRegionId <- docStore.getTargetRegions(pageId = pageId)) {
          val targetRegionLabel: Label = getLabelForTargetRegion(docStore = docStore, targetRegionId = targetRegionId, labels = labels)
          try {
            val textReflow = docStore.getTextReflowForTargetRegion(regionId = targetRegionId)
            if (textReflow.isDefined) {
              if (targetRegionLabel.toString.equals(LB.Affiliations.toString)) {
                affiliationReflows += textReflow.get
              }
              else {
                if (affiliationReflows.nonEmpty) {
                  val affiliationsWithLabels = getAffiliationLabelsForReflows(affiliationReflows, names)
                  affiliationsWithLabels.foreach {
                    affiliationWithLabel => {
                      //                                        print(affiliationWithLabel._1 + " " + getBoundingBoxAsString(affiliationWithLabel._3) + " * I-" + affiliationWithLabel._2.head + "\n")
                      dataFileWriter.write(affiliationWithLabel._1 + " " + getBoundingBoxAsString(affiliationWithLabel._3) + " * I-" + affiliationWithLabel._2.head + "\n")
                    }
                  }
                  affiliationReflows.clear()

                }
                if (targetRegionLabel.toString.equals(LB.Authors.toString)) {
                  val authorNamesWithLabels = getAuthorLabelsForReflow(textReflow.get)
                  authorNamesWithLabels.foreach {
                    authorNameWithLabels => {
                      //                                        print(authorNameWithLabels._1 + " " + getBoundingBoxAsString(bBox = authorNameWithLabels._3) + " * I-" + authorNameWithLabels._2 + "\n")
                      dataFileWriter.write(authorNameWithLabels._1 + " " + getBoundingBoxAsString(bBox = authorNameWithLabels._3) + " * I-" + authorNameWithLabels._2 + "\n")
                      names += authorNameWithLabels._1
                    }
                  }

                }
                else {
                  val componentsWithBoundingBoxes = getBoundingBoxesForComponents(components = cleanPunctuations(tokenizeTextReflow(textReflow.get)), textReflow = textReflow.get)
                  var regionLabel = "O"
                  if (targetRegionLabel.equals(LB.Title)) {
                    regionLabel = "I-TITLE"
                  }
                  else if (targetRegionLabel.equals(LB.Abstract)) {
                    regionLabel = "I-ABSTRACT"
                  }
                  componentsWithBoundingBoxes.foreach {
                    componentWithBoundingBox => {
                      //                                        print(componentWithBoundingBox._1 + " " + getBoundingBoxAsString(componentWithBoundingBox._2) + " * " + regionLabel + "\n")
                      dataFileWriter.write(componentWithBoundingBox._1 + " " + getBoundingBoxAsString(componentWithBoundingBox._2) + " * " + regionLabel + "\n")
                    }
                  }
                }
              }
            }
          }
          catch {
            case exception: Exception =>
              exceptionsFileWriter.write("\n\n---------------------------------------------------" + docStableId + "\n")
              for (stackTraceElement <- exception.getStackTrace) {
                exceptionsFileWriter.write(stackTraceElement.toString)
              }
          }

        }
      }
      dataFileWriter.write("\n")
      names.clear()
    }
    dataFileWriter.close()
  }

}


import ammonite.{ops => fs}, fs._
object RunTransformerUpdate extends App with utils.AppMainBasics {
  val argMap = argsToMap(args)
  println(argMap)

  val dbname = argMap.get("db").flatMap(_.headOption)
    .getOrElse(sys.error("no db supplied (--db ...)"))

  val stableId = "10.1101-090498.d"
  val entry =  argMap.get("entry").flatMap(_.headOption)
    .getOrElse(stableId)


  lazy val corpus = Corpus(pwd / "corpus-test")
  val textReflowDBTables = new CorpusAccessDBTables
  val db = new CorpusAccessDB(dbname, "watrworker",  "watrpasswd")

  val transformer: TransformToCoNLLFormatUpdate = new TransformToCoNLLFormatUpdate(
    CorpusAccessApi(db, corpus)
  )

  val filterToDocs: Seq[String] = Seq()

  transformer.getReflowWithLabelsForPage(2, filterToDocs, Seq(LB.Title, LB.Authors, LB.Affiliations, LB.Abstract))

}
