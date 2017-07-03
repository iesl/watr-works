package edu.umass.cs.iesl.watr
package examples

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}

import corpora._
import corpora.database.CorpusAccessDB
import corpora.database.CorpusAccessDBTables
import watrmarks.{Label, StandardLabels => LB}
import heuristics.GenericHeuristics._
import heuristics.AuthorNameHeuristics._
import heuristics.AffiliationsHeuristics._
import heuristics.Utils._
import textreflow.data._
import corpora.filesys._
import ammonite.{ops => fs}, fs._

import scala.collection.mutable.ListBuffer

class SampleDbCorpus(dbName: String) {
  val textReflowDBTables = new CorpusAccessDBTables

  lazy val textReflowDB = new CorpusAccessDB(tables = textReflowDBTables, dbname = dbName, dbuser = "watrworker", dbpass = "watrpasswd")
  lazy val docStore: DocumentZoningApi = textReflowDB.docStore
  lazy val corpus = Corpus(pwd / "corpus-test")


  def authorNameSegmentation(targetDocumentStableId: String, targetLabel: Label) = {

    val outputFileName: String = "/Users/BatComp/Desktop/UMass/IESL/Code/watr-works/author_segmentation.txt"
    // lazy val outputFileWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFileName)))
    def write(s: String): Unit = {
      // outputFileWriter.write(s)
      println(s)
    }

    for {
      docStableId <- docStore.getDocuments(n = 1) if targetDocumentStableId.equals("") || docStableId.asInstanceOf[String].equals(targetDocumentStableId)
      docId <- docStore.getDocument(stableId = docStableId)
      labelId <- docStore.getZoneLabelsForDocument(docId = docId) if docStore.getLabel(labelId = labelId) == targetLabel
      zoneId <- docStore.getZonesForDocument(docId = docId, label = labelId)
      targetRegion <- docStore.getZone(zoneId = zoneId).regions
    } {
      println("Document: \t\t\t\t\t" + docStableId)
      write("Document: \t\t\t\t\t" + docStableId + "\n")
      val targetRegionTextReflow = docStore.getTextReflowForTargetRegion(regionId = targetRegion.id)
      if (targetRegionTextReflow.isDefined) {
        write("Text Reflow: \t\t\t\t" + targetRegionTextReflow.get.toText() + "\n")
        val tokenizedNames = tokenizeTextReflow(targetRegionTextReflow.get)
        if (tokenizedNames.nonEmpty) {
          write("Tokenized: \t\t\t\t\t" + tokenizedNames + "\n")
          val separateAuthorNamesByText = getSeparateAuthorNamesByText(tokenizedNames)
          if (separateAuthorNamesByText.nonEmpty) {
            write("Text Separated: \t\t\t" + separateAuthorNamesByText + "\n")
            val separateAuthorNamesByGeometry = getSeparateComponentsByGeometry(separateAuthorNamesByText, targetRegionTextReflow.get)
            write("Geometrically Separated: \t" + separateAuthorNamesByGeometry + "\n")
            val separateAuthorNameComponents = separateAuthorNamesByGeometry.map {
              authorName => {
                getSeparateAuthorNameComponents(authorName)
              }
            }
            var nameIndex = 0
            while (nameIndex < separateAuthorNamesByGeometry.length) {
              write("Bounding Box info: \t\t\t" + getBoundingBoxesForComponents(separateAuthorNameComponents(nameIndex), separateAuthorNamesByGeometry(nameIndex), textReflow = targetRegionTextReflow.get).toString + "\n")
              nameIndex += 1
            }
          }
        }
      }
      write("------------------------------------------------------------------------------------------------\n")
    }
    // outputFileWriter.close()
  }

  def exampleFunction1(targetDocumentStableId: String, targetLabel: Label) = {

    for {
      docStableId <- docStore.getDocuments(n = 1) if (targetDocumentStableId.equals("") || docStableId.asInstanceOf[String].equals(targetDocumentStableId)) && !docStableId.asInstanceOf[String].equals("1609.07772.pdf.d")
      docId <- docStore.getDocument(stableId = docStableId)
      zone <- docStore.getDocumentZones(docId = docId, label = targetLabel)
      targetRegion <- zone.regions
    } {
      val targetRegionTextReflow = docStore.getTextReflowForTargetRegion(regionId = targetRegion.id)
      if (targetRegionTextReflow.isDefined) {
        print("Document: \t\t\t\t\t" + docStableId + "\n")
        print("Text Reflow: \t\t\t\t" + targetRegionTextReflow.get.toText() + "\n")
        val tokenizedNames = tokenizeTextReflow(targetRegionTextReflow.get)
        if (tokenizedNames.nonEmpty) {
          print("Tokenized: \t\t\t\t\t" + tokenizedNames.mkString("||") + "\n")
          val separateAuthorNamesByText = getSeparateAuthorNamesByText(tokenizedNames)
          if (separateAuthorNamesByText.nonEmpty) {
            print("Text Separated: \t\t\t" + separateAuthorNamesByText.mkString("||") + "\n")
            val separateAuthorNamesByGeometry = getSeparateComponentsByGeometry(separateAuthorNamesByText, targetRegionTextReflow.get)
            print("Geometrically Separated: \t" + separateAuthorNamesByGeometry.mkString("||") + "\n")
            //                        val separateAuthorNameComponents = separateAuthorNamesByGeometry.map{
            //                            authorName => {
            //                                getSeparateAuthorNameComponents(authorName)
            //                            }
            //                        }
            //                        var nameIndex = 0
            //                        while(nameIndex < separateAuthorNamesByGeometry.length){
            //                            print("Bounding Box info: \t\t\t" + getBoundingBoxesForComponents(separateAuthorNameComponents(nameIndex), separateAuthorNamesByGeometry(nameIndex), textReflow = targetRegionTextReflow.get).toString + "\n")
            //                            nameIndex += 1
            //                        }
          }
        }
      }
      print("------------------------------------------------------------------------------------------------\n")
    }
  }

  def exampleFunction2(targetDocumentStableIds: Seq[String], targetLabel: Label) = {

    for {
      docStableId <- docStore.getDocuments(n = 1) if targetDocumentStableIds.isEmpty || targetDocumentStableIds.contains(docStableId.asInstanceOf[String])
    } {
      println(s"docStableId=${docStableId}")
      for (docId <- docStore.getDocument(stableId = docStableId)) {

        val separatedComponentsWithClasses: ListBuffer[(String, ListBuffer[String])] = new ListBuffer[(String, ListBuffer[String])]()

        for (zone <- docStore.getDocumentZones(docId = docId, label = targetLabel)) {

          for (targetRegion <- zone.regions) {
            println("Document: \t\t\t\t\t" + docStableId)
            val targetRegionTextReflow = docStore.getTextReflowForTargetRegion(regionId = targetRegion.id)
            if (targetRegionTextReflow.isDefined) {
              print("Text Reflow: \t\t\t\t" + targetRegionTextReflow.get.toText() + "\n")
              val tokenizedReflow = tokenizeTextReflow(targetRegionTextReflow.get)
              if (tokenizedReflow.nonEmpty) {
                print("Tokenized: \t\t\t\t\t" + tokenizedReflow.mkString("||") + "\n")
                val affiliationsSeparatedByText = getSeparateAffiliationComponentsByText(tokenizedTextReflow = tokenizedReflow)
                if (affiliationsSeparatedByText.nonEmpty) {
                  print("Text Separated: \t\t\t" + affiliationsSeparatedByText.mkString("||") + "\n")
                  val affiliationsSeparatedByGeometry = getSeparateComponentsByGeometry(componentsSeparatedByText = affiliationsSeparatedByText, textReflow = targetRegionTextReflow.get).map {
                    separateComponent => cleanSeparatedComponent(separateComponent)
                  }
                  if (affiliationsSeparatedByGeometry.nonEmpty) {

                    print("Geometrically Separated: \t" + affiliationsSeparatedByGeometry.mkString("||") + "\n")
                    println("Keywords matched for affiliations: ")
                    separatedComponentsWithClasses.++=(getCategoryForSeparateAffiliationComponents(affiliationsSeparatedByGeometry))
                    //                                        println(getCategoryForSeparateAffiliationComponents(affiliationsSeparatedByGeometry))
                  }
                }
              }
            }
            print("------------------------------------------------------------------------------------------------\n")
          }
        }

        var separatedComponentIndex: Int = 0
        while (separatedComponentIndex < separatedComponentsWithClasses.length) {
          if (separatedComponentsWithClasses(separatedComponentIndex)._2.isEmpty) {
            separatedComponentsWithClasses.update(n = separatedComponentIndex, (separatedComponentsWithClasses(separatedComponentIndex)._1, getMatchedZipCodePatterns(separatedComponentsWithClasses(separatedComponentIndex)._1)))
          }
          separatedComponentIndex += 1
        }
        println(separatedComponentsWithClasses)

      }
    }
  }

  def removeDocuments() = {

    for {
      docStableId <- docStore.getDocuments(n = 1)
    } {

    }
  }

  def exampleFunction3() = {

    print(cleanSeparatedComponent("‡Instituto de mateḿaticas y f́ısica fundamental"))

  }
}

object TextReflowDBExamples extends App with utils.AppMainBasics {

  val argMap = argsToMap(args)
  println(argMap)

  val dbname = argMap.get("db").flatMap(_.headOption)
    .getOrElse("watr_works_db")

  val stableId = "10.1101-090498.d"
  val entry =  argMap.get("entry").flatMap(_.headOption)
    .getOrElse(stableId)

  val dbCorpus = new SampleDbCorpus(dbname)
  // dbCorpus.authorNameSegmentationVariation(entry, LB.Authors)
  //    dbCorpus.exampleFunction1("0101047.pdf.d", LB.Authors)
  // dbCorpus.exampleFunction2(Seq("0101029.pdf.d"), LB.Affiliation)

  dbCorpus.exampleFunction2(Seq(entry), LB.Affiliations)

  //"0101047.pdf.d", "commai.pdf.d", "bengio03a.pdf.d"
  //    dbCorpus.exampleFunction3()
}
