package edu.umass.cs.iesl.watr
package heuristics

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}

import corpora._
import corpora.database.CorpusAccessDB
import corpora.database.CorpusAccessDBTables
import edu.umass.cs.iesl.watr.geometry.LTBounds
import watrmarks.{Label, StandardLabels => LB}
import GenericHeuristics._
import AuthorNameHeuristics._
import AffiliationsHeuristics._
import Utils._
import textreflow.data._

import scala.collection.mutable.ListBuffer

class TransformToCoNLLFormat {

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
        val textReflowDBTables = new CorpusAccessDBTables

        // val textReflowDB = new CorpusAccessDB(dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
      val textReflowDB = new CorpusAccessDB(dbname = "ieslarxiv", dbuser = "watrworker", dbpass = "watrpasswd")
        val docStore: DocumentZoningApi = textReflowDB.docStore

        // val dataFileName: String = "/Users/BatComp/Desktop/UMass/IESL/Code/watr-works/arxiv-sample.txt"
        val dataFileName: String = "arxiv-sample_1.txt"
        val exceptionsFileName: String = "arxiv-exceptions_2.txt"
        val dataFileWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dataFileName)))
        val exceptionsFileWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(exceptionsFileName)))

        var previousDocument: Int = -1

        for {
            docStableId <- docStore.getDocuments(n = documentLimit) if targetDocumentStableId.isEmpty || targetDocumentStableId.contains(docStableId.asInstanceOf[String])
            docId <- docStore.getDocument(docStableId)
        } {
            val affiliationReflows: ListBuffer[TextReflow] = new ListBuffer[TextReflow]()
            val names: ListBuffer[String] = new ListBuffer[String]()

            println("Document: " + docId + " : " + docStableId)

            exceptionsFileWriter.write("\n\n---------------------------------------------------\n" + docStableId + "\n")

            for {
                pageId <- getPagesWithMetadata(docStore = docStore, docId = docId, labels = labels)
            } {

                try {
                    if (!docId.unwrap.equals(previousDocument)) {
                        dataFileWriter.write("\n" + getBoundingBoxAsString(docStore.getPageGeometry(pageId = pageId)) + "\n")
                        previousDocument = docId.unwrap
                    }
                    println("Page Number: " + pageId.toString)

                    for (targetRegionId <- docStore.getTargetRegions(pageId = pageId)) {
                        val targetRegionLabel: Label = getLabelForTargetRegion(docStore = docStore, targetRegionId = targetRegionId, labels = labels)

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
                                            dataFileWriter.write(affiliationWithLabel._1 + " " + pageId.toString + ":" + getBoundingBoxAsString(affiliationWithLabel._3) + " * I-" + affiliationWithLabel._2.head + "\n")
                                        }
                                    }
                                    affiliationReflows.clear()
                                }

                                if (targetRegionLabel.toString.equals(LB.Authors.toString)) {
                                    val authorNamesWithLabels = getAuthorLabelsForReflow(textReflow.get)
                                    authorNamesWithLabels.foreach {
                                        authorNameWithLabels => {
                                            dataFileWriter.write(authorNameWithLabels._1 + " " + pageId.toString + ":" + getBoundingBoxAsString(bBox = authorNameWithLabels._3) + " * I-" + authorNameWithLabels._2 + "\n")
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
                                            dataFileWriter.write(componentWithBoundingBox._1 + " " + pageId.toString + ":" + getBoundingBoxAsString(componentWithBoundingBox._2) + " * " + regionLabel + "\n")
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                catch {
                    case exception: Exception =>
                        exceptionsFileWriter.write(exception.getMessage + "\n")
                        for (stackTraceElement <- exception.getStackTrace) {
                            exceptionsFileWriter.write(stackTraceElement.toString + "\n")
                        }
                }

            }
            dataFileWriter.write("\n")
        }
        dataFileWriter.close()
        exceptionsFileWriter.close()
    }


}

object RunTransformer extends App {
    val documents: Seq[String] = Seq()

    val transformer: TransformToCoNLLFormat = new TransformToCoNLLFormat()
    transformer.getReflowWithLabelsForPage(10000, documents, Seq(LB.Title, LB.Authors, LB.Affiliations, LB.Abstract))
}
