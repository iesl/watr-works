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
import Constants._
import edu.umass.cs.iesl.watr.TypeTags.PageNum

import scala.collection.mutable.ListBuffer

class TransformToCoNLLFormat {

    def getLabelForTargetRegion(docStore: DocumentZoningApi, targetRegionId: Int @@ RegionID, labels: Seq[Label]): Label = {

        for (label <- labels) {
            if (docStore.getZoneForRegion(regionId = targetRegionId, label = label).isDefined) {
                return label
            }
        }
        LB.NullLabel
    }

    def getAuthorLabelsForReflow(authorReflow: TextReflow): ListBuffer[NameWithBBox] = {

        val names: ListBuffer[NameWithBBox] = new ListBuffer[NameWithBBox]()

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
                    names += getBoundingBoxesForAuthorNames(separateAuthorNameComponents(nameIndex), separateAuthorNamesByGeometry(nameIndex), textReflow = authorReflow)
                    nameIndex += 1
                }
            }
        }

        names
    }

    def getAffiliationLabelsForReflows(affiliationReflows: Seq[TextReflow], authorNames: ListBuffer[NameWithBBox]): ListBuffer[(String, ListBuffer[String], LTBounds)] = {


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

    def getReflowWithLabelsForPage(documentLimit: Int, targetDocumentStableId: Seq[String], pageNum: Int @@ PageNum, labels: Seq[Label]) = {
        val textReflowDBTables = new CorpusAccessDBTables

        val textReflowDB = new CorpusAccessDB(tables = textReflowDBTables, dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
        val docStore: DocumentZoningApi = textReflowDB.docStore

        val dataFileName: String = "/Users/BatComp/Desktop/UMass/IESL/Code/watr-works/arxiv-sample.txt"
        val dataFileWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dataFileName)))

        val affiliationReflows: ListBuffer[TextReflow] = new ListBuffer[TextReflow]()

        val names: ListBuffer[NameWithBBox] = new ListBuffer[NameWithBBox]()

        for {
            docStableId <- docStore.getDocuments(n = documentLimit) if targetDocumentStableId.isEmpty || targetDocumentStableId.contains(docStableId.asInstanceOf[String])
            docId <- docStore.getDocument(docStableId)
        } {
            println(docStableId)
            for {
                pageId <- docStore.getPage(docId = docId, pageNum = pageNum)
                targetRegionId <- docStore.getTargetRegions(pageId = pageId)
            } {
                val textReflow = docStore.getTextReflowForTargetRegion(regionId = targetRegionId)
                if (textReflow.isDefined) {
                    val targetRegionLabel: Label = getLabelForTargetRegion(docStore = docStore, targetRegionId = targetRegionId, labels = labels)
                    if (targetRegionLabel.toString.equals(LB.Affiliations.toString)) {
                        affiliationReflows += textReflow.get
                    }
                    else {
                        if (affiliationReflows.nonEmpty) {
                            val affiliationsWithLabels = getAffiliationLabelsForReflows(affiliationReflows, names)
                            affiliationsWithLabels.foreach {
                                affiliationLabel => {
                                    cleanPunctuations(affiliationLabel._1.split(SPACE_SEPARATOR)).foreach(affiliationToken => {
                                        //                                        println(affiliationToken.concat(UNDERSCORE).concat(getBoundingBoxAsString(affiliationLabel._3)) + " * * I-" + affiliationLabel._2.head + "\n")
                                        dataFileWriter.write(affiliationToken + " "  + getBoundingBoxAsString(affiliationLabel._3) + " * I-" + affiliationLabel._2.head + "\n")
                                    })
                                }
                            }
                            affiliationReflows.clear()
                            names.clear()
                        }
                        if (targetRegionLabel.toString.equals(LB.Authors.toString)) {
                            val authorNamesWithLabels = getAuthorLabelsForReflow(textReflow.get)
                            authorNamesWithLabels.foreach {
                                authorNameWithLabels => {
                                    if (authorNameWithLabels.firstName.componentText.nonEmpty) {
                                        authorNameWithLabels.firstName.componentText.split(SPACE_SEPARATOR).foreach(name => {
                                            //                                            println(name.concat(UNDERSCORE).concat(getBoundingBoxAsString(authorNameWithLabels.firstName.componentBBox)) + " * * I-" + "FIRST-NAME" + "\n")
                                            dataFileWriter.write(name + " " + getBoundingBoxAsString(authorNameWithLabels.firstName.componentBBox) + " * I-" + "FIRST-NAME" + "\n")
                                        })
                                    }
                                    if (authorNameWithLabels.middleName.componentText.nonEmpty) {
                                        authorNameWithLabels.middleName.componentText.split(SPACE_SEPARATOR).foreach(name => {
                                            //                                            println(name.concat(UNDERSCORE).concat(getBoundingBoxAsString(authorNameWithLabels.middleName.componentBBox)) + " * * I-" + "MIDDLE-NAME" + "\n")
                                            dataFileWriter.write(name + " " + getBoundingBoxAsString(authorNameWithLabels.middleName.componentBBox) + " * I-" + "MIDDLE-NAME" + "\n")
                                        })
                                    }
                                    if (authorNameWithLabels.lastName.componentText.nonEmpty) {
                                        authorNameWithLabels.lastName.componentText.split(SPACE_SEPARATOR).foreach(name => {
                                            //                                            println(name.concat(UNDERSCORE).concat(getBoundingBoxAsString(authorNameWithLabels.lastName.componentBBox)) + " * * I-" + "LAST-NAME" + "\n")
                                            dataFileWriter.write(name + " " + getBoundingBoxAsString(authorNameWithLabels.lastName.componentBBox) + " * I-" + "LAST-NAME" + "\n")
                                        })
                                    }
                                    if (authorNameWithLabels.hereditySuffix.componentText.nonEmpty) {
                                        authorNameWithLabels.hereditySuffix.componentText.split(SPACE_SEPARATOR).foreach(name => {
                                            //                                            println(name.concat(UNDERSCORE).concat(getBoundingBoxAsString(authorNameWithLabels.hereditySuffix.componentBBox)) + " * * I-" + "HEREDITY-SUFFIX" + "\n")
                                            dataFileWriter.write(name + " " + getBoundingBoxAsString(authorNameWithLabels.hereditySuffix.componentBBox) + " * I-" + "HEREDITY-SUFFIX" + "\n")
                                        })
                                    }
                                    if (authorNameWithLabels.degree.componentText.nonEmpty) {
                                        authorNameWithLabels.degree.componentText.split(SPACE_SEPARATOR).foreach(name => {
                                            //                                            println(name.concat(UNDERSCORE).concat(getBoundingBoxAsString(authorNameWithLabels.degree.componentBBox)) + " * * I-" + "DEGREE" + "\n")
                                            dataFileWriter.write(name + " " + getBoundingBoxAsString(authorNameWithLabels.degree.componentBBox) + " * I-" + "DEGREE" + "\n")
                                        })
                                    }
                                }
                            }
                            names.++=(authorNamesWithLabels)
                        }
                        else {
                            val components = cleanPunctuations(tokenizeTextReflow(textReflow.get))
                            val boundingBox = getBoundingBoxAsString(getBoundingBoxesWithIndexesFromReflow(indexes = (0, textReflow.get.charAtoms().length), textReflow = textReflow.get))
                            var regionLabel = "O"
                            if (targetRegionLabel.equals(LB.Title)) {
                                regionLabel = "I-TITLE"
                            }
                            else if (targetRegionLabel.equals(LB.Abstract)) {
                                regionLabel = "I-ABSTRACT"
                            }
                            components.foreach {
                                component => {
                                    //                                    println(component.concat(UNDERSCORE).concat(boundingBox) + " * * " + regionLabel + "\n")
                                    dataFileWriter.write(component + " " + boundingBox + " * " + regionLabel + "\n")
                                }
                            }
                        }
                    }
                }
            }
            dataFileWriter.write("\n")
        }
        dataFileWriter.close()
    }

}

object RunTransformer extends App {
    val documents: Seq[String] = Seq("0101001.pdf.d")
    val pageNum = PageNum(0)

    val transformer: TransformToCoNLLFormat = new TransformToCoNLLFormat()
    transformer.getReflowWithLabelsForPage(131, documents, pageNum, Seq(LB.Title, LB.Authors, LB.Affiliations, LB.Abstract))
}