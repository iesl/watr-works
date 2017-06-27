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

import scala.collection.mutable.ListBuffer


class SampleDbCorpus {


    def authorNameSegmentation(targetDocumentStableId: String, targetLabel: Label) = {
        val textReflowDBTables = new CorpusAccessDBTables

        val textReflowDB = new CorpusAccessDB(tables = textReflowDBTables, dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
        val docStore: DocumentZoningApi = textReflowDB.docStore

        val outputFileName: String = "/Users/BatComp/Desktop/UMass/IESL/Code/watr-works/author_segmentation.txt"
        val outputFileWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFileName)))
        for {
            docStableId <- docStore.getDocuments(n = 1) if targetDocumentStableId.equals("") || docStableId.asInstanceOf[String].equals(targetDocumentStableId)
            docId <- docStore.getDocument(stableId = docStableId)
            labelId <- docStore.getZoneLabelsForDocument(docId = docId) if docStore.getLabel(labelId = labelId) == targetLabel
            zoneId <- docStore.getZonesForDocument(docId = docId, label = labelId)
            targetRegion <- docStore.getZone(zoneId = zoneId).regions
        } {
            println("Document: \t\t\t\t\t" + docStableId)
            outputFileWriter.write("Document: \t\t\t\t\t" + docStableId + "\n")
            val targetRegionTextReflow = docStore.getTextReflowForTargetRegion(regionId = targetRegion.id)
            if (targetRegionTextReflow.isDefined) {
                outputFileWriter.write("Text Reflow: \t\t\t\t" + targetRegionTextReflow.get.toText() + "\n")
                val tokenizedNames = tokenizeTextReflow(targetRegionTextReflow.get)
                if (tokenizedNames.nonEmpty) {
                    outputFileWriter.write("Tokenized: \t\t\t\t\t" + tokenizedNames + "\n")
                    val separateAuthorNamesByText = getSeparateAuthorNamesByText(tokenizedNames)
                    if (separateAuthorNamesByText.nonEmpty) {
                        outputFileWriter.write("Text Separated: \t\t\t" + separateAuthorNamesByText + "\n")
                        val separateAuthorNamesByGeometry = getSeparateComponentsByGeometry(separateAuthorNamesByText, targetRegionTextReflow.get)
                        outputFileWriter.write("Geometrically Separated: \t" + separateAuthorNamesByGeometry + "\n")
                        val separateAuthorNameComponents = separateAuthorNamesByGeometry.map {
                            authorName => {
                                getSeparateAuthorNameComponents(authorName)
                            }
                        }
                        var nameIndex = 0
                        while (nameIndex < separateAuthorNamesByGeometry.length) {
                            outputFileWriter.write("Bounding Box info: \t\t\t" + getBoundingBoxesForComponents(separateAuthorNameComponents(nameIndex), separateAuthorNamesByGeometry(nameIndex), textReflow = targetRegionTextReflow.get).toString + "\n")
                            nameIndex += 1
                        }
                    }
                }
            }
            outputFileWriter.write("------------------------------------------------------------------------------------------------\n")
        }
        outputFileWriter.close()
    }

    def exampleFunction1(targetDocumentStableId: String, targetLabel: Label) = {
        val textReflowDBTables = new CorpusAccessDBTables

        val textReflowDB = new CorpusAccessDB(tables = textReflowDBTables, dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
        val docStore: DocumentZoningApi = textReflowDB.docStore

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
        val textReflowDBTables = new CorpusAccessDBTables

        val textReflowDB = new CorpusAccessDB(tables = textReflowDBTables, dbname = "watr_works_test_1", dbuser = "watrworker", dbpass = "watrpasswd")
        val docStore: DocumentZoningApi = textReflowDB.docStore

        for {
            docStableId <- docStore.getDocuments(n = 1) if targetDocumentStableIds.isEmpty || targetDocumentStableIds.contains(docStableId.asInstanceOf[String])
        } {
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
        val textReflowDBTables = new CorpusAccessDBTables

        val textReflowDB = new CorpusAccessDB(tables = textReflowDBTables, dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
        val docStore: DocumentZoningApi = textReflowDB.docStore

        for {
            docStableId <- docStore.getDocuments(n = 1)
        } {

        }
    }

    def exampleFunction3() = {

        print(cleanSeparatedComponent("‡Instituto de mateḿaticas y f́ısica fundamental"))

    }
}

object TextReflowDBExamples extends App {

    val dbCorpus = new SampleDbCorpus()
    //    dbCorpus.authorNameSegmentation("", LB.Authors)
    //    dbCorpus.exampleFunction1("0101047.pdf.d", LB.Authors)
    dbCorpus.exampleFunction2(Seq("0101029.pdf.d"), LB.Affiliation)
    //"0101047.pdf.d", "commai.pdf.d", "bengio03a.pdf.d"
    //    dbCorpus.exampleFunction3()
}
