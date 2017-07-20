package edu.umass.cs.iesl.watr
package examples

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}

import TypeTags._
import corpora._
import corpora.database.CorpusAccessDB
import corpora.database.CorpusAccessDBTables
import edu.umass.cs.iesl.watr.heuristics.NameWithBBox
import watrmarks.{Label, StandardLabels => LB}
import heuristics.GenericHeuristics._
import heuristics.AuthorNameHeuristics._
import heuristics.AffiliationsHeuristics._
import heuristics.Utils._
import textreflow.data._
import heuristics.Constants._
//import textreflow.TextReflowF.TextReflow

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
                            outputFileWriter.write("Bounding Box info: \t\t\t" + getBoundingBoxesForAuthorNames(separateAuthorNameComponents(nameIndex), separateAuthorNamesByGeometry(nameIndex), textReflow = targetRegionTextReflow.get).toString + "\n")
                            nameIndex += 1
                        }
                    }
                }
            }
            outputFileWriter.write("------------------------------------------------------------------------------------------------\n")
        }
        outputFileWriter.close()
    }

    def exampleFunction1(documentLimit: Int, targetDocumentStableId: Seq[String], targetLabel: Label): ListBuffer[NameWithBBox] = {
        val textReflowDBTables = new CorpusAccessDBTables

        val textReflowDB = new CorpusAccessDB(tables = textReflowDBTables, dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
        val docStore: DocumentZoningApi = textReflowDB.docStore

        val names: ListBuffer[NameWithBBox] = new ListBuffer[NameWithBBox]()

        for {
            docStableId <- docStore.getDocuments(n = documentLimit) if targetDocumentStableId.isEmpty || targetDocumentStableId.contains(docStableId.asInstanceOf[String])
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
                        val separateAuthorNameComponents = separateAuthorNamesByGeometry.map {
                            authorName => {
                                getSeparateAuthorNameComponents(authorName)
                            }
                        }
                        var nameIndex = 0
                        while (nameIndex < separateAuthorNamesByGeometry.length) {
                            names += getBoundingBoxesForAuthorNames(separateAuthorNameComponents(nameIndex), separateAuthorNamesByGeometry(nameIndex), textReflow = targetRegionTextReflow.get)
                            nameIndex += 1
                        }
                    }
                }
            }


            print("------------------------------------------------------------------------------------------------\n")
        }
//        println(names)
        names
    }

    def exampleFunction2(documentLimit: Int, targetDocumentStableIds: Seq[String], targetLabel: Label, authorNames: ListBuffer[NameWithBBox]): ListBuffer[(String, ListBuffer[String])] = {
        val textReflowDBTables = new CorpusAccessDBTables

        val textReflowDB = new CorpusAccessDB(tables = textReflowDBTables, dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
        val docStore: DocumentZoningApi = textReflowDB.docStore

        val affiliations: ListBuffer[(String, ListBuffer[String])] = new ListBuffer[(String, ListBuffer[String])]()

        for {
            docStableId <- docStore.getDocuments(n = documentLimit) if targetDocumentStableIds.isEmpty || targetDocumentStableIds.contains(docStableId.asInstanceOf[String])
        } {
            for (docId <- docStore.getDocument(stableId = docStableId)) {

                val separatedComponentsWithClasses: ListBuffer[(String, ListBuffer[String])] = new ListBuffer[(String, ListBuffer[String])]()
                val textReflows: ListBuffer[TextReflow] = new ListBuffer[TextReflow]()

                for (zone <- docStore.getDocumentZones(docId = docId, label = targetLabel)) {
                    for (targetRegion <- zone.regions) {
                        //                        println("Document: \t\t\t\t\t" + docStableId)
                        val targetRegionTextReflow = docStore.getTextReflowForTargetRegion(regionId = targetRegion.id)
                        if (targetRegionTextReflow.isDefined) {
                            //                            print("Text Reflow: \t\t\t\t" + targetRegionTextReflow.get.toText() + "\n")
                            //                            textReflows += targetRegionTextReflow.get
                            val tokenizedReflow = tokenizeTextReflow(targetRegionTextReflow.get)
                            if (tokenizedReflow.nonEmpty) {
                                //                                print("Tokenized: \t\t\t\t\t" + tokenizedReflow.mkString("||") + "\n")
                                val affiliationsSeparatedByText = getSeparateAffiliationComponentsByText(tokenizedTextReflow = tokenizedReflow)
                                if (affiliationsSeparatedByText.nonEmpty) {
                                    //                                    print("Text Separated: \t\t\t" + affiliationsSeparatedByText.mkString("||") + "\n")
                                    val affiliationsSeparatedByGeometry = getSeparateComponentsByGeometry(componentsSeparatedByText = affiliationsSeparatedByText, textReflow = targetRegionTextReflow.get).map {
                                        separateComponent => cleanSeparatedComponent(separateComponent)
                                    }
                                    if (affiliationsSeparatedByGeometry.nonEmpty) {

                                        //                                        print("Geometrically Separated: \t" + affiliationsSeparatedByGeometry.mkString("||") + "\n")
                                        separatedComponentsWithClasses.++=(getCategoryForSeparateAffiliationComponents(affiliationsSeparatedByGeometry))
                                    }
                                }
                            }
                        }
                    }
                }
                //                println("Categories for Separated Affiliations")
                val affiliationsWithCategories = getUpdatedCategoriesForAffiliationComponents(authorNames, separatedComponentsWithClasses)
                //                getBoundingBoxesForAffiliations(affiliationsWithCategories, textReflows)
                //                    .foreach {
                //
                //                    affiliation => {
                //                        println(affiliation._1 + " : " + affiliation._2)
                //                        println(affiliation._3)
                //                    }
                //
                //                }
                affiliations.++=:(affiliationsWithCategories)
                //                print("------------------------------------------------------------------------------------------------\n")
            }
        }
        affiliations
    }


}


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

    def getAffiliationLabelsForReflows(affiliationReflows: Seq[TextReflow], authorNames: ListBuffer[NameWithBBox]): ListBuffer[(String, ListBuffer[String])] = {


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
        getUpdatedCategoriesForAffiliationComponents(authorNames, separatedComponentsWithClasses)

    }

    def getReflowWithLabelsForPage(documentLimit: Int, targetDocumentStableId: Seq[String], pageNum: Int @@ PageNum, labels: Seq[Label]) = {
        val textReflowDBTables = new CorpusAccessDBTables

        val textReflowDB = new CorpusAccessDB(tables = textReflowDBTables, dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
        val docStore: DocumentZoningApi = textReflowDB.docStore

        val dataFileName: String = "/Users/BatComp/Desktop/UMass/IESL/Code/watr-works/random-input.txt"
        val dataFileWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dataFileName)))

//        val tokensFileName: String = "/Users/BatComp/Desktop/UMass/IESL/Code/watr-works/arxiv-name-tokens.txt"
//        val tokensFileWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(tokensFileName)))

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
                            getAffiliationLabelsForReflows(affiliationReflows, names).foreach {
                                affiliationLabel => {
                                    if (affiliationLabel._2.head.equals("EMAIL")) {
                                        cleanPunctuations(affiliationLabel._1.split(SPACE_SEPARATOR)).foreach(affiliationToken => {
                                            dataFileWriter.write(affiliationToken + " * * I-" + affiliationLabel._2.head + "\n")
//                                            tokensFileWriter.write(affiliationToken + " ")
                                        })
                                    }
                                    else {
                                        affiliationLabel._1.split(SPACE_SEPARATOR).foreach(affiliationToken => {
                                            dataFileWriter.write(affiliationToken + " * * I-" + affiliationLabel._2.head + "\n")
//                                            tokensFileWriter.write(affiliationToken + " ")
                                        })
                                    }
                                }
                            }
                            affiliationReflows.clear()
                            names.clear()
                        }
                        if (targetRegionLabel.toString.equals(LB.Authors.toString)) {
                            val authorNameToken: String = "AUTHOR_NAME"
                            val authorNamesWithLabels = getAuthorLabelsForReflow(textReflow.get)
                            authorNamesWithLabels.foreach {
                                authorNameWithLabels => {
                                    if (authorNameWithLabels.firstName.componentText.nonEmpty) {
                                        authorNameWithLabels.firstName.componentText.split(SPACE_SEPARATOR).foreach(name => {
                                            dataFileWriter.write(name + " * * I-" + "FIRST-NAME" + "\n")
//                                            tokensFileWriter.write(authorNameToken + " ")
                                        })
                                    }
                                    if (authorNameWithLabels.middleName.componentText.nonEmpty) {
                                        authorNameWithLabels.middleName.componentText.split(SPACE_SEPARATOR).foreach(name => {
                                            dataFileWriter.write(name + " * * I-" + "MIDDLE-NAME" + "\n")
//                                            tokensFileWriter.write(authorNameToken + " ")
                                        })
                                    }
                                    if (authorNameWithLabels.lastName.componentText.nonEmpty) {
                                        authorNameWithLabels.lastName.componentText.split(SPACE_SEPARATOR).foreach(name => {
                                            dataFileWriter.write(name + " * * I-" + "LAST-NAME" + "\n")
//                                            tokensFileWriter.write(authorNameToken + " ")
                                        })
                                    }
                                    if (authorNameWithLabels.hereditySuffix.componentText.nonEmpty) {
                                        authorNameWithLabels.hereditySuffix.componentText.split(SPACE_SEPARATOR).foreach(name => {
                                            dataFileWriter.write(name + " * * I-" + "HEREDITY-SUFFIX" + "\n")
//                                            tokensFileWriter.write(authorNameToken + " ")
                                        })
                                    }
                                    if (authorNameWithLabels.degree.componentText.nonEmpty) {
                                        authorNameWithLabels.degree.componentText.split(SPACE_SEPARATOR).foreach(name => {
                                            dataFileWriter.write(name + " * * I-" + "DEGREE" + "\n")
//                                            tokensFileWriter.write(authorNameToken + " ")
                                        })
                                    }
                                }
                            }
                            names.++=(authorNamesWithLabels)
                        }
                        else {
                            val textReflowTokens = cleanPunctuations(tokenizeTextReflow(textReflow.get))
                            var regionLabel = "O"
                            if (targetRegionLabel.equals(LB.Title)) {
                                regionLabel = "I-TITLE"
                            }
                            else if (targetRegionLabel.equals(LB.Abstract)) {
                                regionLabel = "I-ABSTRACT"
                            }
                            textReflowTokens.foreach {
                                textReflowToken => {
                                    dataFileWriter.write(textReflowToken.trim + " * * " + regionLabel + "\n")
//                                    tokensFileWriter.write(textReflowToken + " ")
                                }
                            }
                        }
                    }
                }
            }
            dataFileWriter.write("\n\n")
//            tokensFileWriter.write("\n")
        }
        dataFileWriter.close()
//        tokensFileWriter.close()
    }

}

object TextReflowDBExamples extends App {

    val documents: Seq[String] = Seq("name_with_degree.pdf.d", "commai.pdf.d")
    val pageNum = PageNum(0)

    //      "1609.03499.pdf.d", "AISTATS2010_Glorot.pdf.d", "1702.02098.pdf.d", "5021-distributed-representations-of-words-and-phrases-and-their-compositionality.pdf.d"
//    val dbCorpus = new SampleDbCorpus()
//    val names = dbCorpus.exampleFunction1(500, documents, LB.Authors)
//    names.foreach{
//        name => {
//            println("First Name: " + name.firstName.componentText)
//            println("Middle Name: " + name.middleName.componentText)
//            println("Last Name: " + name.lastName.componentText)
//            println("Heredity Suffix: " + name.hereditySuffix.componentText)
//            println("Degree: " + name.degree.componentText)
//            println("--------------------------------")
//        }
//    }

    //      val affiliations = dbCorpus.exampleFunction2(130, documents, LB.Affiliations, names)
    //      println(affiliations)
    //      dbCorpus.exampleFunction3()

    val transformer: TransformToCoNLLFormat = new TransformToCoNLLFormat()
    transformer.getReflowWithLabelsForPage(131, documents, pageNum, Seq(LB.Title, LB.Authors, LB.Affiliations, LB.Abstract))

}
