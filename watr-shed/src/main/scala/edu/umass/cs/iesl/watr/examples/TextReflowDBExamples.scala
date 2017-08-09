package edu.umass.cs.iesl.watr
package examples

import java.io.{BufferedWriter, FileOutputStream, OutputStreamWriter}
import scala.io.Source

import TypeTags._
import corpora._
import corpora.database.CorpusAccessDB
import corpora.database.CorpusAccessDBTables
import edu.umass.cs.iesl.watr.geometry.LTBounds
import edu.umass.cs.iesl.watr.heuristics.NameWithBBox
import watrmarks.{Label, StandardLabels => LB}
import heuristics.GenericHeuristics._
import heuristics.AuthorNameHeuristics._
import heuristics.AffiliationsHeuristics._
import heuristics.Utils._
import textreflow.data._
import corpora.filesys._
import ammonite.{ops => fs}, fs._
import heuristics.Constants._
//import textreflow.TextReflowF.TextReflow


import scala.collection.mutable.ListBuffer

class SampleDbCorpus(dbName: String) {
    val textReflowDBTables = new CorpusAccessDBTables

    lazy val textReflowDB = new CorpusAccessDB(dbname = dbName, dbuser = "watrworker", dbpass = "watrpasswd")
    lazy val docStore: DocumentZoningApi = textReflowDB.docStore
    lazy val corpus = Corpus(pwd / "corpus-test")


    def authorNameSegmentation(targetDocumentStableId: String, targetLabel: Label) = {

        val outputFileName: String = "/Users/BatComp/Desktop/UMass/IESL/Code/watr-works/author_segmentation.txt"

        // lazy val outputFileWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outputFileName)))
        def write(s: String): Unit = {
            // outputFileWriter.write(s)
            println(s)
        }

        // val outputFileName: String = "/Users/BatComp/Desktop/UMass/IESL/Code/watr-works/author_segmentation.txt"
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
                        // while (nameIndex < separateAuthorNamesByGeometry.length) {
                        //   val bboxes = getBoundingBoxesForComponents(
                        //     separateAuthorNameComponents(nameIndex),
                        //     separateAuthorNamesByGeometry(nameIndex),
                        //     textReflow = targetRegionTextReflow.get
                        //   )
                        //   write("Bounding Box info: \t\t\t" + bboxes.toString + "\n")
                        //   nameIndex += 1
                        // }
                    }
                    write("------------------------------------------------------------------------------------------------\n")
                }
            }
        }
    }

    // outputFileWriter.close()

    // <<<<<<< HEAD
    //   def exampleFunction1(targetDocumentStableId: String, targetLabel: Label) = {

    //     for {
    //       docStableId <- docStore.getDocuments(n = 1) if (targetDocumentStableId.equals("") || docStableId.asInstanceOf[String].equals(targetDocumentStableId)) && !docStableId.asInstanceOf[String].equals("1609.07772.pdf.d")
    //       docId <- docStore.getDocument(stableId = docStableId)
    //       zone <- docStore.getDocumentZones(docId = docId, label = targetLabel)
    //       targetRegion <- zone.regions
    //     } {
    //       val targetRegionTextReflow = docStore.getTextReflowForTargetRegion(regionId = targetRegion.id)
    //       if (targetRegionTextReflow.isDefined) {
    //         print("Document: \t\t\t\t\t" + docStableId + "\n")
    //         print("Text Reflow: \t\t\t\t" + targetRegionTextReflow.get.toText() + "\n")
    //         val tokenizedNames = tokenizeTextReflow(targetRegionTextReflow.get)
    //         if (tokenizedNames.nonEmpty) {
    //           print("Tokenized: \t\t\t\t\t" + tokenizedNames.mkString("||") + "\n")
    //           val separateAuthorNamesByText = getSeparateAuthorNamesByText(tokenizedNames)
    //           if (separateAuthorNamesByText.nonEmpty) {
    //             print("Text Separated: \t\t\t" + separateAuthorNamesByText.mkString("||") + "\n")
    //             val separateAuthorNamesByGeometry = getSeparateComponentsByGeometry(separateAuthorNamesByText, targetRegionTextReflow.get)
    //             print("Geometrically Separated: \t" + separateAuthorNamesByGeometry.mkString("||") + "\n")
    //             //                        val separateAuthorNameComponents = separateAuthorNamesByGeometry.map{
    //             //                            authorName => {
    //             //                                getSeparateAuthorNameComponents(authorName)
    //             //                            }
    //             //                        }
    //             //                        var nameIndex = 0
    //             //                        while(nameIndex < separateAuthorNamesByGeometry.length){
    //             //                            print("Bounding Box info: \t\t\t" + getBoundingBoxesForComponents(separateAuthorNameComponents(nameIndex), separateAuthorNamesByGeometry(nameIndex), textReflow = targetRegionTextReflow.get).toString + "\n")
    //             //                            nameIndex += 1
    //             //                        }
    //           }
    //         }
    //       }
    //       print("------------------------------------------------------------------------------------------------\n")
    // =======

    def exampleFunction1(documentLimit: Int, targetDocumentStableId: Seq[String], targetLabel: Label): ListBuffer[(String, String, LTBounds)] = {
        val textReflowDBTables = new CorpusAccessDBTables

        val textReflowDB = new CorpusAccessDB(dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
        val docStore: DocumentZoningApi = textReflowDB.docStore

        val names: ListBuffer[(String, String, LTBounds)] = new ListBuffer[(String, String, LTBounds)]()

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
                            names.++=(getBoundingBoxesForAuthorNames(separateAuthorNameComponents(nameIndex), separateAuthorNamesByGeometry(nameIndex), textReflow = targetRegionTextReflow.get))
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

    def exampleFunction2(documentLimit: Int, targetDocumentStableIds: Seq[String], targetLabel: Label, authorNames: Seq[String]): ListBuffer[(String, ListBuffer[String])] = {
        val textReflowDBTables = new CorpusAccessDBTables

        val textReflowDB = new CorpusAccessDB(dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
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
                        println("Document: \t\t\t\t\t" + docStableId)
                        val targetRegionTextReflow = docStore.getTextReflowForTargetRegion(regionId = targetRegion.id)
                        if (targetRegionTextReflow.isDefined) {
                            print("Text Reflow: \t\t\t\t" + targetRegionTextReflow.get.toText() + "\n")
                            textReflows += targetRegionTextReflow.get
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
                                        separatedComponentsWithClasses.++=(getCategoryForSeparateAffiliationComponents(affiliationsSeparatedByGeometry))
                                    }
                                }
                            }
                        }
                    }
                }
                //                println("Categories for Separated Affiliations")
                val affiliationsWithCategories = getUpdatedCategoriesForAffiliationComponents(authorNames, separatedComponentsWithClasses)
                val boundingBoxes = getBoundingBoxesForAffiliations(affiliationsWithCategories, textReflows)
                boundingBoxes.foreach {

                    affiliation => {
                        println(affiliation._1 + " : " + affiliation._2)
                        println(affiliation._3)
                    }

                }
                affiliations.++=:(affiliationsWithCategories)
                print("------------------------------------------------------------------------------------------------\n")
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

    def getAffiliationLabelsForReflows(affiliationReflows: Seq[TextReflow], authorNames: ListBuffer[String]): ListBuffer[(String, ListBuffer[String], LTBounds)] = {

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

        val textReflowDB = new CorpusAccessDB(dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
        val docStore: DocumentZoningApi = textReflowDB.docStore

        val dataFileName: String = "/Users/BatComp/Desktop/UMass/IESL/Code/watr-works/arxiv-sample.txt"
        val dataFileWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dataFileName)))

        val affiliationReflows: ListBuffer[TextReflow] = new ListBuffer[TextReflow]()

        val names: ListBuffer[String] = new ListBuffer[String]()

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
                                        dataFileWriter.write(affiliationToken + " " + getBoundingBoxAsString(affiliationLabel._3) + " * I-" + affiliationLabel._2.head + "\n")
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
                                    dataFileWriter.write(authorNameWithLabels._1 + " " + getBoundingBoxAsString(bBox = authorNameWithLabels._3) + " * I-" + authorNameWithLabels._2)
                                    names += authorNameWithLabels._1
                                }
                            }

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

object TextReflowDBExamples extends App with utils.AppMainBasics {

//    val argMap = argsToMap(args)
//    println(argMap)
//
//    val dbname = argMap.get("db").flatMap(_.headOption)
//        .getOrElse("watr_works_db")
//
//    val stableId = "10.1101-090498.d"
//    val entry = argMap.get("entry").flatMap(_.headOption)
//        .getOrElse(stableId)
//
//    val dbCorpus = new SampleDbCorpus(dbname)

    // dbCorpus.authorNameSegmentationVariation(entry, LB.Authors)
    // dbCorpus.exampleFunction1("0101047.pdf.d", LB.Authors)
    // dbCorpus.exampleFunction2(Seq("0101029.pdf.d"), LB.Affiliation)
    // dbCorpus.exampleFunction2(Seq(entry), LB.Affiliations)

    //"0101047.pdf.d", "commai.pdf.d", "bengio03a.pdf.d"
    //    dbCorpus.exampleFunction3()
    // object TextReflowDBExamples extends App {

    //     val documents: Seq[String] = Seq("0101001.pdf.d")
    //     val pageNum = PageNum(0)

    //     //    val dbCorpus = new SampleDbCorpus()
    //     //    dbCorpus.exampleFunction3()
    //     //    val names = dbCorpus.exampleFunction1(500, documents, LB.Authors)

    //     //    val affiliations = dbCorpus.exampleFunction2(131, documents, LB.Affiliations, names)
    //     //    println(affiliations)

    //     val transformer: TransformToCoNLLFormat = new TransformToCoNLLFormat()
    //     transformer.getReflowWithLabelsForPage(131, documents, pageNum, Seq(LB.Title, LB.Authors, LB.Affiliations, LB.Abstract))

    println(getSeparateAuthorNameComponents("Aditya Narasimha van den Shastry III MS"))
    val names = Seq("Aditya", "Narasimha", "Shastry")
    getUpdatedCategoriesForAffiliationComponents(names, getCategoryForSeparateAffiliationComponents(Seq("College of Information and Computer Sciences", "University of Massachusetts", "Amherst", "ashastry@cs.umass.edu"))).foreach{
        affiliation => {
            cleanPunctuations(affiliation._1.split(SPACE_SEPARATOR)).foreach{
                cleanedAffiliation => println(cleanedAffiliation + " : " + affiliation._2.head)
            }
        }
    }

}
