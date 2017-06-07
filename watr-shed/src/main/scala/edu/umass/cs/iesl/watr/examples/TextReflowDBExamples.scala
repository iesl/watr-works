package edu.umass.cs.iesl.watr.examples

import edu.umass.cs.iesl.watr.corpora.DocumentCorpus
import edu.umass.cs.iesl.watr.docstore.{TextReflowDB, TextReflowDBTables}
import edu.umass.cs.iesl.watr.watrmarks.{StandardLabels => LB, Label}
import edu.umass.cs.iesl.watr.heuristics.AuthorNameHeuristics._


class SampleDbCorpus {

    val textReflowDBTables = new TextReflowDBTables

    val textReflowDB = new TextReflowDB(tables = textReflowDBTables, dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
    val docStore: DocumentCorpus = textReflowDB.docStore

    def exampleFunction1(targetDocumentId: Int, targetLabel: Label) = {
        for {
            docStableId <- docStore.getDocuments(n = 1)
            docId <- docStore.getDocument(stableId = docStableId) if targetDocumentId > 0 && docId.==(targetDocumentId)
            labelId <- docStore.getZoneLabelsForDocument(docId = docId) if docStore.getLabel(labelId = labelId) == targetLabel
        } {
            val tokenizedText = docStore.getZonesForDocument(docId = docId, label = labelId).map {
                zoneId => {
                    docStore.getZone(zoneId = zoneId).regions.map {
                        targetRegion => tokenizeAuthorNames(docStore.getTextReflowForTargetRegion(regionId = targetRegion.id).get)
                    }
                }
            }.head
            println(tokenizedText)
            tokenizedText.map{
                textToken => println(getSeparateAuthorNamesByText(textToken))
            }
        }
    }
}

object TextReflowDBExamples extends App {

    val dbCorpus = new SampleDbCorpus()
    dbCorpus.exampleFunction1(2, LB.Authors)

}
