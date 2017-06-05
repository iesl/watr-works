package edu.umass.cs.iesl.watr.examples

import edu.umass.cs.iesl.watr.corpora.DocumentCorpus
import edu.umass.cs.iesl.watr.docstore.{TextReflowDB, TextReflowDBTables}
import edu.umass.cs.iesl.watr.textreflow.data._
import edu.umass.cs.iesl.watr.watrmarks.{StandardLabels => LB, Label}


class SampleDbCorpus {

    val textReflowDBTables = new TextReflowDBTables

    val textReflowDB = new TextReflowDB(tables = textReflowDBTables, dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
    val docStore: DocumentCorpus = textReflowDB.docStore

    def printVisualLineForLabel(targetDocumentId: Int, targetLabel: Label) = {
        for {
            docStableId <- docStore.getDocuments(n = 1)
            docId <- docStore.getDocument(stableId = docStableId)
            labelId <- docStore.getZoneLabelsForDocument(docId = docId)
        }{
            if(docId.==(targetDocumentId)) {
                val label = docStore.getLabel(labelId = labelId)
                if (label == targetLabel) {
                    println(targetLabel.key)
                    val zoneIds = docStore.getZonesForDocument(docId = docId, label = labelId)
                    for (zoneId <- zoneIds) {
                        val zone = docStore.getZone(zoneId = zoneId)
                        val targetRegions = zone.regions

                        for (targetRegion <- targetRegions) {
                            println(docStore.getTextReflowForTargetRegion(regionId = targetRegion.id))
                        }
                    }
                }
            }
        }
    }
}

object TextReflowDBExamples extends App{

    val dbCorpus = new SampleDbCorpus()
//    dbCorpus.printVisualLineForLabel(LB.Title)
    dbCorpus.printVisualLineForLabel(3, LB.Authors)

}
