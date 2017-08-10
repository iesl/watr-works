package edu.umass.cs.iesl.watr
package statistics

import java.io._

import corpora.DocumentZoningApi
import corpora.database.{CorpusAccessDB, CorpusAccessDBTables}
import heuristics.Utils._
import watrmarks.{Label, StandardLabels => LB}
import TypeTags._


class ArxivStats {

    def pageStats(docStore: DocumentZoningApi, targetDocuments: Seq[String], metadataLabels: Seq[Label] ): Unit = {

        val pageStatsFileName: String = "arxiv_page_stats.txt"
//        val pageStatsFileWriter = new PrintWriter(new File(pageStatsFileName))
        val pageStatsFileWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(pageStatsFileName)))

        for {
            docStableId <- docStore.getDocuments(n = 10000) if targetDocuments.isEmpty || targetDocuments.contains(docStableId.asInstanceOf[String])
            docId <- docStore.getDocument(docStableId)
        } {
            println(docId)
            var pageNumbers = getPagesWithMetadata(docStore = docStore, docId = docId, labels = metadataLabels)
            pageNumbers = pageNumbers.map(pageNumber => PageID(pageNumber.unwrap - pageNumbers.head.unwrap + 1))
            pageStatsFileWriter.write(docId + ":" + docStableId + "\t" + pageNumbers.mkString("\t") + "\n")
        }

        pageStatsFileWriter.close()
    }


}


object RunArxivStats extends App{

    val textReflowDBTables = new CorpusAccessDBTables

    val textReflowDB = new CorpusAccessDB(tables = textReflowDBTables, dbname = "watr_works_db", dbuser = "watrworker", dbpass = "watrpasswd")
    val docStore: DocumentZoningApi = textReflowDB.docStore

    val arxivStats = new ArxivStats()
    arxivStats.pageStats(docStore = docStore, targetDocuments = Seq(), metadataLabels = Seq(LB.Title, LB.Authors, LB.Affiliations, LB.Abstract))

}
