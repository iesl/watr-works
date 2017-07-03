package edu.umass.cs.iesl.watr
package labeling

import workflow._
import corpora._


trait ServerLabelerBuilder extends LabelerBuilder {
  def corpusAccessApi: CorpusAccessApi

  def docStore: DocumentZoningApi = corpusAccessApi.docStore
  def workflowApi: WorkflowApi = corpusAccessApi.workflowApi
  def userbaseApi: UserbaseApi = corpusAccessApi.userbaseApi
}
