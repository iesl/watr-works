package org.watrworks
package segment
package prelude


sealed trait AnalysisOption

object AnalysisOption {
  // UseImageAnalysis
  // Dehyphenation
  // Super/subscript finding/output
}

sealed trait OutputOption {
  // PaginatedTextFlow
  // JoinedPageTextFlow
  // Include..
  //    IncludeTracelogs
  //    IncludeFontInfo
  //    ImageAnalysisLabels
  // Super/subscript latex-ish escaping
  //

}
