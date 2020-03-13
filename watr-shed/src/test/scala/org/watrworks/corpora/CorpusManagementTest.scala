package org.watrworks
package corpora

import org.scalatest._

trait CorpusManagementTest extends AnyFlatSpec with Matchers with CorpusTestingUtil {

  // given the root of a corpus (filesys), scan for artifacts (pdfs, extracted text, images, etc).
  // Ensure that particular artifacts exist (e.g., run image extraction where needed)
  // Manage generated artifacts: db backup/snapshot storage, exported annotations
  // Add new PDFs, directly through filesys, or via spidering/bulk-download/user-upload

  // Given 2 (or more) corpus roots, find
}
