package edu.umass.cs.iesl.watr
package labeling


object LabelingTasks {

  // - List all tasks
  // - List tasks of a particular variety (e.g., "author names")
  // - Retrieve LabelWidgets for a task
  // - Workflow for labeling
  //   - NEW -> TODO -> INPROGRESS -> DONE/SKIPPED/ERROR
  //   - assign labeling task to a participant


  // What is the unit of granularity for a labeling task?
  //   Document, page, target-region, lists of(doc|page|region)

  /*

   - Label authors in document

   DocumentType => LB.Authors
   DocumentType => LB.Abstract
   PageType => LB.Title

   ZoneType[w/LB.Authors] => LB.Author
   ZoneType[w/LB.Author] => LB.Name, LB.NoteMarker
   ZoneType[w/LB.Name] => LB.NameFirst,Middle,Sur,Fore

   */



}
