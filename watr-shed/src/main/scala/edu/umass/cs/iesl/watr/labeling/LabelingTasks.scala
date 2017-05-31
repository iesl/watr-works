package edu.umass.cs.iesl.watr
package labeling


object LabelingTasks {

  // - List all tasks
  // - List tasks of a particular variety (e.g., "author names")
  // - Retrieve LabelWidgets for a task
  // - Workflow for labeling
  //   - NEW -> TODO -> INPROGRESS -> DONE/SKIPPED/ERROR
  //   - assign labeling task to a participant



  // Search for docs w/labels like
  //  Find doc with all labels present:  '(authors & title & abstract)'
  //  Find doc with any label present:  '(authors | title | abstract)'
  //  Find doc with at least on missing label:  '!(authors & title & abstract)'
  //  Find doc with at least on missing label:  '!(authors | title | abstract)'


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



   Labeling target as grid-of-grids of target regions:

     Inner grid (e.g, the pages of a document, or joined lines of text comprising a reference, or author list, etc.)
       labeler definition (request definition)
          target-regions=3,4,5,... 40; (all regions/pages that make up a "document", s.t. zones can cross pages in this set)
          view-grid=4x4; (or MxN|flow|concat, where m or n can be variable length, or can be evenly spaced or flowed together)
          pagination-increment=2; ('next' button jumps forward this far)
          pagination-start=2;
          flow-dir=horizontal|vertical;
          target-labels=author,title,...

       labeler response: (definition + current state, label colors)
          label-def: {above definition}
          label-colors: authors=blue,...
          label-state: [authors=4@specified, titles=0@specified, abstract=unspecified]          ; how many regions have these labels
          ?unlabeled-state?: % of regions that have none of the target labels applied

     Outer grid:
       view-grid=5x3;
       labeler-defs: [{[9, 10,...], ...}, {..}]
       labeler-def-overrides:
           view-grid=1x1;
           pagination-start=0;
           flow-dir=H;
           target-labels=author,title,...


   To construct a grid of page-1 regions for labeling:
      Find docs with labels state:  count(title)=unspecified

   Find next unexamined document re: title :
      select * from zone left outer join (w)orkflow join page join document
        where w.id is null


   Find next examined but unspecified document re: title :
      select * from zone join (w)orkflow join page join document
        where  w.label=title
          and  w.status=examined


   Workflow Relation structure:
      zone           : Int@@ZoneID
      targetregion   : Int@@RegionID
      label          : Int@@LabelID
      workflowStatus : (unexamined|examined|specified)
      user           : Int@@UserID











   */



}
