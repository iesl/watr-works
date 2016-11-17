
WatrWorks is PDF text extraction and analysis system that improves on existing
open-source systems in several respects. It improves on the accuracy of
character-level extraction, particularly when handling symbols, Greek letters,
and other math-related characters. The exact dimensions of the bounding box for
each extracted character is calculated, which allows for accurate spatial
analysis of layout. Through spatial analysis, text-line formatting is preserved,
including super- and sub-scripting, along with column finding and left/right
justification. Text is grouped into lines and labeled as paragraphs, section
headings, abstracts, and other high-level organizational units. Several output
formats are supported, including SVG and JSON.


Our work with MIT has focused on improving text extraction in material science
literature. Particular attention is given to chemical formulas, symbols used to
express quantities and measures, and labeling sections of text that are of
interest. IESL and MIT have coordinated through weekly online meetings, as well
as larger group meetings each semester.


- TODO create a way to label paper failure w/reasons, for future reference
  e.g., the paper text is actually images, with hand-entered transcription text underneath


- extraction of text particular to material science:
-- formula, math, super/subscript parsing
-- Parsing sentence structure while respecting the atomicity of chemical formula


+ Character encoding
  + Problem: the value representing the character is not always printable (ASCII or Unicode)
    + In the case of unprintable characters, the font dictionary is supposed to supply a Unicode version, but
      it does not always provide one, or it provides an incorrect value
       (e.g., a stylized '*' is extracted as '§', Greek letters α, β  specify unicode equivalents as a, b)

  + Combining marks (acute, grave, diuresis) make unicode translation difficult
  + perhaps try a font analysis to determine "true" character?

+ lines of text that get omitted between the SVG and the XML.


+ RPP and paragraph-finding.
  + Currently have 2 paragraph-finding implementations, porting one over to SVG
  + marginalia interfers with text flow, make paragraph finding across pages/columns difficult



Alignment walkthrough:

wt> corpus.entries()
entry1
entry2
..

wt> val paperEntry1 = corpus.entries()(1)

wt> paperEntry1.list           //  List[Component] == VisualLines
"line 1"
"line 2"
...

wt> paperEntry1.list(TextBlock).list(VisualLine)           //  List[List[Component]] == VisualLines



///  Labeling entry for entity/operation/conditions
for (paras <- paperEntry1.labels(Paragraph)) yield {
  val paraMatch = bestMatch(mitParas, para) // figure out the mit-paragraph text span that corresponds to this para


  val visualLineText = for(visLine <- para.labels(VisualLine)) yield text(visLine)

  val effectiveParaText = hjoin(lines).joinPairwise{(l1, l2) => (/* join lines of text together into single line */)}
  val matches = mitEntities.map{ e => (e.rawText, effectiveParaText.matchString(e.rawText)) }

  val entitySpans = matches.map {
    (text, Some(entMatch) => hjoin(entMatch.targetRegions).joinPairwise { (e1, e2) => e1 + "..." + e2 }
    (text, None) =>
  }


  val ent+op+cond-layout := (col:entity, col:operation, col:condition)

  ( "Paragraph"
  % image(para)
  % visualLineText)

}


- Features

- Mathematical symbols, Greek
- Line-based formatting information
- Simplification of unicode text to a canonical standard
  + e.g., reduce combining mark + letter to single codepoint: '̂ '+a => â
  
- Text alignment tools
- Spatial alignment tools
- Labeling 
  
 
-
