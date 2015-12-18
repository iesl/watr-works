
WATR-works
=====

A set of related projects to extract text from PDFs


--- Notes for MIT gang...
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

+ RPP
  + overall better, still some missing text
  + issues with fonts
  + superscripts, subscripts
    + is it possible to get necessary info (chemical formulae) without this?

+ Manual annotation
  + walking through an example so you see how the data structure has evolved


+ Priorities
  + Finish graphical annotator, so that we have an established way to share annotation tasks,
    view results and and record errors
  + Port paragraph-handling code to work over SVGs
  + Maybe use font data analysis to improve font-to-unicode translation
  + Handle combining marks to improve text quality


Font substitution, tested over ~800 documents:
  Running on our linux cluster (CentOS), ~8k font substitutions, 100% of documents
  Running on my Ubuntu laptop, ~2k font substitutions, 70% of documents


  Specific fonts substituted on CentOS: 55 fonts, e.g.,
    Substituting font Bookman-Demi for BookmanOldStyle.
    Substituting font Bookman-DemiItalic for BookmanOldStyle,Italic.
    Substituting font Bookman-Light for BookmanOldStyle-Bold.
    Substituting font Bookman-LightItalic for BookmanOldStyle,BoldItalic.
    Substituting font Courier-Bold for CourierNew,Bold.
    Substituting font Courier-Bold for LetterGothicMT-Bold.
    Substituting font Courier-BoldOblique for CourierNew,BoldItalic.
    Substituting font Times-Roman for Wingdings.




