
*** Varieties of bounding boxes: (anything with some type of x,y,width,height)
    - spatialindex.Bounds
      - stores min/max pairs for x/y
      - can readily extend to higher dimensions
    - watr.db.Rect
      - slick marshalling case class
    - watrmarks.dom.{Viewbox, Rect}
      - SVG element reprs


- [ ] Create a bbox visualizer for pdf.js SVG pages, at the char level
  - [ ] output includes: bbox, text (char), font info/remappings, spelling/alternatives

- [ ] Load Cermine-generated bboxes and overlay onto SVG pages

- [ ] Serialize annotation bboxes into svg as svg-rects


- [ ] Create initial annotations on SVG, including:
  - [ ] headers (title/abstract in particular)
  - [ ] body + paragraph markers


- [ ] augment pdf.js svg output to include full bbox info, font info


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



- super/sub
  - Mx_{3}O_{2}   K mn${3.1}
  - How to represent a/b (super/sub)
    -  *just use super followed by sub

- How to represent a token that has sub-tokenization?
  - Mx_{3}O_{x + 4}   K mn^{3.1 * a}
  - ligature substitution
  - join hyphenated lines

 - ascii subs for m-dash, n-dash
 -- hanging indents??
 
 - numbered lists (aligned list elements, aligned numbers)


 Ni–Co–Mn–In a

-how to represent symbols w/no ascii version?

{1 1 2}fcc||{1 1 2}bcc planes

\{1 1 2\}_{fcc}||\{1 1 2\}_{bcc} planes



// nto a (NH_4_)_2_HPO_4_ solution of 0.25 mol l ^1^ at a molar ratio                                                                                          (l:42.52, t:472.61, w:251.01, h:12.76) 


                                                                                                                                                                                                                                                                                            
Page:0 file:///home/saunders/projects/the-livingroom/rexa-text-extractors/watr-works/corpus-one/101016jactamat200710045.pdf.d/101016jactamat200710045.pdf                                                                                                                                    
                                                                                                                                                                                                                                                                                             
Available online at www.sciencedirect.com                                                                                                                                                                                                                                                    
Acta Materialia 56 (2008) 913–923                                                                                                                                                                                                                                                            
www.elsevier.com/locate/actamat                                                                                                                                                                                                                                                              
In situ high-energy X-ray studies of magnetic-ﬁeld-induced                                                                                                                                                                                                                                   
phase transition in a ferromagnetic shape memory Ni–Co–Mn–In alloy                                                                                                                                                                                                                           
Y.D. Wa _J_ n _.__N_ g _._ ^a^^,^_D_ ^b^^,^^*^ _e_ ,_n_E _g_.W _a__,_ . _H_ H _._ u _C_ a _h_ n _o_ g _o_^b^_b_ ,_,_Y _P_._.__K_ R _._en _L__i_^c^ _a_ ,_w_ Z. _b_ H _,_ _D_ . N _.__E_ ie _._ _B_ ^a^,_r__o_ G _w_ ._n_ W _d_a _,_ n _L_ g _._ ^a^ _Z_ ,_u_ Y _o_.D _a_ . Liu ^a^,          
^a^ Key Laboratory for Anisotropy and Texture of Materials (Ministry of Education), Northeastern University, Shenyang 110004, China                                                                                                                                                          
^b^ D e p a r t m e n t _c_ o _X_ f _-_ M _r_ _a_ a _y_ t e _S_ r i _c_ a _i_ _e_ l s _n_ _c_ S _e_ c i _D_ e n _i_ _v_ c _i_ e _s_ _i_ a _o_ n _n_ d _,_ _A_ E _r_ n _g_ g _o_ i _n_ n _n_ e e _e_ r i _N_ n g _a_ , _t_ _i_ T _o_ _n_ h _a_ e _l_ U _L_ n _a_ i _b_ v _o_ e _r_ r _a_ s i _




        /*
         ı φ α · Δ ⋅
         ψ  τ ˆ ß μ ⁄ Ω   ø ϕ π € Æ θ Ł   × δ º   ∆ ¼  ð  ˇ
         Γ β ° ω ρ  ˚ σ Þ  → ˜ ν   ο η  ¨   γ ⇑   Ø ε  ½ µ þ ´

         + large parenthesis chars
         ⎛  ⎞
         ⎜  ⎟
         ⎝  ⎠

         Å
         ±
         ‘
         ’
         “”
         Ι

         */
