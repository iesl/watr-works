---
layout: docs
---

# Current Status


## Text extraction overview

## Output formats

### Line tokenization
### Common problems


Spacing in patterns like these:
```
            cind @nitt.edu
            www.*.*
            Pt/PANi
            [7,17–24]
            10^{5}s^{1}
            doi: 10.1029/2005JD006318
            70^{◦}C
            Fe_{3}O_{4}@C
```


-  words parsed as stream of single chars (particularly on line w/one word)
  - 2 0 1 2 J D 0 1 7 45 9 . 1010022013gl058232.pdf.d <target pg:4 (l:319.97, t:560.87, w:50.71, h:7.28)
-  sup/subs are squished w/adjacent words
-  ligatures get sup/subscripted
-  inclusion of technical discipline-specific vocabulary in addition to default dictionary
-  common phrase parsing (e.g, "et al." as a single token)
-  parse footnote sup/sub markers as independent tokens
-  A few reversed words across entire paper ??!! (I think doc orientation value is incorrect)
-  Non-textline elimination
-  better weight/measure/quantity parsing and tokenization

### Symbol Glyph Identification

Accurate extraction of math symbols, Greek/Latin alphabet, common scientific symbols

### Document Layout Analysis

Accurate identification of tables, graphs, charts, footnotes, sections, and paragraphs.
