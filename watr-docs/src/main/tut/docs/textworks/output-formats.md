---
layout: docs 
title: Output Formats 
---

### Output Format for Document Segmentation

Segmentation produces a json file like this:


    { "pages": [
        {"page": 0,
         "blocks": [
           {"labels": ["header", "publisher"],
            "lines": [
              [["©","2014","Elsevier","B.V.","All","rights","reserved."],     [0,1,2,3,4,5,6]]
            ]},
           {"labels": ["body", "section-heading"],
            "lines": [
              [["1.","Introduction"],     [7,8]]
           ]},
           {"labels": ["body", "paragraph"],
            "lines": [
              [["{PhSiH_{3}}","into","the","corresponding","phosphines."],     [643,644,645,646,647]],
          ]
        },
        {"page": 1,
         "blocks": [
             ...
         ]
        },
     },
     "ids": [
        [0,[0, [426.41, 556.48, 5.90, 7.17]]],[1,[0, [434.17, 556.48, 17.26, 7.17]]],
        [10,[0, [367.43, 687.76, 14.25, 7.97]]],[11,[0, [383.27, 687.76, 56.16, 7.97]]]
     ]}


Each page consists of a list of blocks, where each block is a labeled set of text lines. The labels identify the section
of the paper where the text appeared (header, body, or references), and, if it can be determined, the role of that
block of text (section-heading, paragraph, caption, table). The text blocks appear in reading-order, as best as can
be determined by layout analysis.


Each line in a block of text is a comma separated list of tokens, followed by an equal length list of IDs for each token.
At the end of the json file is an "id" section, which lists, every token id with the format

    [idnum, [pagenum, [top,left,width,height]]]

which describes the page number and bounding box for that particular token.

Some portions of a token may have special formatting information, like super- or sub-script annotation. If this is the case,
The token will begin and end with braces, like so:

    "{PhSiH_{3}}"

Within the token, superscripts are designated with ^{...}, subscripts with _{...}.

### Escaped Characters:

The following characters are always escaped:
   standard json escapes: '"', '\', ..
   braces: '{', '}'

Inside of a tex-formatted token, '_', '^' are also escaped.
