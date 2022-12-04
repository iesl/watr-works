---
layout: docs
title: Output Formats
---

### Output Format for Document Segmentation

Segmentation produces a json file like this:

```json




```

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


### Annotation Format

```json
    {
      "stableId" : "10.1101-007096.d",
      "annotations" : [
        {
          "id" : 1438,
          "document" : 70,
          "owner" : null,
          "annotPath" : "Bioarxiv.UmaCzi2018.250PlusGold.ExternalTeam",
          "created" : 1523461739018,
          "label" : "Keywords",
          "location" : {
            "Zone" : {
              "regions" : [
                {
                  "page" : {
                    "stableId" : "10.1101-007096.d",
                    "pageNum" : 0
                  },
                  "bbox" : {
                    "left" : 9036,
                    "top" : 46087,
                    "width" : 35298,
                    "height" : 3936
                  }
                }
              ]
            }
          },
          "body" : {
            "stableId" : "10.1101-007096.d",
            "rows" : [
              {
                "offset" : 0,
                "text" : "Keywords: ",
                "loci" : [
                  {
```
