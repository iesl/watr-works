---
layout: docs 
title: Corpus Management
---

## Corpus structure and initialization

bin/works expects the PDFs to be organized such that there is one subdirectory per PDF,
and all generated artifacts (as well as the original PDF) are placed together in the appropriate
artifacts directory, like so:

    .
    ├── .corpus-root
    ├── 0575.pdf.d
    │   ├── 0575.pdf
    │   ├── bbox.svg
    │   ├── docseg.json
    │   ├── lineseg.txt
    │   └── paragraphs.txt
    ├── 1483.pdf.d
    │   ├── 1483.pdf
    │   ├── bbox.svg
    │   ├── docseg.json
    │   ├── lineseg.txt
    │   └── paragraphs.txt
    └── 2839.pdf.d
        ├── 2839.pdf
        ├── lineseg.txt
        └── paragraphs.txt


bin/works can initialize directory filled with PDFs (all in the root of the directory), by creating the
subdirectories (hereafter called artifact directories), and stashing each pdf in its artifact directory.

There is a marker file named '.corpus-root' placed in the root directory. It is created by bin/works
when initializing a corpus, and later checked for when processing corpus entries. To initialize:

> bin/works init --corpus ./path/to/dir/with/pdfs

You can add new pdfs to the root of the corpus directory and re-run init without disturbing
existing entries.
