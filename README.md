# Watr-Works

A set of related projects to extract text from PDFs.

## TextWorks PDF text extractor

### Obtaining

A prebuilt version of the most recent version of TextWorks can be fetched like so:


From the github release page
  https://github.com/iesl/watr-works/releases/
  
Download the latest binary release (under 'Assets'):
    textworks-app.tar.gz

Extract the application, e.g.,

> tar xvfz textworks-app.tar.gz

### Running the command-line app

The extracted app will have the following structure:

    textworks-app
    ├── bin
    │   └── textworks
    └── lib
        └── *.jar

Place the bin/ directory on your path, or run it as

> /path/to/textworks-app/bin/textworks --help

to see options.

## Running on a single file

> textworks --file document.pdf --output document.json

Specify --overwrite if the output file already exists

## Running on a large corpus

### Corpus structure and initialization

Textworks expects the PDFs to be organized such that there is one subdirectory
per PDF, and all generated artifacts (as well as the original PDF) are grouped
together in the same directory. The textworks app can perform the initializions step
by creating appropriately named subdirectories (hereafter called artifact directories), and
stashing each pdf in its artifact directory.

To initialize a corpus, place the PDFs in some directory:

    ./corpus/root
    ├── alpha.pdf
    ├── beta.pdf
    └── gamma.pdf


Then run:
> textworks init --corpus ./path/to/corpus/root

This will result in the following structure:

    ./corpus/root
    ├── .corpus-root
    ├── alpha.pdf.d
    │   └── alpha.pdf
    ├── beta.pdf.d
    │   └── beta.pdf
    └── gamma.pdf.d
        └── gamma.pdf


There is a marker file named '.corpus-root' placed in the root directory. It is created by textworks,
when initializing a corpus, and later checked when processing corpus entries.

New PDFs may be added to the root of the corpus directory and the init step may be re-run at any time, 
without disturbing existing entries.


## Extracting text

By default, textworks will process everything in the specified corpus directory. 

> textworks --corpus ./path/to/corpus/root [--overwrite]

This will write a file named textgrid.json to the root of each pdf artifact directory. It will check for 
the existence of this file first, skipping PDFs that have already been processed, unless --overwrite is specified.


After processing, the directory will look like so:

    ./corpus/root
    ├── alpha.pdf.d
    │   ├── textgrid.json
    │   └── alpha.pdf
    ├── beta.pdf.d
    │   ├── textgrid.json
    │   └── beta.pdf
    └── gamma.pdf.d
        ├── textgrid.json
        └── gamma.pdf


## Selecting corpus entries to process

## Document Segmentation

### Output Format for Document Segmentation

Segmentation produces a json file like this:


    {"description" : "Extracted Pages for 10.1101-056275.d",
      "documentId" : "10.1101-056275.d",
      "pages" : [{"pageGeometry" : [0, 0, 61200, 79200],
          "textgrid" : {"stableId" : "10.1101-056275.d",
            "rows" : [{"offset" : 0,
                "text" : "Prediction of allosteric sites and mediating interactions through bond-to-bond",
                "loci" : [{"g" : [["P", 0, [7792, 5383, 798, 820]]]





    { "pages": [
        {"page": 0,
         "blocks": [
           {"labels": ["header", "publisher"],
            "lines": [
              [["©","2014","Elsevier","B.V.","All","rights","reserved."],     [0,1,2,3,4,5,6]]
            ]},



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
