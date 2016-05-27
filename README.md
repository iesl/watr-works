# WATR-works
A set of related projects to extract text from PDFs

## Running document segmentation

From the project root,
> bin/works [cmd] [args]

For help,
> bin/works --help


To run segmentation, put all pdfs you wish to process in a directory,
then run

> bin/works init -c ./path/to/dir/with/pdfs

This will create a directory for each pdf to hold all generated artifacts.
You can add new pdfs to the directory and re-run init without disturbing existing pdfs.

After that, run segmentation with:

> bin/works docseg -c ./path/to/dir/with/pdfs

This will generate a file named docseg.json for each pdf.



## Output Format for Document Segmentation
Segmentation produces a json file like this:

    { "pages": [
          {"page": 0,
           "lines": [
              [["©","2014","Elsevier","B.V.","All","rights","reserved."],     [0,1,2,3,4,5,6]],
              [["1.","Introduction"],     [7,8]],
           ...
           ]},
          {"page": 2,
           "lines": [
               [["{PhSiH_{3}}","into","the","corresponding","phosphines."],     [643,644,645,646,647]],
           ]}
     },
     "ids": [
        [0,[0, [426.41, 556.48, 5.90, 7.17]]],[1,[0, [434.17, 556.48, 17.26, 7.17]]],
        [10,[0, [367.43, 687.76, 14.25, 7.97]]],[11,[0, [383.27, 687.76, 56.16, 7.97]]]
     ]}

Each line of a page is a comma separated list of tokens, followed by an equal length list of IDs for each token.
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







