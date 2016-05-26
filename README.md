WATR-works
=====
A set of related projects to extract text from PDFs

Running document segmentation
----

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











