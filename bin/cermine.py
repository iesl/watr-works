#!/usr/bin/python

from subprocess import call
import os
import argparse

def script_dir():
    return os.path.dirname(os.path.realpath(__file__))

def extract_nlm(path):
    # To extract the content from PDF files:
    print "{sd}/cermine-impl-1.8-SNAPSHOT-jar-with-dependencies.jar".format(sd=script_dir())

    call([
        "java", "-cp", "{sd}/cermine-impl-1.8-SNAPSHOT-jar-with-dependencies.jar".format(sd=script_dir()),
        "pl.edu.icm.cermine.PdfNLMContentExtractor",
        "-path", path
    ])

def main():
    args = parse_arguments()
    print args
    print args.file
    extract_nlm(args.file)


def parse_arguments():
    parser = argparse.ArgumentParser(description='')
    # parser.add_argument('--file', type=file, help='input file')
    parser.add_argument('--file', help='input file')
    parser.add_argument('--extract-nlm', help='extract NLM-style')

    return parser.parse_args()

if __name__ == '__main__':
    main()



# To extract metadata from a reference string:
# $ java -cp target/cermine-impl-1.7-SNAPSHOT-jar-with-dependencies.jar pl.edu.icm.cermine.bibref.CRFBibReferenceParser -reference "the text of the reference"
# To extract metadata from an affiliation string:
# $ java -cp target/cermine-impl-1.7-SNAPSHOT-jar-with-dependencies.jar pl.edu.icm.cermine.metadata.affiliation.CRFAffiliationParser -affiliation "the text of the affiliation"
