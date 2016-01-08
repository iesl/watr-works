#!/bin/bash

SCRIPT=`basename $0`
SCRIPTDIR=$(cd `dirname $0` && pwd -P)

usage() {
    local prog="$SCRIPT"
    echo "Usage: $prog <todo>"
    echo "       $prog -h for help."
    exit 2
}

showhelp() {
    echo "Usage: $SCRIPT: "
    echo "  todo  "
    exit 2
}

# default arg vals
FILEPATH=

while getopts "f:" name; do
    case $name in
        f)    FILEPATH=$OPTARG;;
        [?])  usage $0;;
    esac
done

FILE=$(basename $FILEPATH)

# pdf
java -cp $SCRIPTDIR/cermine-impl-1.8-SNAPSHOT-jar-with-dependencies.jar pl.edu.icm.cermine.PdfNLMContentExtractor -str -path $FILEPATH

# To extract metadata from a reference string:
#	$ java -cp target/cermine-impl-1.7-SNAPSHOT-jar-with-dependencies.jar pl.edu.icm.cermine.bibref.CRFBibReferenceParser -reference "the text of the reference"

# To extract metadata from an affiliation string:
# java -cp target/cermine-impl-1.7-SNAPSHOT-jar-with-dependencies.jar pl.edu.icm.cermine.metadata.affiliation.CRFAffiliationParser -affiliation "the text of the affiliation"
