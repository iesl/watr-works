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
    echo "  verify that all *.json files in specified dir parse correctly  "
    exit 2
}

# default arg vals
checkdir=$1

# while getopts "d:h" name; do
#     case $name in
#         d)    checkdir=$OPTARG;;
#         h)    showhelp $0;;
#         [?])  usage $0;;
#     esac
# done
echo "checkdir=$checkdir"

if [ -z "$checkdir" ]; then
    ## ...
    exit 2
fi

find $checkdir -type f -name 'docseg*' -printf "echo \"%p\" && cat '%p' | python -m json.tool\n" | sh | egrep -v "^   "
