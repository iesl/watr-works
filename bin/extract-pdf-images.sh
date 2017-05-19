#!/bin/bash

## http://stackoverflow.com/questions/59895/getting-the-source-directory-of-a-bash-script-from-within
SOURCE="BASH_SOURCE[0]"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
    DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
SCRIPTDIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
SCRIPT=`basename $0`

showhelp() {
    echo "Usage: $SCRIPT: "
    echo "  todo  "
    exit 2
}

# default arg vals
pdffile=

while getopts "f:h" name; do
    case $name in
        f)    pdffile=$OPTARG;;
        h)    showhelp $0;;
        [?])  showhelp $0;;
    esac
done

if [ -z "$pdffile" ]; then
    echo '-f pdffile required'
    exit 1
fi

pdfdir=$( dirname "$pdffile" )
pageimagedir="$pdfdir/page-images"
thumbdir=$pdfdir/page-thumbs

rm -rf $pageimagedir
mkdir $pageimagedir
rm -rf $thumbdir
mkdir $thumbdir

echo "Extracting page images"

mudraw_exists=$(hash mudraw 2>/dev/null && echo 1)
mutool_exists=$(hash mutool 2>/dev/null && echo 1)

# if [ -n $mudraw_exists ]; then
#     echo "mudraw exists; "
# fi
# if [ -n $mudraw_exists ]; then
#     echo "mutool exists;"
# fi

if [ -n $mudraw_exists ]; then
    # echo "using mudraw "
    mudraw -r 110 -o "$pageimagedir/page-%d.png" $pdffile
elif [ -n $mudraw_exists ]; then
    # echo "using mutool"
    mutool draw -r 110 -o "$pageimagedir/page-%d.png" $pdffile
fi


echo "Generating thumbnails"

thumbsize=150

mogrify -path $thumbdir\
        -filter Triangle\
        -define filter:support=2\
        -thumbnail $thumbsize\
        -unsharp 0.25x0.25+8+0.065\
        -dither None\
        -posterize 136\
        -quality 100\
        -define jpeg:fancy-upsampling=off\
        -define png:compression-filter=5\
        -define png:compression-level=9\
        -define png:compression-strategy=1\
        -define png:exclude-chunk=all\
        -interlace none\
        -colorspace sRGB\
        -strip $pageimagedir/*.png

echo "Optimizing image sizes"
pngquant --ext .opt.png $pageimagedir/*.png

find $pageimagedir/ -type f \( -not -name '*.opt.png' \) -exec rm {} ';'
