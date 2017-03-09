#!/bin/sh

# config: set source doc and output
DOC="doc"
OUT="target/site/doc"

function die(){
 echo $1
 exit 1
}

# make sure that it works when invoked from an other directory
cd `dirname $0`


# check for required commands and remove old output
command -v xsltproc   > /dev/null || die "Command 'xsltproc' not found"
command -v mvn   > /dev/null || die "Command 'mvn' not found"
command -v yelp-build > /dev/null || die "Command 'yelp-build' not found"
#test -e $OUT && echo "Confirm to remove '$OUT'..." && rm -Ir "$OUT"

# First build the javadoc
#mvn javadoc:javadoc
mkdir -p "$OUT"

cd $DOC
echo Generating $DOC ...

# generate biblio page if needed
if [ -e biblio.xml ]
then
	xsltproc -o biblio.page xslt/biblio.xslt biblio.xml || exit 1
fi

# generate HTML version of the documentation
yelp-build html -x "xslt/custom.xslt" -o ../$OUT *.page || exit 1

# cleanup
test -e biblio.xml && test -e biblio.page && rm biblio.page
cd ..

