#!/bin/sh
#

TARGETS="\
  extract-sig \
  extract-info \
  html-gen \
  html-index \
  html-toc \
  latex-gen \
  merge-info \
  mk-mldoc-makefile \
  mkdoc \
"
#  proof-latex \
#  valid \

export SML

SML=${SML:-sml}

for i in $TARGETS
do
  if [ -d "$i" ]; then
    echo "********** Starting $i build **********"
    (cd $i; ./make.sh)
    echo "********** $i build complete  **********"
  fi
done

ls -l ../bin/.heap

