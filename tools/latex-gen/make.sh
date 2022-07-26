#!/bin/sh
#

OBJ="latex-gen"
SML=${SML:-sml}

$SML <<XXX
  CM.make "sources.cm";
  (SMLofNJ.exportFn("$OBJ", Main.main)):unit;
XXX

mv $OBJ.*-* ../../bin/.heap/
