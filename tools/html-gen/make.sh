#!/bin/sh
#

OBJ="html-gen"
SML=${SML:-sml}

$SML <<XXX
  CM.make();
  (SMLofNJ.exportFn("$OBJ", Main.main)):unit;
XXX

mv $OBJ.*-* ../../bin/.heap/
