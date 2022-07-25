#!/bin/sh
#

OBJ="extract-info"
SML=${SML:-sml}

$SML <<XXX
  CM.make();
  (SMLofNJ.exportFn("$OBJ", Main.main)):unit;
XXX

mv $OBJ.*-* ../../bin/.heap/
