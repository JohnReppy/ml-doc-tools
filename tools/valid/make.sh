#!/bin/sh
#

OBJ="valid"

SML=${SML:-sml}

$SML <<XXX
  CM.autoloading(SOME false);
  CM.make();
  (SMLofNJ.exportFn("$OBJ", Main.main)):unit;
XXX

mv $OBJ.*-* ../../bin/.heap/
