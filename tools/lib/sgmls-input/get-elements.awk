#!/bin/nawk -f
#

BEGIN {
    }

($1 == "<!ELEMENT") {
	printf("    val elem%s\t\t= Name.mkName \"%s\"\n", $2, $2)
#	printf("      | elemName %s = \"%s\"\n", $2, $2)
#	printf("\t\t  (elem%s, mk %s),\n", $2, $2)
#	printf("      | %s of {}\n", $2)
#	print $2
    }

END {
    }
