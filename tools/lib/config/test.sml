use "config-scan.sml";
use "config-parse.sml";

structure S = ConfigScan;

fun scanAll strm = (case S.nextTok strm
       of NONE => print "EOF\n"
	| SOME(tok, strm) => (
	    print(concat["token: ", S.toString tok, "\n"]);
	    scanAll strm)
      (* end case *));

fun testScan file = scanAll(ConfigScan.mkStream{errMsg=print, name=file});

fun testParse file = ConfigParse.parse {errMsg=print, name=file};

