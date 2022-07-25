# ML Doc Tools

## Introduction

This is a release of the ML-Doc toolset.  It requires that you have
[SML/NJ](https://smlnj.org) version 110.99+ installed (including the ML-Yacc,
ML-Lex, and SML/NJ utility, HTML, and PP libraries).  Currently, there is no
documentation (other than this file), but there is some useful information in
the various README files.  Look at the `ml-doc.dtd` file
(in `ML-Doc/lib`) for a description of the markup.

This software is covered by a BSD 3-Clause License (see the `LICENSE` file).

The ML-Doc tools were developed to support the writing of the [**Standard
ML Basis Library** specification](https://smlfamily.github.io/Basis/).  They
provide a mechanism for generating both LaTeX and HTML output from a SGML
file.

## Requirements

We use the `nsgmls` (or `onsgmls`) program as a preprocessor.  This program
was developed by James Clark as part of his SP package.  There is a version
available from the **OpenJade** project (http://openjade.sourceforge.net)
as well as from James Clark's website (http://www.jclark.com/sp/index.htm).
On **macOS**, you can install the `open-sp` package from [Homebrew](https://brew.sh).

## Installation

See the `INSTALL` file for instructions.

## ML-DOC TOOLS

Here is a summary of the tools in this distribution:

* `bin/extract-info`
    -- Extracts a summary info (.info) file from an ML-Doc source file.

* `bin/extract-sig`
    -- Extracts SML signatures from ML-Doc source files.

* `bin/filter-index`
    -- Postprocessing for index files to get merge duplicate pages have
    different encaps.

* `bin/html-gen`
    -- Generates HTML from ML-Doc source files (and the Master.info file
    for the document).

* `bin/html-index`
    -- Generates an HTML index from Master.info file for the document.

* `bin/html-toc`
    -- Generates an HTML table of contents for the document.

* `bin/merge-info`
    -- Merges the individual ".info" files into a master info file.
    A typical document will have two versions of this file; one for
    HTML and one for LaTeX.

* `bin/latex-gen`
    -- Generates LaTeX for producing hardcopy versions of the documentation.

* `bin/mk-mldoc-makefile`
    -- Generates a makefile for a document.

* `bin/mkdoc`
    -- Generates skeleton ML-Doc files from SML source code.  You can use
    the flag `-before` or the flag `-after` to copy comments from the
    SML signature file into the generated ML-Doc file (before looks for
    comments before the specification, while after looks for comments
    after the specification).  There are also options for specifying
    the module that you want a signature to be assigned to:

     - `-s` `sname`           gives `<STRUCTURE STRID="sname">`
     - `-i` `sname`           gives `<SIGINSTANCE> <ID>sname`
     - `-f` `sname` `sid` `ssig`  gives `<FUNCTOR FCTID="sname"><ID>sid</ID><ID>ssig</ID>`

    Using `+` instead of `-` causes the matching to be opaque.

* `bin/proof`
    -- Generates "proofs" of the ML-Doc source files in LaTeX.  Note that
    this is not intended to be a final presentation form; but is just
    meant as a way to proof check the document.


## Creating an Ml-Doc Document

Each ML-Doc document lives in a directory with the following structure:

    $ROOT/Config.cfg	- the document's configuration file
    $ROOT/CATALOG	- the document's catalog
    $ROOT/Entities.sgml	- document specific entities
    $ROOT/ML-Doc	- directory containing ML-Doc source files
    $ROOT/HTML		- directory to place generated HTML files
    $ROOT/Info		- directory to place generated Info files
    $ROOT/Hardcopy	- directory to place generated latex files
    $ROOT/Proof		- directory to place generated proof latex files

where `$ROOT` is the root directory of the document.

To create a document, first you should create the directory structure described
above.  Then you need to create the CATALOG, Config.cfg, and Entities.sgml files.

The CATALOG is used by nsgmls to find things like the ML-Doc DTD.  It should
have the following contents (at least):

    ENTITY	%document-entities	"Entities.sgml"
    CATALOG				"<mldoc-root>/lib/catalog"

where `<mldoc-root>` is the path of the root of the ML-Doc installation.

The Config.cfg file contains configuration information for the ML-Doc tools.
Here is the Config.cfg file from the SML'97 basis document:

```
    # Config.cfg
    #

    Catalog		"/home/sml/Doc/Basis/CATALOG"

    HTML		{
	BaseURL		    "/cm/cs/what/smlnj/Basis"
	Template	    "test.template"
    }

    Proof-LaTeX		{
	TopLevelSection	    "Chapter"
    }

    Tools		{
	SGMLS		    "/usr/local/bin/nsgmls"
    }
```

A config file consists of name/value pairs, where values are strings,
integers or collections of name/value pairs (inside "{" "}").  This
Config file specifies the path of the CATALOG file, and where to find
the Master info file for all tools.  It also specifies tool-specific
information for HTML tools (html-gen and html-index), proof-latex, and
where to find the SGMLS program.

The Entities.sgml file is an SGML file that specifies the entities which
are specific to this document.  The most important of these are the
entities that specify source code file names for the various SML signatures.
For each <SIGBODY> element with a FILE attribute in the document, we
must have an entity defined in the Entities.sgml file.

## Acknowledgements

Andrew Appel wrote the first version of the latex-gen tool, Emden Gansner
wrote the mkdoc and valid tools; Lal George wrote the proof-latex tool;
and Dan Wang provided the comment extraction feature in the mkdoc tool.
Alley Stoughton has provided helpful fixes at various times.
