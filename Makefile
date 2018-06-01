# This is to make the stupid expansion in clean work.
SHELL=/bin/bash

BIB=bib.bib

#######################################################################
#                               Targets                               #
#######################################################################
.PHONY: all clean 

all: LinearTalk.pdf

%.tex: %.lhs
	lhs2TeX -o $@ $<

%.pdf: %.tex $(BIB)
	cd $(dir $<) && xelatex --shell-escape $(notdir $*)
	cd $(dir $<) && biber $(notdir $*)
	cd $(dir $<) && xelatex --shell-escape $(notdir $*)

	cd $(dir $<) && xelatex --shell-escape -jobname=$(notdir $*)_handout  "\PassOptionsToClass{handout}{beamer}\input{$(notdir $*)}"
	cd $(dir $<) && biber $(notdir $*)_handout
	cd $(dir $<) && xelatex --shell-escape -jobname=$(notdir $*)_handout  "\PassOptionsToClass{handout}{beamer}\input{$(notdir $*)}"

clean:
	rm -f *.tex *.aux *.bbl *.ptb *.pdf *.toc *.out *.run.xml *.blg *.log *.nav *.snm *.bcf
