SOURCES=*.tex
BIB=references.bib

all: quick

diff: 
	latexdiff -c ld.cfg paper.tex paper-edited.tex > paper-diff.tex; latexmk -pdf paper-diff.tex

bib: $(SOURCES) $(BIB)
	pdflatex paper
	bibtex paper
	pdflatex paper
	pdflatex paper

quick:  $(SOURCES)
	pdflatex paper
