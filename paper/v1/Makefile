SOURCES=*.tex
BIB=references.bib

all: quick

bib: $(SOURCES) $(BIB)
	pdflatex paper
	bibtex paper
	pdflatex paper
	pdflatex paper

quick:  $(SOURCES)
	pdflatex paper
