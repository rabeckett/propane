SOURCES=*.tex
BIB=references.bib

all: quick

bib: $(SOURCES) $(BIB)
	pdflatex sosr
	bibtex sosr
	pdflatex sosr
	pdflatex sosr

quick:  $(SOURCES)
	pdflatex sosr
