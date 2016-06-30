SOURCES=*.tex
BIB=references.bib

all: quick

bib: $(SOURCES) $(BIB)
	pdflatex propane
	bibtex propane
	pdflatex propane
	pdflatex propane

quick:  $(SOURCES)
	pdflatex propane
