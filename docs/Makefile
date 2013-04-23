LATEX_OPTS=--table-of-contents --latex-engine=pdflatex --include-in-header=latex_hdr.tex
LATEX_VARS=-V geometry:margin=1in -V numbersections:true -V "include-before:\thispagestyle{firstpage}" -V tables
DOC=LS1MCS-Architecture

all: $(DOC).html $(DOC).pdf

$(DOC).html: $(DOC).md
	pandoc -s -o $(DOC).html $(DOC).md

$(DOC).pdf: $(DOC).tex
	sed 's/\[htbp\]/\[H\]/' -i $(DOC).tex
	pdflatex $(DOC).tex
	pdflatex $(DOC).tex

$(DOC).tex: $(DOC).md
	pandoc -f markdown -t latex -s $(LATEX_OPTS) $(LATEX_VARS) -o $(DOC).tex $(DOC).md

clean:
	rm -f $(DOC).html $(DOC).tex

img-add:
	cat $(DOC).md | grep "^\!.*uml/" | sed "s|^\!.*uml/\(.*\))|uml/\1|" | xargs git add -f

img-clean:
	rm -f uml/*.png


