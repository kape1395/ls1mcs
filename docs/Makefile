LATEX_OPTS=--table-of-contents --latex-engine=pdflatex --include-in-header=latex_hdr.tex
LATEX_VARS=-V geometry:margin=1in -V numbersections:true -V "include-before:\thispagestyle{firstpage}"

all: LS1MCS-Architecture.html LS1MCS-Architecture.pdf

LS1MCS-Architecture.html: LS1MCS-Architecture.md
	pandoc -s -o LS1MCS-Architecture.html LS1MCS-Architecture.md

LS1MCS-Architecture.pdf: LS1MCS-Architecture.tex
	pdflatex LS1MCS-Architecture.tex
	pdflatex LS1MCS-Architecture.tex

LS1MCS-Architecture.tex: LS1MCS-Architecture.md
	pandoc -f markdown -t latex -s $(LATEX_OPTS) $(LATEX_VARS) -o LS1MCS-Architecture.tex LS1MCS-Architecture.md

clean:
	rm -f LS1MCS-Architecture.html LS1MCS-Architecture.tex

clean-img:
	rm -f uml/*.png

