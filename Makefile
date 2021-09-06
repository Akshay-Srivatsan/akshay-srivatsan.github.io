HTML_TEMPLATE=templates/index.html
TEMPLATES=$(wildcard templates/*)

SOURCE=$(wildcard content/*.md)
HTML=$(patsubst content/%.md,%.html,$(SOURCE))

PANDOCFLAGS=--standalone\
			--template=$(HTML_TEMPLATE)\
			-fmarkdown-implicit_figures\
			--variable copyyear=$(shell date +"%Y")\
			--variable author="Akshay Srivatsan"\
			--variable image=assets/img/portrait-small.jpg

BASEURL=https://aks.io

.PHONY: all
all: $(HTML)

%.html: content/%.md $(TEMPLATES) Makefile
	pandoc $< --output $@ $(PANDOCFLAGS) --variable url="$(BASEURL)/$@"

.PHONY: clean
clean:
	rm -rf $(HTML)
