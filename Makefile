HTML_TEMPLATE=templates/index.html
TEMPLATES=$(wildcard templates/*)

SOURCE=$(wildcard content/*.md)
HTML=$(patsubst content/%.md,%.html,$(SOURCE))

PANDOCFLAGS=--from markdown+bracketed_spans+fenced_divs\
			--standalone\
			--template=$(HTML_TEMPLATE)\
			-fmarkdown-implicit_figures\
			--variable copyyear=$(shell date +"%Y")\
			--variable author="Akshay Srivatsan"\
			--variable image=assets/img/portrait-small.jpg

BASEURL=https://aks.io

.PHONY: all
all: $(HTML) mappings


PYTHON = $(wildcard transliteration/*.py)
mappings: $(PYTHON)
	cd transliteration && python3 generate_maps.py
	prettier --write transliteration/*.js

%.html: content/%.md $(TEMPLATES) Makefile
	pandoc $< --output $@ $(PANDOCFLAGS) --variable url="$(BASEURL)/$@"

.PHONY: clean
clean:
	rm -rf $(HTML)
