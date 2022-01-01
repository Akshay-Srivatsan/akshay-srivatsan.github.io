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

TRANSLITERATE=transliterate/target/release/transliterate
RUST = $(wildcard transliterate/src/*.rs)
JS = transliterate/sanskrit.js transliterate/tamil.js

.PHONY: all
all: $(HTML) $(JS)

$(TRANSLITERATE): $(RUST) transliterate/Cargo.toml
	cd transliterate && cargo build --release

transliterate/%.js: transliterate/%.yaml $(TRANSLITERATE)
	$(TRANSLITERATE) --input $< --output $@

%.html: content/%.md $(TEMPLATES) Makefile
	pandoc $< --output $@ $(PANDOCFLAGS) --variable url="$(BASEURL)/$@"

.PHONY: clean
clean:
	rm -rf $(HTML)

.PHONY: serve
serve:
	miniserve .

.PHONY: watch
watch:
	sh -c "trap 'kill 0' SIGINT; while true; do inotifywait -emodify -r .; $(MAKE) dev; done"

.PHONY: run
run:
	sh -c "trap 'kill 0' SIGINT; $(MAKE) serve & $(MAKE) watch & wait"

.PHONY: dev
dev: all
	$(MAKE) refresh

.PHONY: refresh
refresh:
	$(eval CID := $(shell xdotool getwindowfocus))
	$(eval WID := $(shell xdotool search --name "Mozilla Firefox"))
	$(MAKE) $(patsubst %,%.refresh,$(WID))
	xdotool windowactivate $(CID)

.PHONY: %.refresh
%.refresh:
	xdotool windowactivate $*
	xdotool key F5
