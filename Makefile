HUGO ?= hugo

.PHONY: all build rebuild clean serve watch check
all: build

build:
	$(HUGO) --cleanDestinationDir --printI18nWarnings --panicOnWarning

rebuild: build

clean:
	rm -rf public

serve watch:
	$(HUGO) server

check: build
	HUGO="$(HUGO)" sh scripts/check-site.sh
