PKG := $(shell head -1 DESCRIPTION | sed 's/Package: //' | cat)
VERSION := $(shell sed -n 3p DESCRIPTION | sed 's/Version: //' | cat)
BINARY := $(PKG)_$(VERSION).tar.gz
MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
CURRENT_DIR := $(notdir $(patsubst %/,%,$(dir $(MAKEFILE_PATH))))
R_ARGS := --no-site-file --no-environ --no-save \
	  --no-restore --no-resave-data --no-manual --quiet
ifneq (, $(findstring r-devel,$(CURRENT_DIR)))
  R := R_devel.sh
else
  R := R
endif

all: clean docs data readme build check install

quick: clean 

clean:
	@rm -rf src/*.so src/*.o *tar.gz *Rcheck*

build: 
	$(R) $(R_ARGS) CMD build .  

build-cran:
	$(R) CMD build . --no-resave-data --no-manual
	
check: 
	$(R) CMD check $(PKG)_$(VERSION).tar.gz

check-cran: 
	$(R) CMD check --as-cran $(PKG)_$(VERSION).tar.gz

check-quick $(PKG)_$(VERSION).tar.gz:
	$(R) $(R_ARGS) CMD build . 
	$(R) CMD check $(PKG)_$(VERSION).tar.gz

install: $(PKG)_$(VERSION).tar.gz
	$(R) CMD INSTALL --no-multiarch --with-keep.source \
	  $(PKG)_$(VERSION).tar.gz

install-code:
	$(R) CMD INSTALL --no-multiarch --with-keep.source --no-docs .

install-quick:
	$(R) CMD INSTALL --no-multiarch --no-docs --no-html \
	  --with-keep.source .

readme: README.Rmd
	$(R) --vanilla --slave -e "rmarkdown::render('README.Rmd')"

docs:
	$(R) --vanilla --slave -e "devtools::document()"

data:
	Rscript --vanilla --slave tools/example_objects.R
	Rscript --vanilla --slave tools/test_objects.R

site:
	$(R) --vanilla --slave -e "pkgdown::build_site()"

.PHONY: clean docs data
