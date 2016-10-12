# Makefile for development use

VERSION := $(shell sed -n 3p DESCRIPTION | sed 's/Version: //' | cat)
BINARY := dgo_$(VERSION).tar.gz

all: build check install readme doc

build: 
	R --no-site-file --no-environ  \
	  --no-save --no-restore --quiet CMD build .  \
	  --no-resave-data --no-manual

check: 
	R CMD CHECK dgo_$(VERSION).tar.gz

check-cran: 
	R CMD CHECK --as-cran dgo_$(VERSION).tar.gz

install: dgo_$(VERSION).tar.gz
	R CMD INSTALL --no-multiarch --with-keep.source dgo_$(VERSION).tar.gz

install-code:
	R CMD INSTALL --no-multiarch --with-keep.source --no-docs .

readme: dgo_$(VERSION).tar.gz
	R --vanilla --slave -e "rmarkdown::render('README.Rmd')"

doc: dgo_$(VERSION).tar.gz
	R CMD CHECK --no-install --no-tests --no-examples dgo_$(VERSION).tar.gz

quick-install:
	R CMD INSTALL --no-multiarch --no-docs --no-html \
	  --with-keep.source .

quick-check dgo_$(VERSION).tar.gz:
	R --no-site-file --no-environ  \
	  --no-save --no-restore --quiet CMD build .  \
	  --no-resave-data --no-manual
	R CMD CHECK dgo_$(VERSION).tar.gz
