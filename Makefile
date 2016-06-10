# Makefile for development use

VERSION := $(shell sed -n 3p DESCRIPTION | sed 's/Version: //' | cat)

all: build check-build install-build readme doc

build: 
	R --no-site-file --no-environ  \
	  --no-save --no-restore --quiet CMD build .  \
	  --no-resave-data --no-manual

check-build: dgirt_$(VERSION).tar.gz
	R CMD CHECK --as-cran dgirt_$(VERSION).tar.gz

install-build: dgirt_$(VERSION).tar.gz
	R CMD INSTALL --no-multiarch --with-keep.source dgirt_$(VERSION).tar.gz

install:
	R CMD INSTALL --no-multiarch --with-keep.source --no-docs .

readme: dgirt_$(VERSION).tar.gz
	R --vanilla --slave -e "rmarkdown::render('README.Rmd')"

doc: dgirt_$(VERSION).tar.gz
	R CMD CHECK --no-install --no-tests --no-examples dgirt_$(VERSION).tar.gz

quick-install:
	R CMD INSTALL --no-multiarch --no-docs --no-html \
	  --with-keep.source .

quick-check dgirt_$(VERSION).tar.gz:
	R --no-site-file --no-environ  \
	  --no-save --no-restore --quiet CMD build .  \
	  --no-resave-data --no-manual
	R CMD CHECK dgirt_$(VERSION).tar.gz
