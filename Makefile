# Makefile for development use

VERSION := $(shell sed -n 3p DESCRIPTION | sed 's/Version: //' | cat)

all: build check install readme

build: 
	R --no-site-file --no-environ  \
	  --no-save --no-restore --quiet CMD build .  \
	  --no-resave-data --no-manual

check: dgirt_$(VERSION).tar.gz
	R CMD CHECK --as-cran dgirt_$(VERSION).tar.gz

install: dgirt_$(VERSION).tar.gz
	R CMD INSTALL --no-multiarch --with-keep.source dgirt_$(VERSION).tar.gz

readme: dgirt_$(VERSION).tar.gz
	R --vanilla --slave -e "rmarkdown::render('README.Rmd')"

dir-install:
	R CMD INSTALL --no-multiarch --with-keep.source .

doc:
	R CMD CHECK --no-install --no-tests --no-examples dgirt_$(VERSION).tar.gz
