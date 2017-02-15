# Makefile for dgo development 

VERSION := $(shell sed -n 3p DESCRIPTION | sed 's/Version: //' | cat)
BINARY := dgo_$(VERSION).tar.gz

all: clean build check install readme doc

clean:
	rm -f src/*.so src/*.o

build: 
	R --no-site-file --no-environ  \
	  --no-save --no-restore --quiet CMD build .  \
	  --no-resave-data --no-manual
	
build-devel: 
	R-devel.sh --no-site-file --no-environ  \
	  --no-save --no-restore --quiet CMD build .  \
	  --no-resave-data --no-manual

check: 
	R CMD check dgo_$(VERSION).tar.gz

check-devel:
	R-devel.sh CMD check dgo_$(VERSION).tar.gz

check-cran: 
	R CMD check --as-cran dgo_$(VERSION).tar.gz

check-cran-devel: 
	R-devel.sh CMD check --as-cran dgo_$(VERSION).tar.gz

install: dgo_$(VERSION).tar.gz
	R CMD INSTALL --no-multiarch --with-keep.source dgo_$(VERSION).tar.gz

install-devel: dgo_$(VERSION).tar.gz
	R-devel.sh CMD INSTALL --no-multiarch --with-keep.source dgo_$(VERSION).tar.gz

install-code:
	R CMD INSTALL --no-multiarch --with-keep.source --no-docs .

readme: dgo_$(VERSION).tar.gz
	R --vanilla --slave -e "rmarkdown::render('README.Rmd')"

doc: dgo_$(VERSION).tar.gz
	R CMD check --no-install --no-tests --no-examples dgo_$(VERSION).tar.gz

quick-install:
	R CMD INSTALL --no-multiarch --no-docs --no-html \
	  --with-keep.source .

quick-check dgo_$(VERSION).tar.gz:
	R --no-site-file --no-environ  \
	  --no-save --no-restore --quiet CMD build .  \
	  --no-resave-data --no-manual
	R CMD check dgo_$(VERSION).tar.gz

