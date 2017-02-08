## Test environments

* local Ubuntu 16.04, R 3.3.2 (devel and release)
* Ubuntu 12.04 (on travis-ci), R 3.3.2
* Windows Server 2012 R2 x64 (on appveyor), R 3.3.2

## R CMD check results

There were no ERRORs or WARNINGs. 

There were a few NOTEs:

* "Strong dependencies not in mainstream repositories: dgodata": The dgodata
  package contains datasets that appear in dgo examples and will be useful to
  dgo users. We've submitted it to CRAN along with dgo. It's meanwhile available
  at <https://github.com/jamesdunham/dgodata>.

* "installed size is 5.8Mb / sub-directories of 1Mb or more: libs 3.8Mb": The
  file in the 'libs' directory provides Stan models compiled during package
  installation. This follows the practice of existing CRAN package 'rstanarm'. 

* "New submission": This is the first submission with this maintainer email.

