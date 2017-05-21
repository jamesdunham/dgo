## Test environments

* local Ubuntu 16.04, R 3.4.0
* Ubuntu 12.04 (on travis-ci), R 3.4.0 (devel and release)
* Windows Server 2012 R2 x64 (on appveyor), R 3.4.0 (devel and release)


## R CMD check results

There were no ERRORs or WARNINGs. 

There were a few NOTEs:

* "installed size is 5.8Mb / sub-directories of 1Mb or more: libs 3.8Mb": The
  file in the 'libs' directory provides Stan models compiled during package
  installation. This follows the practice of existing CRAN package 'rstanarm'.
  It avoids expensive recompilation of the models in every R session.

* "New submission": This is the first submission with this maintainer email.

