## Test environments

* local Ubuntu 16.04, R 3.4.0
* Ubuntu 12.04 (on travis-ci), R 3.4.0 (devel, release) and R 3.3.3
* OS X 10.11 and 10.12 (on travis-ci), R 3.4.0 (devel, release) and R 3.3.3
* Windows Server 2012 R2 x64 (on appveyor), R 3.4.0 (devel and release) and R
  3.3.3


## R CMD check results

There were no ERRORs or WARNINGs. 

There was one NOTE:

* "Days since last update: 6": This submission attempts to resolve a build
  failure (only) on
  [r-release-osx-x86_64](https://www.r-project.org/nosvn/R.check/r-release-osx-x86_64/dgo-00check.html)
  resulting from a failing test, which does not occur on travis-ci. This
  submission avoids using dependency RStan during tests and removes a dependency
  on Rcpp. The goal of these changes is to render moot variation in build
  environments.
