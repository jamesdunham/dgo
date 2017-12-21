## Test environments

* Ubuntu 14.04.5 LTS (on travis-ci), R 3.4.2 and R 3.3.3
* macOS 10.12.6 and 10.11.6 (on travis-ci), R 3.4.3 and R 3.3.3
* Windows Server 2012 R2 x64 (on appveyor), R 3.4.3 (release) i386 and x64
* Fedora 27 (local), R 3.4.3 and R-devel 2017-12-20 r73933 with --disable-long-double

## R CMD check results

There were no ERRORs or WARNINGs; there was one NOTE. This submission resolves a
failing noLD check. Confirmed using a local build of R-devel with
--disable-long-double.
