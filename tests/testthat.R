library(testthat)
library(dgo)

# https://stackoverflow.com/questions/12410694/rbundler-build-error-cannot-open-file-startup-rs-no-such-file-or-directory#27994299
Sys.setenv("R_TESTS" = "")
test_check("dgo")
