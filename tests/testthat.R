library(testthat)
library(Rnightlights)

## force tests to be executed if in dev release which we define as
## having a sub-release, eg 0.9.15.5 is one whereas 0.9.16 is not
if (length(strsplit(packageDescription("Rnightlights")$Version, "\\.")[[1]]) > 3) {	# dev release, and
  if (Sys.getenv("RunAllTests") != "no") { 				# if env.var not explicitly set to no
    message("Setting \"RunAllTests\"=\"yes\" for development release\n")
    Sys.setenv("RunAllTests"="yes")
  }
}

test_check("Rnightlights")
