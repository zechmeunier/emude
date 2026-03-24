# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(JuliaCall)
library(emude)

# Wrap Julia setup so R CMD check doesn't abort on machines without Julia.
# Mock-based tests (test-*_mock.R) do not need Julia and will always run.
# Integration tests (test-integration.R) use julia_is_available() to skip
# themselves when this setup fails.
tryCatch(
  {
    julia_setup()
    emude_setup()
  },
  error = function(e) {
    message("Julia setup failed — integration tests will be skipped: ",
            conditionMessage(e))
  }
)

test_check("emude")
