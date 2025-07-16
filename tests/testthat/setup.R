# Test setup file for searchAnalyzeR package
# This file is sourced before running tests

# Load required libraries
library(testthat)
library(dplyr)
library(ggplot2)
library(lubridate)

# Suppress warnings during testing
options(warn = -1)

# Set up consistent testing environment
options(stringsAsFactors = FALSE)
set.seed(42)  # For reproducible random data in tests

# Handle function conflicts
# Store base R is_empty function (avoid purrr dependency)
base_is_empty <- function(x) {
  if (is.null(x)) return(TRUE)
  if (length(x) == 0) return(TRUE)
  if (is.data.frame(x) && nrow(x) == 0) return(TRUE)
  if (is.character(x) && all(is.na(x))) return(TRUE)
  return(FALSE)
}

# Create test-specific temporary directory
test_temp_dir <- file.path(tempdir(), "searchanalyzer_tests")
if (!dir.exists(test_temp_dir)) {
  dir.create(test_temp_dir, recursive = TRUE)
}

# Global test configuration
TEST_CONFIG <- list(
  temp_dir = test_temp_dir,
  small_dataset_size = 20,
  medium_dataset_size = 100,
  large_dataset_size = 1000,
  default_timeout = 30,  # seconds
  precision_tolerance = 0.001
)

# Function to handle expect_no_error compatibility
# For compatibility with different testthat versions
expect_no_error <- function(object) {
  # Try to use the native expect_no_error if available
  if (exists("expect_no_error", where = asNamespace("testthat"), inherits = FALSE)) {
    # Use testthat's expect_no_error without extra arguments
    testthat::expect_no_error(object)
  } else {
    # Fallback for older versions - expect no error by expecting error with NA pattern
    testthat::expect_error(object, regexp = NA)
  }
}

# Alternative function for silent execution expectation
expect_silent_execution <- function(object) {
  testthat::expect_silent(object)
}

# Cleanup function to be called after all tests
cleanup_all_tests <- function() {
  if (dir.exists(TEST_CONFIG$temp_dir)) {
    unlink(TEST_CONFIG$temp_dir, recursive = TRUE)
  }
}

# Register cleanup to run when R session ends
reg.finalizer(environment(), function(e) cleanup_all_tests(), onexit = TRUE)
