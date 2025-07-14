#' Utility Functions for searchAnalyzeR Package
#'
#' This file contains general utility functions used throughout the package.
#' @importFrom R6 R6Class

#' Check Package Dependencies
#'
#' @param required_packages Character vector of required package names
#' @param install_missing Logical, whether to install missing packages
#' @return Logical vector indicating which packages are available
#' @details
#' This function checks if required packages are installed and optionally
#' installs missing packages. It uses \code{requireNamespace} to check
#' availability without loading packages.
#' @examples
#' # Check if packages are available
#' required <- c("dplyr", "ggplot2", "nonexistent_package")
#' availability <- check_deps(required, install_missing = FALSE)
#' print(availability)
#'
#' # Check and install missing packages (use with caution)
#' \dontrun{
#' check_deps(required, install_missing = TRUE)
#' }
#' @importFrom stats setNames
#' @importFrom utils install.packages
#' @export
check_deps <- function(required_packages, install_missing = FALSE) {
  # If no packages were requested, immediately return a logical(0)
  if (length(required_packages) == 0) {
    return(stats::setNames(logical(0), character(0)))
  }

  # Always get a logical vector back, even length‐0, by using vapply()
  available <- vapply(
    required_packages,
    function(pkg) requireNamespace(pkg, quietly = TRUE),
    logical(1)
  )

  if (any(!available)) {
    missing_packages <- required_packages[!available]

    if (install_missing) {
      message("Installing missing packages: ",
              paste(missing_packages, collapse = ", "))
      utils::install.packages(missing_packages)

      # Re-check availability, again via vapply()
      available <- vapply(
        required_packages,
        function(pkg) requireNamespace(pkg, quietly = TRUE),
        logical(1)
      )
    } else {
      warning("Missing required packages: ",
              paste(missing_packages, collapse = ", "))
    }
  }

  names(available) <- required_packages
  available
}

#' Validate Search Strategy Object
#'
#' @param search_strategy Search strategy object to validate
#' @return Logical indicating if valid, with warnings for issues
#' @export
validate_strategy <- function(search_strategy) {
  if (!is.list(search_strategy)) {
    stop("Search strategy must be a list")
  }

  required_fields <- c("terms", "databases")
  missing_fields <- setdiff(required_fields, names(search_strategy))

  if (length(missing_fields) > 0) {
    stop("Missing required fields in search strategy: ", paste(missing_fields, collapse = ", "))
  }

  # Check terms
  if (!is.character(search_strategy$terms) || length(search_strategy$terms) == 0) {
    stop("Search terms must be a non-empty character vector")
  }

  # Check databases
  if (!is.character(search_strategy$databases) || length(search_strategy$databases) == 0) {
    stop("Databases must be a non-empty character vector")
  }

  # Optional field validations
  if ("date_range" %in% names(search_strategy)) {
    if (!inherits(search_strategy$date_range, "Date") || length(search_strategy$date_range) != 2) {
      warning("date_range should be a Date vector of length 2")
    }
  }

  if ("filters" %in% names(search_strategy)) {
    if (!is.list(search_strategy$filters)) {
      warning("filters should be a list")
    }
  }

  return(TRUE)
}

#' Create Default Search Strategy Template
#'
#' @param terms Character vector of search terms
#' @param databases Character vector of databases
#' @param date_range Date vector of length 2 (start, end)
#' @param filters List of additional filters
#' @return Search strategy list
#' @export
create_strategy <- function(terms, databases, date_range = NULL, filters = NULL) {
  strategy <- list(
    terms = terms,
    databases = databases,
    timestamp = Sys.time()
  )

  if (!is.null(date_range)) {
    strategy$date_range <- date_range
  }

  if (!is.null(filters)) {
    strategy$filters <- filters
  }

  class(strategy) <- "search_strategy"
  return(strategy)
}

#' Calculate Text Similarity
#'
#' @param text1 First text string
#' @param text2 Second text string
#' @param method Similarity method ("jaccard", "cosine", "jaro_winkler")
#' @return Similarity score between 0 and 1
#' @importFrom stringr str_remove_all str_split str_count
#' @importFrom purrr map_int
#' @export
calc_text_sim <- function(text1, text2, method = "jaccard") {
  if (is.na(text1) || is.na(text2) || text1 == "" || text2 == "") {
    return(0)
  }

  # Clean and tokenize text
  clean_text1 <- tolower(stringr::str_remove_all(text1, "[^a-z0-9 ]"))
  clean_text2 <- tolower(stringr::str_remove_all(text2, "[^a-z0-9 ]"))

  switch(method,
         "jaccard" = calc_jaccard(clean_text1, clean_text2),
         "cosine" = calc_cosine(clean_text1, clean_text2),
         "jaro_winkler" = 1 - stringdist::stringdist(clean_text1, clean_text2, method = "jw"),
         stop("Unsupported similarity method: ", method)
  )
}

#' Calculate Jaccard Similarity
#'
#' @param text1 First text string
#' @param text2 Second text string
#' @return Jaccard similarity score
#' @importFrom stringr str_split
calc_jaccard <- function(text1, text2) {
  # Handle empty strings - should return 0, not 1
  if (text1 == "" && text2 == "") {
    return(0)
  }

  if (text1 == "" || text2 == "") {
    return(0)
  }

  tokens1 <- unique(stringr::str_split(text1, "\\s+")[[1]])
  tokens2 <- unique(stringr::str_split(text2, "\\s+")[[1]])

  # Remove empty tokens that might result from splitting empty strings
  tokens1 <- tokens1[tokens1 != ""]
  tokens2 <- tokens2[tokens2 != ""]

  # If both have no valid tokens after cleaning, return 0
  if (length(tokens1) == 0 && length(tokens2) == 0) {
    return(0)
  }

  # If one has no tokens and the other has tokens, return 0
  if (length(tokens1) == 0 || length(tokens2) == 0) {
    return(0)
  }

  intersection <- length(intersect(tokens1, tokens2))
  union <- length(union(tokens1, tokens2))

  if (union == 0) return(0)
  return(intersection / union)
}

#' Calculate Cosine Similarity
#'
#' @param text1 First text string
#' @param text2 Second text string
#' @return Cosine similarity score
#' @importFrom stringr str_split str_count
#' @importFrom purrr map_int
calc_cosine <- function(text1, text2) {
  # Handle empty strings gracefully
  if (is.na(text1) || is.na(text2) || text1 == "" || text2 == "") {
    return(0)
  }

  # Split into tokens
  tokens1 <- stringr::str_split(text1, "\\s+")[[1]]
  tokens2 <- stringr::str_split(text2, "\\s+")[[1]]

  # Remove empty tokens
  tokens1 <- tokens1[tokens1 != ""]
  tokens2 <- tokens2[tokens2 != ""]

  # If no valid tokens, return 0
  if (length(tokens1) == 0 || length(tokens2) == 0) {
    return(0)
  }

  # Create term frequency vectors
  all_terms <- unique(c(tokens1, tokens2))

  # Handle case where no terms exist
  if (length(all_terms) == 0) {
    return(0)
  }

  vec1 <- purrr::map_int(all_terms, ~sum(stringr::str_count(text1, fixed(.x))))
  vec2 <- purrr::map_int(all_terms, ~sum(stringr::str_count(text2, fixed(.x))))

  # Calculate cosine similarity
  dot_product <- sum(vec1 * vec2)
  magnitude1 <- sqrt(sum(vec1^2))
  magnitude2 <- sqrt(sum(vec2^2))

  if (magnitude1 == 0 || magnitude2 == 0) return(0)
  return(dot_product / (magnitude1 * magnitude2))
}

#' Generate Reproducible Random Seed
#'
#' @param base_string Base string for seed generation
#' @return Integer seed value
#' @details
#' This function generates a reproducible seed based on a string input.
#' It does not set the seed automatically - users should call set.seed()
#' themselves if they want to use the generated seed.
#' @examples
#' # Generate a seed value
#' seed_value <- gen_repro_seed("my_analysis")
#'
#' # User can choose to set it
#' \donttest{
#' set.seed(seed_value)
#' sample(1:10, 3)
#' }
#' @export
gen_repro_seed <- function(base_string = "searchAnalyzeR") {
  # Create reproducible seed from string
  seed <- sum(utf8ToInt(base_string)) %% .Machine$integer.max
  return(as.integer(seed))
}

#' Safe Division Function
#'
#' @param numerator Numerator value
#' @param denominator Denominator value
#' @param default_value Value to return if denominator is 0
#' @return Division result or default value
safe_divide <- function(numerator, denominator, default_value = 0) {
  ifelse(denominator == 0 | is.na(denominator), default_value, numerator / denominator)
}

#' Format Numbers for Display
#'
#' @param x Numeric vector
#' @param digits Number of decimal places
#' @param percent Logical, whether to format as percentage
#' @return Formatted character vector
#' @export
format_numbers <- function(x, digits = 3, percent = FALSE) {
  if (percent) {
    paste0(round(x * 100, digits), "%")
  } else {
    round(x, digits)
  }
}

#' Check if Object is Empty
#'
#' @param x Object to check
#' @return Logical indicating if object is empty
is_empty <- function(x) {
  if (is.null(x)) return(TRUE)
  if (length(x) == 0) return(TRUE)

  # For character vectors, check different conditions
  if (is.character(x)) {
    # If ALL elements are NA, return TRUE
    if (all(is.na(x))) return(TRUE)

    # Otherwise return FALSE (empty strings should be considered non-empty)
    return(FALSE)
  }

  # For other types, check if all are NA
  if (all(is.na(x))) return(TRUE)

  if (is.data.frame(x) && nrow(x) == 0) return(TRUE)
  return(FALSE)
}

#' Convert List to Data Frame Safely
#'
#' @param x List to convert
#' @return Data frame or NULL if conversion fails
#' @importFrom purrr map_int
#' @importFrom tibble tibble
safe_list_to_df <- function(x) {
  tryCatch({
    if (is.list(x) && length(x) > 0) {
      # Check if all elements have the same length
      lengths <- purrr::map_int(x, length)
      if (length(unique(lengths)) == 1) {
        return(as.data.frame(x, stringsAsFactors = FALSE))
      } else {
        # Try to convert to tibble which handles different lengths
        return(tibble::tibble(!!!x))
      }
    }
    return(NULL)
  }, error = function(e) {
    warning("Could not convert list to data frame: ", e$message)
    return(NULL)
  })
}

#' Extract Package Version Information
#'
#' @param packages Character vector of package names
#' @return Data frame with package version information
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @export
get_pkg_versions <- function(packages = c("searchAnalyzeR", "dplyr", "ggplot2", "stringr")) {
  purrr::map_dfr(packages, function(pkg) {
    tryCatch({
      version <- packageVersion(pkg)
      tibble::tibble(
        package = pkg,
        version = as.character(version),
        available = TRUE
      )
    }, error = function(e) {
      tibble::tibble(
        package = pkg,
        version = NA_character_,
        available = FALSE
      )
    })
  })
}

#' Create Progress Bar for Long Operations
#'
#' @param total Total number of iterations
#' @param format Progress bar format string
#' @return Progress bar object
#' @export
create_progress_bar <- function(total, format = "[:bar] :percent :elapsed") {
  if (requireNamespace("progress", quietly = TRUE)) {
    progress::progress_bar$new(
      format = format,
      total = total,
      clear = FALSE,
      width = 60
    )
  } else {
    # Simple fallback progress indicator
    list(
      total = total,
      current = 0,
      tick = function() {
        self$current <- self$current + 1
        if (self$current %% max(1, floor(self$total / 10)) == 0) {
          cat("Progress:", round(self$current / self$total * 100), "%\n")
        }
      }
    )
  }
}

#' Validate Date Range
#'
#' @param date_range Date vector of length 2
#' @param allow_future Logical, whether future dates are allowed
#' @return Logical indicating if valid
validate_date_range <- function(date_range, allow_future = TRUE) {
  if (!inherits(date_range, "Date") || length(date_range) != 2) {
    return(FALSE)
  }

  if (is.na(date_range[1]) || is.na(date_range[2])) {
    return(FALSE)
  }

  if (date_range[1] > date_range[2]) {
    return(FALSE)
  }

  if (!allow_future && any(date_range > Sys.Date())) {
    return(FALSE)
  }

  return(TRUE)
}

#' Clean Column Names
#'
#' @param names Character vector of column names
#' @return Cleaned column names
#' @importFrom stringr str_replace_all str_remove
#' @export
clean_col_names <- function(names) {
  names %>%
    # Convert to lowercase
    tolower() %>%
    # Replace spaces and special characters with underscores
    stringr::str_replace_all("[^a-z0-9]", "_") %>%
    # Remove multiple consecutive underscores
    stringr::str_replace_all("_{2,}", "_") %>%
    # Remove leading/trailing underscores
    stringr::str_remove("^_+|_+$") %>%
    # Ensure names don't start with numbers
    stringr::str_replace("^([0-9])", "x\\1")
}

#' Calculate Confidence Intervals
#'
#' @param x Numeric vector
#' @param conf_level Confidence level (0-1)
#' @param method Method for calculation ("normal", "bootstrap")
#' @return List with lower and upper bounds
#' @importFrom stats sd qnorm
#' @export
calc_ci <- function(x, conf_level = 0.95, method = "normal") {
  if (length(x) < 2) {
    return(list(lower = NA, upper = NA))
  }

  alpha <- 1 - conf_level

  if (method == "normal") {
    mean_x <- mean(x, na.rm = TRUE)
    se_x <- stats::sd(x, na.rm = TRUE) / sqrt(length(x))

    list(
      lower = mean_x - stats::qnorm(1 - alpha/2) * se_x,
      upper = mean_x + stats::qnorm(1 - alpha/2) * se_x
    )
  } else if (method == "bootstrap") {
    if (requireNamespace("boot", quietly = TRUE)) {
      boot_result <- boot::boot(x, function(data, indices) mean(data[indices]), R = 1000)
      boot_ci <- boot::boot.ci(boot_result, conf = conf_level, type = "perc")

      list(
        lower = boot_ci$percent[4],
        upper = boot_ci$percent[5]
      )
    } else {
      warning("boot package not available, using normal method")
      calc_ci(x, conf_level, "normal")
    }
  } else {
    stop("Unsupported method: ", method)
  }
}

#' Create Summary Statistics Table
#'
#' @param data Data frame
#' @param numeric_vars Character vector of numeric variable names
#' @param categorical_vars Character vector of categorical variable names
#' @return Summary statistics data frame
#' @importFrom stats median quantile sd
#' @export
create_summary <- function(data,
                                 numeric_vars = NULL,
                                 categorical_vars = NULL) {
  # Identify numeric columns
  if (is.null(numeric_vars)) {
    numeric_vars <- names(data)[
      vapply(data, is.numeric, logical(1))
    ]
  }

  # Identify categorical columns
  if (is.null(categorical_vars)) {
    categorical_vars <- names(data)[
      vapply(data, function(x) is.character(x) || is.factor(x), logical(1))
    ]
  }

  # Build numeric_summary or empty data.frame
  numeric_list <- lapply(numeric_vars, function(var) {
    if (!var %in% names(data)) return(NULL)
    x <- data[[var]]
    data.frame(
      variable = var,
      type     = "numeric",
      n        = sum(!is.na(x)),
      missing  = sum(is.na(x)),
      mean     = mean(x, na.rm = TRUE),
      sd       = stats::sd(x, na.rm = TRUE),
      min      = min(x, na.rm = TRUE),
      q25      = stats::quantile(x, 0.25, na.rm = TRUE),
      median   = stats::median(x, na.rm = TRUE),
      q75      = stats::quantile(x, 0.75, na.rm = TRUE),
      max      = max(x, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  if (length(numeric_list) > 0) {
    numeric_summary <- do.call(rbind, numeric_list)
  } else {
    numeric_summary <- data.frame()
  }

  # Build categorical_summary or empty data.frame
  categorical_list <- lapply(categorical_vars, function(var) {
    if (!var %in% names(data)) return(NULL)
    x <- data[[var]]
    tbl <- sort(table(x, useNA = "no"), decreasing = TRUE)
    top_vals <- names(tbl)[seq_len(min(3, length(tbl)))]
    data.frame(
      variable      = var,
      type          = "categorical",
      n             = sum(!is.na(x)),
      missing       = sum(is.na(x)),
      unique_values = length(unique(x[!is.na(x)])),
      top_values    = paste(top_vals, collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
  if (length(categorical_list) > 0) {
    categorical_summary <- do.call(rbind, categorical_list)
  } else {
    categorical_summary <- data.frame()
  }

  # Combine
  nr <- nrow(numeric_summary)
  cr <- nrow(categorical_summary)

  if (nr > 0 && cr > 0) {
    # pad columns
    categorical_summary[, c("mean","sd","min","q25","median","q75","max")] <- NA
    numeric_summary[, c("unique_values","top_values")] <- NA
    return(rbind(numeric_summary, categorical_summary))
  } else if (nr > 0) {
    return(numeric_summary)
  } else if (cr > 0) {
    return(categorical_summary)
  } else {
    return(data.frame())
  }
}
