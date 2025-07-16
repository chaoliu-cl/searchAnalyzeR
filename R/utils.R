#' Utility Functions for searchAnalyzeR Package
#'
#' This file contains general utility functions used throughout the package.
#' @importFrom R6 R6Class
#' @importFrom stats aggregate reorder reshape
#' @importFrom utils head object.size packageVersion write.csv

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
#' @export
calc_text_sim <- function(text1, text2, method = "jaccard") {
  if (is.na(text1) || is.na(text2) || text1 == "" || text2 == "") {
    return(0)
  }

  # Clean and tokenize text
  clean_text1 <- tolower(gsub("[^a-z0-9 ]", "", text1))
  clean_text2 <- tolower(gsub("[^a-z0-9 ]", "", text2))

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
calc_jaccard <- function(text1, text2) {
  # Handle empty strings - should return 0, not 1
  if (text1 == "" && text2 == "") {
    return(0)
  }

  if (text1 == "" || text2 == "") {
    return(0)
  }

  tokens1 <- unique(strsplit(text1, "\\s+")[[1]])
  tokens2 <- unique(strsplit(text2, "\\s+")[[1]])

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
calc_cosine <- function(text1, text2) {
  # Handle empty strings gracefully
  if (is.na(text1) || is.na(text2) || text1 == "" || text2 == "") {
    return(0)
  }

  # Split into tokens
  tokens1 <- strsplit(text1, "\\s+")[[1]]
  tokens2 <- strsplit(text2, "\\s+")[[1]]

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

  # Base R version of map_int with str_count
  vec1 <- sapply(all_terms, function(term) sum(gregexpr(term, text1, fixed = TRUE)[[1]] > 0))
  vec2 <- sapply(all_terms, function(term) sum(gregexpr(term, text2, fixed = TRUE)[[1]] > 0))

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
  char_codes <- utf8ToInt(base_string)
  seed <- sum(char_codes) %% .Machine$integer.max
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
safe_list_to_df <- function(x) {
  tryCatch({
    if (is.list(x) && length(x) > 0) {
      # Check if all elements have the same length
      lengths <- sapply(x, length)
      if (length(unique(lengths)) == 1) {
        return(as.data.frame(x, stringsAsFactors = FALSE))
      } else {
        # Create a data frame with different column lengths
        result <- data.frame(stringsAsFactors = FALSE)
        for (col_name in names(x)) {
          result[[col_name]] <- x[[col_name]]
        }
        return(result)
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
#' @export
get_pkg_versions <- function(packages = c("searchAnalyzeR", "ggplot2", "lubridate", "openxlsx")) {
  result <- data.frame(
    package = character(),
    version = character(),
    available = logical(),
    stringsAsFactors = FALSE
  )

  for (pkg in packages) {
    tryCatch({
      version <- utils::packageVersion(pkg)
      result <- rbind(result, data.frame(
        package = pkg,
        version = as.character(version),
        available = TRUE,
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      result <- rbind(result, data.frame(
        package = pkg,
        version = NA_character_,
        available = FALSE,
        stringsAsFactors = FALSE
      ))
    })
  }

  return(result)
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
        env <- parent.env(environment())
        env$current <- env$current + 1
        if (env$current %% max(1, floor(env$total / 10)) == 0) {
          cat("Progress:", round(env$current / env$total * 100), "%\n")
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
#' @export
clean_col_names <- function(names) {
  # Convert to lowercase
  names <- tolower(names)

  # Replace spaces and special characters with underscores
  names <- gsub("[^a-z0-9]", "_", names)

  # Remove multiple consecutive underscores
  names <- gsub("_{2,}", "_", names)

  # Remove leading/trailing underscores
  names <- gsub("^_+|_+$", "", names)

  # Ensure names don't start with numbers
  names <- gsub("^([0-9])", "x\\1", names)

  return(names)
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
    numeric_vars <- names(data)[sapply(data, is.numeric)]
  }

  # Identify categorical columns
  if (is.null(categorical_vars)) {
    categorical_vars <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
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

#' Analyze Term Effectiveness in Search Results
#'
#' @description
#' Analyzes the effectiveness of individual search terms by calculating precision,
#' coverage, and other relevant metrics for each term. This provides insight into
#' which terms are most effective at retrieving relevant articles.
#'
#' @param terms Character vector of search terms to analyze
#' @param search_results Data frame with search results
#' @param gold_standard Optional vector of relevant article IDs
#' @param text_fields Character vector of column names to search for terms (default: c("title", "abstract"))
#' @return Data frame with term effectiveness metrics
#'
#' @details
#' For each term, this function calculates:
#' \itemize{
#'   \item Number of articles containing the term
#'   \item Number of relevant articles containing the term (if gold_standard provided)
#'   \item Precision (proportion of retrieved articles that are relevant)
#'   \item Coverage (proportion of relevant articles retrieved by the term)
#' }
#'
#' @examples
#' # Create sample data
#' search_results <- data.frame(
#'   id = paste0("art", 1:10),
#'   title = c("Diabetes treatment", "Clinical trial", "Diabetes study",
#'             "Treatment options", "New therapy", "Glucose control",
#'             "Insulin therapy", "Management of diabetes", "Clinical study",
#'             "Therapy comparison"),
#'   abstract = c("This study examines diabetes treatments.",
#'                "A clinical trial on new treatments.",
#'                "Diabetes research findings.",
#'                "Comparison of treatment options.",
#'                "Novel therapy approach.",
#'                "Methods to control glucose levels.",
#'                "Insulin therapy effectiveness.",
#'                "Managing diabetes effectively.",
#'                "Clinical research protocols.",
#'                "Comparing therapy approaches.")
#' )
#'
#' # Define search terms
#' terms <- c("diabetes", "treatment", "clinical", "therapy")
#'
#' # Define gold standard (relevant articles)
#' gold_standard <- c("art1", "art3", "art7", "art8")
#'
#' # Analyze term effectiveness
#' term_metrics <- term_effectiveness(terms, search_results, gold_standard)
#' print(term_metrics)
#'
#' @export
term_effectiveness <- function(terms, search_results, gold_standard = NULL,
                               text_fields = c("title", "abstract")) {

  # Validate input
  if (!is.character(terms) || length(terms) == 0) {
    stop("Terms must be a non-empty character vector")
  }

  if (!is.data.frame(search_results) || nrow(search_results) == 0) {
    stop("Search results must be a non-empty data frame")
  }

  # Ensure all text fields exist in search_results
  missing_fields <- setdiff(text_fields, names(search_results))
  if (length(missing_fields) > 0) {
    warning("The following text fields are missing: ", paste(missing_fields, collapse = ", "),
            ". Using available fields only.")
    text_fields <- intersect(text_fields, names(search_results))
    if (length(text_fields) == 0) {
      stop("None of the specified text fields exist in search_results")
    }
  }

  # Check if ID field exists
  if (!"id" %in% names(search_results)) {
    stop("Search results must contain an 'id' column")
  }

  # Initialize results data frame
  term_analysis <- data.frame(
    term = terms,
    articles_with_term = numeric(length(terms)),
    relevant_with_term = numeric(length(terms)),
    precision = numeric(length(terms)),
    coverage = numeric(length(terms)),
    stringsAsFactors = FALSE
  )

  # Calculate total number of relevant articles if gold standard is provided
  total_relevant <- 0
  if (!is.null(gold_standard)) {
    # Make sure gold_standard exists in search_results
    valid_gold <- gold_standard[gold_standard %in% search_results$id]
    if (length(valid_gold) == 0) {
      warning("None of the gold standard IDs were found in search_results")
    }
    total_relevant <- length(valid_gold)
  }

  # Prepare text to search for each article
  search_text <- character(nrow(search_results))
  for (field in text_fields) {
    # Combine fields with a space separator, handling NAs
    field_values <- search_results[[field]]
    field_values[is.na(field_values)] <- ""
    search_text <- paste(search_text, field_values, sep = " ")
  }
  search_text <- tolower(search_text)

  # Analyze each term
  for (i in seq_along(terms)) {
    term <- terms[i]
    term_lower <- tolower(term)

    # Find articles containing this term
    articles_with_term <- grepl(term_lower, search_text, fixed = TRUE)
    articles_with_term_ids <- search_results$id[articles_with_term]
    term_analysis$articles_with_term[i] <- sum(articles_with_term)

    # If gold standard is provided, calculate precision and coverage
    if (!is.null(gold_standard) && total_relevant > 0) {
      # Find relevant articles containing this term
      relevant_with_term <- sum(articles_with_term_ids %in% gold_standard)
      term_analysis$relevant_with_term[i] <- relevant_with_term

      # Calculate precision (if any articles found)
      if (term_analysis$articles_with_term[i] > 0) {
        term_analysis$precision[i] <- relevant_with_term / term_analysis$articles_with_term[i]
      } else {
        term_analysis$precision[i] <- 0
      }

      # Calculate coverage
      term_analysis$coverage[i] <- relevant_with_term / total_relevant
    } else {
      term_analysis$relevant_with_term[i] <- NA
      term_analysis$precision[i] <- NA
      term_analysis$coverage[i] <- NA
    }
  }

  # Add metadata
  attr(term_analysis, "analysis_info") <- list(
    search_results_count = nrow(search_results),
    gold_standard_count = total_relevant,
    text_fields_analyzed = text_fields,
    timestamp = Sys.time()
  )

  class(term_analysis) <- c("term_effectiveness", "data.frame")
  return(term_analysis)
}

#' Print Method for term_effectiveness Objects
#'
#' @param x A term_effectiveness object
#' @param ... Further arguments passed to or from other methods
#' @return Invisibly returns the input object
#' @export
print.term_effectiveness <- function(x, ...) {
  cat("Term Effectiveness Analysis\n")
  cat("==========================\n")

  # Get analysis info
  info <- attr(x, "analysis_info")
  if (!is.null(info)) {
    cat("Search Results:", info$search_results_count, "articles\n")
    if (!is.null(info$gold_standard_count) && info$gold_standard_count > 0) {
      cat("Gold Standard:", info$gold_standard_count, "relevant articles\n")
    }
    cat("Fields Analyzed:", paste(info$text_fields_analyzed, collapse = ", "), "\n")
    cat("\n")
  }

  # Format the data for printing
  formatted <- x
  formatted$precision <- format(round(formatted$precision, 3), nsmall = 3)
  formatted$coverage <- format(round(formatted$coverage, 3), nsmall = 3)

  # Print the data
  print.data.frame(formatted, row.names = FALSE, ...)

  invisible(x)
}

#' Plot Term Effectiveness Results
#'
#' @param term_analysis Result from term_effectiveness function
#' @param plot_type Type of plot to create ("precision_coverage", "counts", "comparison", "precision_only", "coverage_only")
#' @param highlight_terms Optional character vector of terms to highlight
#' @param title_override Optional custom title for the plot
#' @param show_values Logical, whether to show values on bars/points (default: TRUE)
#' @return A ggplot object if ggplot2 is available, otherwise NULL with a message
#'
#' @details
#' This function creates visualizations of term effectiveness results with enhanced options
#' for creating individual, clean plots. New plot types include "precision_only" and
#' "coverage_only" for focused analysis.
#'
#' @examples
#' \dontrun{
#' # First analyze term effectiveness
#' term_metrics <- term_effectiveness(terms, search_results, gold_standard)
#'
#' # Create individual plots
#' precision_plot <- plot_term_effectiveness(term_metrics, "precision_only")
#' coverage_plot <- plot_term_effectiveness(term_metrics, "coverage_only")
#' bubble_plot <- plot_term_effectiveness(term_metrics, "precision_coverage")
#' }
#'
#' @export
plot_term_effectiveness <- function(term_analysis, plot_type = "precision_coverage",
                                    highlight_terms = NULL, title_override = NULL,
                                    show_values = TRUE) {

  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("The ggplot2 package is required for plotting. Install with: install.packages('ggplot2')")
    return(NULL)
  }

  # Make sure input is from term_effectiveness function
  if (!inherits(term_analysis, "term_effectiveness")) {
    stop("Input must be a result from the term_effectiveness function")
  }

  # If precision and coverage are not available, limit plot types
  if (all(is.na(term_analysis$precision)) &&
      plot_type %in% c("precision_coverage", "comparison", "precision_only", "coverage_only")) {
    warning("Precision and coverage not available. Switching to 'counts' plot type.")
    plot_type <- "counts"
  }

  # Add highlight column if highlight_terms is provided
  if (!is.null(highlight_terms)) {
    term_analysis$highlighted <- term_analysis$term %in% highlight_terms
  } else {
    term_analysis$highlighted <- FALSE
  }

  # Define consistent colors
  colors <- list(
    primary = "#2E86AB",
    secondary = "#A23B72",
    highlight = "#F18F01",
    neutral = "#E8E8E8"
  )

  # Generate appropriate plot based on plot_type
  if (plot_type == "precision_coverage") {
    title <- if (!is.null(title_override)) title_override else "Term Effectiveness: Precision vs Coverage"

    p <- ggplot2::ggplot(
      term_analysis,
      ggplot2::aes(
        x = .data[["coverage"]],
        y = .data[["precision"]],
        size = .data[["articles_with_term"]],
        color = .data[["highlighted"]]
      )
    ) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::scale_size_continuous(
        range = c(3, 12),
        name = "Articles\nRetrieved",
        guide = ggplot2::guide_legend(override.aes = list(alpha = 0.7))
      ) +
      ggplot2::scale_color_manual(
        values = c("FALSE" = colors$primary, "TRUE" = colors$secondary),
        name = "Highlighted",
        guide = if (any(term_analysis$highlighted)) "legend" else "none"
      ) +
      ggplot2::labs(
        title = title,
        subtitle = "Bubble size represents number of articles retrieved",
        x = "Coverage (Proportion of Relevant Articles Retrieved)",
        y = "Precision (Proportion of Retrieved Articles that are Relevant)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11, color = "gray60"),
        legend.position = "right",
        panel.grid.minor = ggplot2::element_blank()
      ) +
      ggplot2::xlim(0, 1) +
      ggplot2::ylim(0, 1)

    # Add term labels with better positioning
    if (show_values) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = .data[["term"]]),
        hjust = -0.1,
        vjust = 0.5,
        size = 3,
        nudge_x = 0.02,
        check_overlap = TRUE
      )
    }

  } else if (plot_type == "precision_only") {
    title <- if (!is.null(title_override)) title_override else "Term Precision Analysis"

    p <- ggplot2::ggplot(
      term_analysis,
      ggplot2::aes(
        x = stats::reorder(.data[["term"]], .data[["precision"]]),
        y = .data[["precision"]],
        fill = .data[["highlighted"]]
      )
    ) +
      ggplot2::geom_col(alpha = 0.8, width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = c("FALSE" = colors$primary, "TRUE" = colors$secondary),
        name = "Highlighted",
        guide = if (any(term_analysis$highlighted)) "legend" else "none"
      ) +
      ggplot2::labs(
        title = title,
        subtitle = "Proportion of retrieved articles that are relevant",
        x = "Search Term",
        y = "Precision (0 = No relevant articles, 1 = All articles relevant)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11, color = "gray60"),
        panel.grid.minor = ggplot2::element_blank()
      ) +
      ggplot2::ylim(0, 1)

    if (show_values) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.3f", .data[["precision"]])),
        hjust = -0.1,
        size = 3
      )
    }

  } else if (plot_type == "coverage_only") {
    title <- if (!is.null(title_override)) title_override else "Term Coverage Analysis"

    p <- ggplot2::ggplot(
      term_analysis,
      ggplot2::aes(
        x = stats::reorder(.data[["term"]], .data[["coverage"]]),
        y = .data[["coverage"]],
        fill = .data[["highlighted"]]
      )
    ) +
      ggplot2::geom_col(alpha = 0.8, width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = c("FALSE" = colors$secondary, "TRUE" = colors$highlight),
        name = "Highlighted",
        guide = if (any(term_analysis$highlighted)) "legend" else "none"
      ) +
      ggplot2::labs(
        title = title,
        subtitle = "Proportion of relevant articles retrieved by each term",
        x = "Search Term",
        y = "Coverage (0 = No relevant articles found, 1 = All relevant articles found)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11, color = "gray60"),
        panel.grid.minor = ggplot2::element_blank()
      ) +
      ggplot2::ylim(0, 1)

    if (show_values) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.3f", .data[["coverage"]])),
        hjust = -0.1,
        size = 3
      )
    }

  } else if (plot_type == "counts") {
    title <- if (!is.null(title_override)) title_override else "Term Retrieval Counts"

    p <- ggplot2::ggplot(
      term_analysis,
      ggplot2::aes(
        x = stats::reorder(.data[["term"]], .data[["articles_with_term"]]),
        y = .data[["articles_with_term"]],
        fill = .data[["highlighted"]]
      )
    ) +
      ggplot2::geom_col(alpha = 0.8, width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = c("FALSE" = colors$primary, "TRUE" = colors$secondary),
        name = "Highlighted",
        guide = if (any(term_analysis$highlighted)) "legend" else "none"
      ) +
      ggplot2::labs(
        title = title,
        subtitle = "Total number of articles retrieved by each search term",
        x = "Search Term",
        y = "Number of Articles Retrieved"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11, color = "gray60"),
        panel.grid.minor = ggplot2::element_blank()
      )

    if (show_values) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = .data[["articles_with_term"]]),
        hjust = -0.1,
        size = 3
      )
    }

  } else if (plot_type == "comparison") {
    title <- if (!is.null(title_override)) title_override else "Term Performance Comparison"

    # Create comparison of precision and coverage using base R approach
    # Convert to long format manually
    precision_data <- data.frame(
      term = term_analysis$term,
      metric = "precision",
      value = term_analysis$precision,
      highlighted = term_analysis$highlighted,
      stringsAsFactors = FALSE
    )

    coverage_data <- data.frame(
      term = term_analysis$term,
      metric = "coverage",
      value = term_analysis$coverage,
      highlighted = term_analysis$highlighted,
      stringsAsFactors = FALSE
    )

    long_data <- rbind(precision_data, coverage_data)

    p <- ggplot2::ggplot(
      long_data,
      ggplot2::aes(
        x = stats::reorder(.data[["term"]], .data[["value"]]),
        y = .data[["value"]],
        fill = .data[["metric"]]
      )
    ) +
      ggplot2::geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = c("precision" = colors$primary, "coverage" = colors$secondary),
        name = "Metric",
        labels = c("precision" = "Precision", "coverage" = "Coverage")
      ) +
      ggplot2::labs(
        title = title,
        subtitle = "Side-by-side comparison of precision and coverage metrics",
        x = "Search Term",
        y = "Score (0-1)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11, color = "gray60"),
        legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank()
      ) +
      ggplot2::ylim(0, 1)

    if (show_values) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.2f", .data[["value"]])),
        position = ggplot2::position_dodge(width = 0.7),
        hjust = -0.1,
        size = 2.5
      )
    }
  } else {
    stop("Invalid plot_type. Choose from: 'precision_coverage', 'counts', 'comparison', 'precision_only', or 'coverage_only'")
  }

  return(p)
}
