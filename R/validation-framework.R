#' Benchmark Validation System
#'
#' @description
#' A comprehensive validation framework for testing search strategies against
#' established benchmark datasets across multiple domains.
#'
#' @details
#' The BenchmarkValidator class provides tools for:
#' \itemize{
#'   \item Cross-domain validation across medical, environmental, social science domains
#'   \item Sensitivity analysis for search parameters
#'   \item Statistical comparison of strategy performance
#'   \item Reproducible benchmark testing
#' }
#'
#' @section Fields:
#' \describe{
#'   \item{\code{benchmarks}}{List of benchmark datasets with known relevant articles}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{Initialize a new BenchmarkValidator instance}
#'   \item{\code{validate_strategy(search_strategy, benchmark_name)}}{Validate against specific benchmark}
#'   \item{\code{cross_domain_validation(search_strategy)}}{Test across multiple domains}
#'   \item{\code{sensitivity_analysis(base_strategy, parameter_ranges)}}{Parameter sensitivity testing}
#' }
#'
#' @examples
#' # Create validator
#' validator <- BenchmarkValidator$new()
#'
#' # Check available benchmarks
#' print(names(validator$benchmarks))
#'
#' # Define search strategy
#' strategy <- list(
#'   terms = c("systematic review", "meta-analysis"),
#'   databases = c("PubMed", "Embase")
#' )
#'
#' # Create sample data for validation
#' sample_data <- data.frame(
#'   id = paste0("art", 1:20),
#'   title = paste("Article", 1:20),
#'   abstract = paste("Abstract", 1:20),
#'   source = "Journal",
#'   date = Sys.Date()
#' )
#'
#' # Add custom benchmark
#' validator$add_benchmark("custom", sample_data, paste0("art", 1:5))
#'
#' # Validate against custom benchmark
#' results <- validator$validate_strategy(strategy, "custom")
#'
#' @export
BenchmarkValidator <- R6::R6Class(
  "BenchmarkValidator",
  public = list(
    #' @field benchmarks List of benchmark datasets
    benchmarks = NULL,

    #' @description
    #' Creates a new BenchmarkValidator instance and loads benchmark datasets.
    #' This method is called automatically when creating a new validator with
    #' \code{BenchmarkValidator$new()}.
    #' @return No return value, called for side effects (loading benchmarks)
    initialize = function() {
      self$benchmarks <- private$load_benchmark_datasets()
    },

    #' Add a custom benchmark dataset
    #' @param name Name of the benchmark
    #' @param corpus Data frame with article corpus
    #' @param relevant_ids Vector of relevant article IDs
    #' @return No return value, called for side effects
    add_benchmark = function(name, corpus, relevant_ids) {
      self$benchmarks[[name]] <- list(
        corpus = corpus,
        relevant_ids = relevant_ids
      )
      invisible(self)
    },

    #' Validate search strategy against benchmarks
    #' @param search_strategy Search strategy object
    #' @param benchmark_name Name of benchmark dataset
    #' @return Validation results
    #' @importFrom purrr map
    validate_strategy = function(search_strategy, benchmark_name = "all") {
      if (length(self$benchmarks) == 0) {
        stop("No benchmarks available. Add benchmarks using add_benchmark() method.")
      }

      if (benchmark_name == "all") {
        # Use public method instead of private method
        results <- purrr::map(names(self$benchmarks), ~self$validate_single_benchmark(search_strategy, .x))
        names(results) <- names(self$benchmarks)
        return(results)
      } else {
        return(self$validate_single_benchmark(search_strategy, benchmark_name))
      }
    },

    #' Validate against single benchmark (PUBLIC METHOD)
    #' @param search_strategy Search strategy object
    #' @param benchmark_name Name of benchmark dataset
    #' @return Validation results
    validate_single_benchmark = function(search_strategy, benchmark_name) {
      if (!benchmark_name %in% names(self$benchmarks)) {
        stop("Benchmark '", benchmark_name, "' not found. Available benchmarks: ",
             paste(names(self$benchmarks), collapse = ", "))
      }

      benchmark <- self$benchmarks[[benchmark_name]]
      private$execute_validation(search_strategy, benchmark)
    },

    #' Cross-domain validation
    #' @param search_strategy Search strategy object
    #' @return Cross-domain validation results
    #' @importFrom purrr map_dfr
    #' @importFrom dplyr summarise mutate arrange
    #' @importFrom tibble tibble
    cross_domain_validation = function(search_strategy) {
      if (length(self$benchmarks) == 0) {
        warning("No benchmarks available for cross-domain validation")
        return(tibble::tibble())
      }

      domains <- c("medical", "environmental", "social_science", "engineering")

      results <- purrr::map_dfr(domains, function(domain) {
        # FIXED: Use base R pattern matching instead of str_detect
        domain_benchmarks <- self$benchmarks[grepl(domain, names(self$benchmarks))]

        if (length(domain_benchmarks) == 0) {
          return(tibble::tibble(domain = domain, mean_precision = NA, mean_recall = NA,
                                mean_f1 = NA, sd_precision = NA, sd_recall = NA, n_benchmarks = 0))
        }

        domain_results <- purrr::map_dfr(domain_benchmarks, function(benchmark) {
          validation <- private$execute_validation(search_strategy, benchmark)
          tibble::tibble(
            precision = validation$precision,
            recall = validation$recall,
            f1_score = validation$f1_score
          )
        }, .id = "benchmark")

        domain_results %>%
          dplyr::summarise(
            mean_precision = mean(.data$precision, na.rm = TRUE),
            mean_recall = mean(.data$recall, na.rm = TRUE),
            mean_f1 = mean(.data$f1_score, na.rm = TRUE),
            sd_precision = stats::sd(.data$precision, na.rm = TRUE),
            sd_recall = stats::sd(.data$recall, na.rm = TRUE),
            n_benchmarks = n(),
            .groups = "drop"
          ) %>%
          dplyr::mutate(domain = domain)
      })

      return(results)
    },

    #' Sensitivity analysis for search parameters
    #' @param base_strategy Base search strategy
    #' @param parameter_ranges List of parameter ranges to test
    #' @return Sensitivity analysis results
    #' @importFrom tidyr expand_grid
    #' @importFrom purrr pmap_dfr
    #' @importFrom dplyr summarise bind_cols
    sensitivity_analysis = function(base_strategy, parameter_ranges) {
      if (length(self$benchmarks) == 0) {
        stop("No benchmarks available for sensitivity analysis")
      }

      param_grid <- tidyr::expand_grid(!!!parameter_ranges)

      results <- purrr::pmap_dfr(param_grid, function(...) {
        params <- list(...)
        modified_strategy <- private$modify_strategy(base_strategy, params)

        # Test against available benchmarks (up to 3 for efficiency)
        benchmark_subset <- utils::head(self$benchmarks, 3)

        benchmark_results <- purrr::map_dfr(benchmark_subset, function(benchmark) {
          validation <- private$execute_validation(modified_strategy, benchmark)
          tibble::tibble(
            precision = validation$precision,
            recall = validation$recall,
            f1_score = validation$f1_score
          )
        })

        # Aggregate results
        benchmark_results %>%
          dplyr::summarise(
            mean_precision = mean(.data$precision, na.rm = TRUE),
            mean_recall = mean(.data$recall, na.rm = TRUE),
            mean_f1 = mean(.data$f1_score, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::bind_cols(tibble::tibble(!!!params))
      })

      class(results) <- c("sensitivity_analysis", class(results))
      return(results)
    }
  ),

  private = list(
    load_benchmark_datasets = function() {
      # Create sample benchmark datasets since the external ones don't exist
      # In a real package, these would be loaded from installed data files

      sample_benchmarks <- list()

      # Create a sample medical benchmark
      if (requireNamespace("dplyr", quietly = TRUE)) {
        medical_corpus <- data.frame(
          id = paste0("med", 1:50),
          title = paste("Medical study", 1:50),
          abstract = paste("Medical research on", sample(c("diabetes", "hypertension", "cancer"), 50, replace = TRUE)),
          source = "Medical Journal",
          date = Sys.Date() - sample(1:365, 50, replace = TRUE),
          stringsAsFactors = FALSE
        )

        sample_benchmarks$medical_benchmark <- list(
          corpus = medical_corpus,
          relevant_ids = paste0("med", sample(1:50, 10))
        )
      }

      # Return the sample benchmarks (empty list if no data available)
      return(sample_benchmarks)
    },

    execute_validation = function(strategy, benchmark) {
      # Simulate search execution against benchmark
      retrieved_ids <- private$simulate_search(strategy, benchmark$corpus)

      calc_precision_recall(
        retrieved = retrieved_ids,
        relevant = benchmark$relevant_ids,
        total_relevant = length(benchmark$relevant_ids)
      )
    },

    simulate_search = function(strategy, corpus) {
      # Simplified search simulation
      # In practice, this would execute the actual search
      search_terms <- strategy$terms

      # Handle missing or empty terms
      if (is.null(search_terms) || length(search_terms) == 0) {
        return(character(0))
      }

      # FIXED: Use base R functions instead of stringr
      # Text matching simulation with fallback to base R
      if (requireNamespace("dplyr", quietly = TRUE) && requireNamespace("stringr", quietly = TRUE)) {
        # Use tidyverse if available
        matches <- corpus %>%
          dplyr::filter(stringr::str_detect(tolower(paste(.data$title, .data$abstract)),
                                            paste(tolower(search_terms), collapse = "|"))) %>%
          dplyr::pull(.data$id)
      } else {
        # FIXED: Fallback to base R - this is the key fix
        searchable_text <- tolower(paste(corpus$title, corpus$abstract))
        pattern <- paste(tolower(search_terms), collapse = "|")
        match_indices <- grep(pattern, searchable_text)
        matches <- corpus$id[match_indices]
      }

      return(matches)
    },

    modify_strategy = function(base_strategy, params) {
      # Modify strategy parameters for sensitivity analysis
      modified <- base_strategy

      for (param_name in names(params)) {
        modified[[param_name]] <- params[[param_name]]
      }

      return(modified)
    }
  )
)
