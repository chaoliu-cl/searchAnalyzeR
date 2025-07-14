#' Search Strategy Analytics Engine
#'
#' Core class for analyzing systematic review search strategies
#'
#' @description
#' The SearchAnalyzer class provides a comprehensive framework for analyzing
#' the performance of systematic review search strategies. It calculates
#' precision, recall, and other performance metrics, generates visualizations,
#' and supports validation against gold standard datasets.
#'
#' @details
#' This R6 class encapsulates all functionality needed for search strategy
#' analysis. Key capabilities include:
#' \itemize{
#'   \item Performance metric calculation (precision, recall, F1, efficiency)
#'   \item Temporal and database coverage analysis
#'   \item Visualization generation for reports
#'   \item Gold standard validation
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(search_results, gold_standard, search_strategy)}}{Initialize analyzer}
#'   \item{\code{calculate_metrics()}}{Calculate comprehensive performance metrics}
#'   \item{\code{visualize_performance(type)}}{Generate performance visualizations}
#' }
#'
#' @examples
#' # Initialize with search results
#' search_data <- data.frame(
#'   id = paste0("art", 1:100),
#'   title = paste("Article", 1:100),
#'   abstract = paste("Abstract for article", 1:100),
#'   source = "PubMed",
#'   date = Sys.Date() - sample(1:365, 100, replace = TRUE)
#' )
#'
#' gold_standard <- paste0("art", sample(1:100, 20))
#'
#' analyzer <- SearchAnalyzer$new(
#'   search_results = search_data,
#'   gold_standard = gold_standard
#' )
#'
#' # Calculate metrics
#' metrics <- analyzer$calculate_metrics()
#'
#' # Generate overview plot
#' overview_plot <- analyzer$visualize_performance("overview")
#'
#' @export
SearchAnalyzer <- R6::R6Class(
  "SearchAnalyzer",
  public = list(
    #' @field search_results Data frame containing search results
    search_results = NULL,

    #' @field gold_standard Reference set of relevant articles
    gold_standard = NULL,

    #' @field metadata Search strategy metadata
    metadata = NULL,

    #' @description
    #' Initialize the analyzer with search results and optional gold standard.
    #' @param search_results Data frame with search results
    #' @param gold_standard Vector of known relevant article IDs
    #' @param search_strategy List containing search parameters
    #' @return No return value, called for side effects
    initialize = function(search_results, gold_standard = NULL, search_strategy = NULL) {
      self$search_results <- private$validate_search_results(search_results)
      self$gold_standard <- gold_standard
      self$metadata <- private$extract_metadata(search_strategy)
      private$analysis_timestamp <- Sys.time()
    },

    #' Calculate comprehensive performance metrics
    #' @return List of performance metrics
    calculate_metrics = function() {
      metrics <- list(
        basic = private$calculate_basic_metrics(),
        precision_recall = private$calc_precision_recall(),
        efficiency = private$calc_efficiency(),
        coverage = private$calc_coverage(),
        temporal = private$calculate_temporal_metrics()
      )

      class(metrics) <- "search_metrics"
      return(metrics)
    },

    #' Generate performance visualization
    #' @param type Type of visualization
    #' @return ggplot object
    visualize_performance = function(type = "overview") {
      switch(type,
             "overview" = private$plot_overview(),
             "precision_recall" = private$plot_pr_curve(),
             "temporal" = private$plot_temporal(),
             "database_comparison" = private$plot_db_performance(),
             "keyword_effectiveness" = private$plot_keyword_analysis(),
             stop("Unsupported visualization type: ", type)
      )
    }
  ),

  private = list(
    analysis_timestamp = NULL,

    validate_search_results = function(results) {
      required_cols <- c("id", "title", "abstract", "source", "date")
      if (!all(required_cols %in% names(results))) {
        stop("Search results must contain columns: ", paste(required_cols, collapse = ", "))
      }
      return(results)
    },

    extract_metadata = function(strategy) {
      if (is.null(strategy)) {
        return(list(
          search_terms = NULL,
          databases = NULL,
          date_range = NULL,
          filters = NULL,
          timestamp = Sys.time()
        ))
      }

      list(
        search_terms = strategy$terms,
        databases = strategy$databases,
        date_range = strategy$date_range,
        filters = strategy$filters,
        timestamp = Sys.time()
      )
    },

    calculate_basic_metrics = function() {
      list(
        total_records = nrow(self$search_results),
        unique_records = ifelse("duplicate" %in% names(self$search_results),
                                sum(!self$search_results$duplicate, na.rm = TRUE),
                                nrow(self$search_results)),
        duplicates = ifelse("duplicate" %in% names(self$search_results),
                            sum(self$search_results$duplicate, na.rm = TRUE),
                            0),
        date_range = range(self$search_results$date, na.rm = TRUE),
        sources = length(unique(self$search_results$source))
      )
    },

    calc_precision_recall = function() {
      if (is.null(self$gold_standard)) {
        warning("No gold standard provided - cannot calculate precision/recall")
        return(list(
          precision = NA,
          recall = NA,
          f1_score = NA,
          true_positives = NA,
          false_positives = NA,
          false_negatives = NA,
          number_needed_to_read = NA
        ))
      }

      calc_precision_recall(
        retrieved = self$search_results$id,
        relevant = self$gold_standard
      )
    },

    calc_efficiency = function() {
      # Placeholder - would need actual search time data
      list(
        efficiency_score = ifelse(is.null(self$gold_standard), NA,
                                  length(intersect(self$search_results$id, self$gold_standard)) / nrow(self$search_results))
      )
    },

    calc_coverage = function() {
      if ("search_source" %in% names(self$search_results)) {
        sources <- unique(self$search_results$search_source)
        results_by_source <- split(self$search_results$id, self$search_results$search_source)

        if (!is.null(self$gold_standard)) {
          coverage <- calc_coverage(results_by_source, self$gold_standard)
          return(coverage)
        }
      }

      list(total_coverage = NA)
    },

    calculate_temporal_metrics = function() {
      if (!all(is.na(self$search_results$date))) {
        years <- lubridate::year(self$search_results$date)
        year_dist <- table(years)

        return(list(
          date_range = range(self$search_results$date, na.rm = TRUE),
          year_distribution = as.list(year_dist),
          temporal_span = diff(range(self$search_results$date, na.rm = TRUE))
        ))
      }

      list(date_range = c(NA, NA))
    },

    plot_overview = function() {
      if (is.null(self$gold_standard)) {
        # Create basic plot without performance metrics
        basic_metrics <- private$calculate_basic_metrics()

        overview_data <- tibble(
          metric = c("Total Records", "Unique Records", "Sources"),
          value = c(basic_metrics$total_records / max(basic_metrics$total_records, 1),
                    basic_metrics$unique_records / max(basic_metrics$total_records, 1),
                    basic_metrics$sources / 10), # Normalize to 0-1 scale
          category = c("Count", "Count", "Diversity")
        )

        ggplot(overview_data, aes(x = metric, y = value, fill = category)) +
          geom_col(alpha = 0.8) +
          geom_text(aes(label = sprintf("%.2f", value)), vjust = -0.5) +
          labs(title = "Search Results Overview",
               subtitle = "Basic statistics (no gold standard provided)",
               x = "Metrics", y = "Normalized Score") +
          theme_minimal() +
          ylim(0, 1)
      } else {
        # Use the full plot_overview function
        metrics <- self$calculate_metrics()
        plot_overview(metrics)
      }
    },

    plot_pr_curve = function() {
      if (is.null(self$gold_standard)) {
        # Create placeholder plot
        ggplot() +
          geom_text(aes(x = 0.5, y = 0.5), label = "No gold standard provided\nfor precision-recall analysis") +
          xlim(0, 1) + ylim(0, 1) +
          labs(title = "Precision-Recall Curve", x = "Recall", y = "Precision") +
          theme_minimal()
      } else {
        plot_pr_curve(self$search_results$id, self$gold_standard)
      }
    },

    plot_temporal = function() {
      plot_temporal(self$search_results, self$gold_standard)
    },

    plot_db_performance = function() {
      if ("search_source" %in% names(self$search_results)) {
        results_by_db <- split(self$search_results$id, self$search_results$search_source)
        plot_db_performance(results_by_db, self$gold_standard)
      } else {
        ggplot() +
          geom_text(aes(x = 0.5, y = 0.5), label = "No database source information available") +
          xlim(0, 1) + ylim(0, 1) +
          labs(title = "Database Performance Comparison") +
          theme_minimal()
      }
    },

    plot_keyword_analysis = function() {
      if (!is.null(self$metadata$search_terms)) {
        plot_keyword_effectiveness(self$search_results, self$metadata$search_terms, self$gold_standard)
      } else {
        ggplot() +
          geom_text(aes(x = 0.5, y = 0.5), label = "No search terms available for analysis") +
          xlim(0, 1) + ylim(0, 1) +
          labs(title = "Keyword Effectiveness Analysis") +
          theme_minimal()
      }
    }
  )
)
