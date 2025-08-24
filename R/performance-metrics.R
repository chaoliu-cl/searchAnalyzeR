#' Calculate Precision and Recall Metrics
#'
#' @param retrieved Vector of retrieved article IDs
#' @param relevant Vector of relevant article IDs (gold standard)
#' @param total_relevant Total number of relevant articles in corpus
#' @details
#' Calculates standard information retrieval metrics:
#' \itemize{
#'   \item \strong{Precision}: TP/(TP+FP) - proportion of retrieved articles that are relevant
#'   \item \strong{Recall}: TP/(TP+FN) - proportion of relevant articles that were retrieved
#'   \item \strong{F1 Score}: Harmonic mean of precision and recall
#'   \item \strong{Number Needed to Read}: 1/precision - articles needed to read to find one relevant
#' }
#' where TP = True Positives, FP = False Positives, FN = False Negatives
#' @examples
#' retrieved_ids <- c("art1", "art2", "art3", "art4", "art5")
#' relevant_ids <- c("art1", "art3", "art6", "art7")
#' metrics <- calc_precision_recall(retrieved_ids, relevant_ids)
#' print(paste("Precision:", round(metrics$precision, 3)))
#' print(paste("Recall:", round(metrics$recall, 3)))
#' @references
#' Manning, C. D., Raghavan, P., & Schütze, H. (2008). Introduction to information retrieval.
#' @export
calc_precision_recall <- function(retrieved, relevant, total_relevant = NULL) {
  if (is.null(total_relevant)) total_relevant <- length(relevant)

  # Handle edge cases
  if (length(retrieved) == 0) {
    return(list(
      precision = 0,
      recall = 0,
      f1_score = 0,
      true_positives = 0,
      false_positives = 0,
      false_negatives = length(relevant),
      number_needed_to_read = Inf
    ))
  }

  if (length(relevant) == 0) {
    return(list(
      precision = 0,
      recall = NA,
      f1_score = 0,
      true_positives = 0,
      false_positives = length(retrieved),
      false_negatives = 0,
      number_needed_to_read = Inf
    ))
  }

  true_positives <- length(intersect(retrieved, relevant))
  false_positives <- length(setdiff(retrieved, relevant))
  false_negatives <- length(setdiff(relevant, retrieved))

  precision <- ifelse(length(retrieved) > 0, true_positives / length(retrieved), 0)
  recall <- ifelse(total_relevant > 0, true_positives / total_relevant, 0)
  f1_score <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)

  list(
    precision = precision,
    recall = recall,
    f1_score = f1_score,
    true_positives = true_positives,
    false_positives = false_positives,
    false_negatives = false_negatives,
    number_needed_to_read = ifelse(precision > 0, 1 / precision, Inf)
  )
}

#' Calculate Search Efficiency Metrics
#'
#' @param search_time Time taken to execute search (in seconds)
#' @param results_count Number of results retrieved
#' @param relevant_count Number of relevant results
#' @return List containing efficiency metrics
#' @details
#' Calculates various efficiency metrics for search performance:
#' \itemize{
#'   \item \strong{time_per_result}: Average time to retrieve each result
#'   \item \strong{time_per_relevant}: Average time to retrieve each relevant result
#'   \item \strong{relevant_ratio}: Proportion of results that are relevant
#'   \item \strong{efficiency_score}: Overall efficiency combining time and relevance
#' }
#' @examples
#' efficiency <- calc_efficiency(search_time = 30, results_count = 100, relevant_count = 15)
#' print(paste("Efficiency score:", round(efficiency$efficiency_score, 4)))
#' @export
calc_efficiency <- function(search_time, results_count, relevant_count) {
  # Handle edge cases
  if (search_time <= 0 || results_count <= 0) {
    return(list(
      time_per_result = Inf,
      time_per_relevant = Inf,
      relevant_ratio = 0,
      efficiency_score = 0
    ))
  }

  if (relevant_count <= 0) {
    return(list(
      time_per_result = search_time / results_count,
      time_per_relevant = Inf,
      relevant_ratio = 0,
      efficiency_score = 0
    ))
  }

  list(
    time_per_result = search_time / results_count,
    time_per_relevant = search_time / relevant_count,
    relevant_ratio = relevant_count / results_count,
    efficiency_score = relevant_count / (search_time * results_count)
  )
}

#' Calculate Coverage Metrics Across Databases
#'
#' @param results_by_database List of result sets by database
#' @param gold_standard Vector of relevant article IDs
#' @return List containing coverage statistics
#' @details
#' Calculates coverage metrics for each database and overall:
#' \itemize{
#'   \item \strong{coverage_count}: Number of relevant articles found by each database
#'   \item \strong{coverage_rate}: Proportion of relevant articles found by each database
#'   \item \strong{unique_coverage}: Number of relevant articles found only by this database
#'   \item \strong{total_coverage}: Overall proportion of relevant articles found by all databases
#'   \item \strong{redundancy_rate}: Proportion of duplicate results across databases
#' }
#' @examples
#' # Create sample data
#' results_db1 <- c("art1", "art2", "art3", "art4")
#' results_db2 <- c("art2", "art3", "art5", "art6")
#' results_by_db <- list("Database1" = results_db1, "Database2" = results_db2)
#' gold_standard <- c("art1", "art3", "art5", "art7", "art8")
#'
#' coverage <- calc_coverage(results_by_db, gold_standard)
#' print(coverage$total_coverage)
#' @export
calc_coverage <- function(results_by_database, gold_standard) {
  if (length(results_by_database) == 0 || length(gold_standard) == 0) {
    return(list(
      by_database = data.frame(),
      total_coverage = 0,
      redundancy_rate = 0
    ))
  }

  # Calculate coverage for each database using base R instead of purrr::map_dfr
  coverage_list <- lapply(names(results_by_database), function(db_name) {
    db_results <- results_by_database[[db_name]]
    covered <- intersect(db_results, gold_standard)

    # Calculate unique coverage (articles found only by this database)
    other_databases <- results_by_database[names(results_by_database) != db_name]
    other_results <- unique(unlist(other_databases))
    unique_covered <- setdiff(covered, other_results)

    data.frame(
      database = db_name,
      coverage_count = length(covered),
      coverage_rate = length(covered) / length(gold_standard),
      unique_coverage = length(unique_covered),
      stringsAsFactors = FALSE
    )
  })

  coverage_stats <- do.call(rbind, coverage_list)

  # Calculate overlap statistics
  all_results <- unique(unlist(results_by_database))
  total_coverage <- length(intersect(all_results, gold_standard)) / length(gold_standard)

  # Calculate redundancy rate
  total_results_with_duplicates <- sum(sapply(results_by_database, length))
  redundancy_rate <- if (total_results_with_duplicates > 0) {
    (total_results_with_duplicates - length(all_results)) / total_results_with_duplicates
  } else {
    0
  }

  list(
    by_database = coverage_stats,
    total_coverage = total_coverage,
    redundancy_rate = redundancy_rate
  )
}

#' Calculate Temporal Coverage Metrics
#'
#' @param search_results Data frame with search results including date column
#' @param target_date_range Vector of two dates defining the target time period
#' @return List containing temporal coverage statistics
#' @details
#' Analyzes the temporal distribution of search results:
#' \itemize{
#'   \item \strong{coverage_by_year}: Number of articles by publication year
#'   \item \strong{target_period_coverage}: Proportion of results in target date range
#'   \item \strong{temporal_gaps}: Years with no results in the target period
#'   \item \strong{peak_years}: Years with highest number of results
#' }
#' @examples
#' # Create sample data
#' search_results <- data.frame(
#'   id = paste0("art", 1:20),
#'   date = seq(as.Date("2010-01-01"), as.Date("2023-12-31"), length.out = 20)
#' )
#' target_range <- c(as.Date("2015-01-01"), as.Date("2020-12-31"))
#'
#' temporal_metrics <- calc_temporal_coverage(search_results, target_range)
#' print(temporal_metrics$target_period_coverage)
#' @export
calc_temporal_coverage <- function(search_results, target_date_range = NULL) {
  if (!"date" %in% names(search_results) || nrow(search_results) == 0) {
    return(list(
      coverage_by_year = data.frame(),
      target_period_coverage = NA,
      temporal_gaps = character(0),
      peak_years = character(0)
    ))
  }

  # Convert dates and extract years
  search_results$date <- as.Date(search_results$date)
  search_results$year <- lubridate::year(search_results$date)

  # Calculate coverage by year
  year_counts <- table(search_results$year)
  coverage_by_year <- data.frame(
    year = as.numeric(names(year_counts)),
    count = as.numeric(year_counts),
    stringsAsFactors = FALSE
  )

  # Calculate target period coverage if specified
  target_period_coverage <- NA
  temporal_gaps <- character(0)

  if (!is.null(target_date_range) && length(target_date_range) == 2) {
    target_results <- search_results[
      search_results$date >= target_date_range[1] & search_results$date <= target_date_range[2],
    ]
    target_period_coverage <- nrow(target_results) / nrow(search_results)

    # Find temporal gaps in target period
    target_start_year <- lubridate::year(target_date_range[1])
    target_end_year <- lubridate::year(target_date_range[2])
    all_target_years <- target_start_year:target_end_year
    years_with_results <- unique(lubridate::year(target_results$date))
    temporal_gaps <- setdiff(all_target_years, years_with_results)
  }

  # Find peak years (top 3)
  if (nrow(coverage_by_year) > 0) {
    sorted_years <- coverage_by_year[order(coverage_by_year$count, decreasing = TRUE), ]
    peak_years <- head(sorted_years$year, 3)
  } else {
    peak_years <- character(0)
  }

  list(
    coverage_by_year = coverage_by_year,
    target_period_coverage = target_period_coverage,
    temporal_gaps = temporal_gaps,
    peak_years = peak_years
  )
}

#' Calculate Strategy Comparison Metrics
#'
#' @param strategy1_results Vector of article IDs from strategy 1
#' @param strategy2_results Vector of article IDs from strategy 2
#' @param gold_standard Vector of relevant article IDs
#' @return List containing comparison metrics
#' @details
#' Compares two search strategies across multiple dimensions:
#' \itemize{
#'   \item \strong{overlap_analysis}: Articles found by both, one, or neither strategy
#'   \item \strong{performance_comparison}: Precision, recall, F1 for each strategy
#'   \item \strong{complementarity}: How well strategies complement each other
#'   \item \strong{efficiency_comparison}: Relative efficiency metrics
#' }
#' @examples
#' strategy1 <- c("art1", "art2", "art3", "art4", "art5")
#' strategy2 <- c("art3", "art4", "art5", "art6", "art7")
#' gold_standard <- c("art1", "art3", "art5", "art8", "art9")
#'
#' comparison <- calc_strategy_comparison(strategy1, strategy2, gold_standard)
#' print(comparison$overlap_analysis)
#' @export
calc_strategy_comparison <- function(strategy1_results, strategy2_results, gold_standard) {
  # Input validation
  if (is.null(strategy1_results) || is.null(strategy2_results) || is.null(gold_standard)) {
    stop("All inputs must be non-NULL")
  }

  if (!is.vector(strategy1_results) || !is.vector(strategy2_results) || !is.vector(gold_standard)) {
    stop("All inputs must be vectors")
  }

  # Calculate performance metrics for each strategy
  metrics1 <- calc_precision_recall(strategy1_results, gold_standard)
  metrics2 <- calc_precision_recall(strategy2_results, gold_standard)

  # Overlap analysis
  overlap <- intersect(strategy1_results, strategy2_results)
  unique_to_1 <- setdiff(strategy1_results, strategy2_results)
  unique_to_2 <- setdiff(strategy2_results, strategy1_results)
  combined_unique <- union(strategy1_results, strategy2_results)

  overlap_analysis <- list(
    overlap_count = length(overlap),
    unique_to_strategy1 = length(unique_to_1),
    unique_to_strategy2 = length(unique_to_2),
    total_unique = length(combined_unique),
    overlap_percentage = if (length(combined_unique) > 0) {
      length(overlap) / length(combined_unique) * 100
    } else {
      0
    }
  )

  # Performance comparison
  performance_comparison <- data.frame(
    strategy = c("Strategy 1", "Strategy 2", "Combined"),
    precision = c(metrics1$precision, metrics2$precision,
                  calc_precision_recall(combined_unique, gold_standard)$precision),
    recall = c(metrics1$recall, metrics2$recall,
               calc_precision_recall(combined_unique, gold_standard)$recall),
    f1_score = c(metrics1$f1_score, metrics2$f1_score,
                 calc_precision_recall(combined_unique, gold_standard)$f1_score),
    results_count = c(length(strategy1_results), length(strategy2_results), length(combined_unique)),
    stringsAsFactors = FALSE
  )

  # Complementarity metrics
  relevant_in_overlap <- length(intersect(overlap, gold_standard))
  relevant_unique_to_1 <- length(intersect(unique_to_1, gold_standard))
  relevant_unique_to_2 <- length(intersect(unique_to_2, gold_standard))

  complementarity <- list(
    added_recall_by_strategy2 = if (length(gold_standard) > 0) {
      relevant_unique_to_2 / length(gold_standard)
    } else {
      0
    },
    added_recall_by_strategy1 = if (length(gold_standard) > 0) {
      relevant_unique_to_1 / length(gold_standard)
    } else {
      0
    },
    synergy_score = if ((length(unique_to_1) + length(unique_to_2)) > 0) {
      (relevant_unique_to_1 + relevant_unique_to_2) / (length(unique_to_1) + length(unique_to_2))
    } else {
      0
    }
  )

  list(
    overlap_analysis = overlap_analysis,
    performance_comparison = performance_comparison,
    complementarity = complementarity,
    strategy1_metrics = metrics1,
    strategy2_metrics = metrics2
  )
}

#' Calculate Term Effectiveness Score
#'
#' @description
#' Calculates a balanced effectiveness score for individual search terms
#' using the harmonic mean of precision and coverage. This provides a
#' single metric to evaluate how well each term performs in retrieving
#' relevant articles.
#'
#' @param term_analysis Data frame from term_effectiveness() function
#' @param score_name Name for the new score column (default: "tes")
#' @return Data frame with added effectiveness score column
#'
#' @details
#' The Term Effectiveness Score (TES) is calculated as:
#' \deqn{TES = 2 \times \frac{precision \times coverage}{precision + coverage}}
#'
#' Where:
#' \itemize{
#'   \item \strong{Precision}: Proportion of retrieved articles that are relevant
#'   \item \strong{Coverage}: Proportion of term-specific relevant articles that were retrieved
#' }
#'
#' This differs from the traditional F1 score in that it uses \strong{coverage}
#' (term-specific relevance) rather than \strong{recall} (overall strategy relevance).
#'
#' **Key Differences from F1 Score:**
#' \itemize{
#'   \item \strong{F1 Score}: Precision × Recall (strategy-level performance)
#'   \item \strong{TES}: Precision × Coverage (term-level performance)
#'   \item \strong{Recall}: Relevant articles found / All relevant articles
#'   \item \strong{Coverage}: Relevant articles found / Term-specific relevant articles
#' }
#'
#' @examples
#' # Create sample term analysis
#' terms <- c("diabetes", "treatment", "clinical")
#' search_results <- data.frame(
#'   id = paste0("art", 1:20),
#'   title = paste("Study on", sample(terms, 20, replace = TRUE)),
#'   abstract = paste("Research about", sample(terms, 20, replace = TRUE))
#' )
#' gold_standard <- paste0("art", c(1, 3, 5, 7, 9))
#'
#' # Analyze term effectiveness
#' term_analysis <- term_effectiveness(terms, search_results, gold_standard)
#'
#' # Calculate effectiveness scores
#' term_scores <- calc_tes(term_analysis)
#' print(term_scores[order(term_scores$tes, decreasing = TRUE), ])
#'
#' @seealso
#' \code{\link{term_effectiveness}} for calculating term precision and coverage
#' \code{\link{calc_precision_recall}} for strategy-level F1 scores
#'
#' @export
calc_tes <- function(term_analysis, score_name = "tes") {

  # Validate input
  if (!inherits(term_analysis, "term_effectiveness")) {
    stop("Input must be a result from the term_effectiveness() function")
  }

  if (!all(c("precision", "coverage") %in% names(term_analysis))) {
    stop("term_analysis must contain 'precision' and 'coverage' columns")
  }

  # Calculate effectiveness score (harmonic mean of precision and coverage)
  precision <- term_analysis$precision
  coverage <- term_analysis$coverage

  # Handle cases where either precision or coverage is NA or 0
  effectiveness_score <- ifelse(
    is.na(precision) | is.na(coverage) | (precision + coverage) == 0,
    0,
    2 * (precision * coverage) / (precision + coverage)
  )

  # Add the score to the data frame
  result <- term_analysis
  result[[score_name]] <- effectiveness_score

  # Add metadata about the calculation
  attr(result, "score_info") <- list(
    score_name = score_name,
    formula = "2 * (precision * coverage) / (precision + coverage)",
    description = "Harmonic mean of precision and coverage for term-level effectiveness",
    calculated_at = Sys.time()
  )

  return(result)
}

#' Find Top Performing Terms
#'
#' @description
#' Identifies the top-performing search terms based on their effectiveness scores
#' and optionally creates highlighted visualizations.
#'
#' @param term_analysis Data frame from term_effectiveness() function
#' @param n Number of top terms to identify (default: 3)
#' @param score_col Name of the score column to use for ranking (default: "tes")
#' @param plot Whether to create a highlighted plot (default: TRUE)
#' @param plot_type Type of plot for highlighting ("precision_only", "coverage_only", "precision_coverage")
#' @return List containing top terms and optionally a highlighted plot
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Calculates effectiveness scores if not already present
#'   \item Identifies the top N performing terms
#'   \item Optionally creates a visualization highlighting these terms
#' }
#'
#' @export
find_top_terms <- function(term_analysis, n = 3, score_col = "tes",
                           plot = TRUE, plot_type = "precision_only") {

  # Calculate effectiveness scores if not present
  if (!score_col %in% names(term_analysis)) {
    term_analysis <- calc_tes(term_analysis, score_col)
  }

  # Identify top terms
  valid_scores <- !is.na(term_analysis[[score_col]])
  if (!any(valid_scores)) {
    warning("No valid effectiveness scores found")
    return(list(terms = character(0), data = NULL, plot = NULL))
  }

  # Sort by effectiveness score and get top N
  sorted_analysis <- term_analysis[order(term_analysis[[score_col]], decreasing = TRUE), ]
  top_terms <- head(sorted_analysis$term, n)

  result <- list(
    terms = top_terms,
    data = head(sorted_analysis, n),
    plot = NULL
  )

  # Create highlighted plot if requested
  if (plot && requireNamespace("ggplot2", quietly = TRUE)) {
    plot_title <- paste("Top", n, "Performing Terms (Highlighted)")

    result$plot <- plot_term_effectiveness(
      term_analysis,
      plot_type = plot_type,
      highlight_terms = top_terms,
      title_override = plot_title,
      show_values = TRUE
    )
  }

  return(result)
}

#' Compare Terms Across Strategies
#'
#' @description
#' Compares the effectiveness of terms across multiple search strategies
#' to identify which terms perform best in different contexts.
#'
#' @param term_list Named list of term_analysis objects from different strategies
#' @param top_n Number of top terms to compare (default: 5)
#' @return Data frame comparing term effectiveness across strategies
#'
#' @details
#' This function:
#' \itemize{
#'   \item Calculates effectiveness scores for each strategy
#'   \item Identifies top terms in each strategy
#'   \item Creates a comparison matrix showing performance across strategies
#' }
#'
#' @export
compare_terms <- function(term_list, top_n = 5) {

  if (!is.list(term_list) || length(term_list) < 2) {
    stop("term_list must be a named list with at least 2 strategies")
  }

  # Calculate effectiveness scores for each strategy
  strategy_results <- lapply(names(term_list), function(strategy_name) {
    analysis <- term_list[[strategy_name]]

    # Add effectiveness scores
    analysis <- calc_tes(analysis)

    # Get top terms
    top_results <- find_top_terms(analysis, n = top_n, plot = FALSE)

    data.frame(
      strategy = strategy_name,
      term = analysis$term,
      precision = analysis$precision,
      coverage = analysis$coverage,
      tes = analysis$tes,
      is_top = analysis$term %in% top_results$terms,
      stringsAsFactors = FALSE
    )
  })

  # Combine results
  comparison_df <- do.call(rbind, strategy_results)

  # Create summary of top terms by strategy
  top_summary <- aggregate(
    term ~ strategy,
    data = comparison_df[comparison_df$is_top, ],
    FUN = function(x) paste(x, collapse = ", ")
  )

  attr(comparison_df, "top_summary") <- top_summary
  attr(comparison_df, "comparison_info") <- list(
    n_strategies = length(term_list),
    top_n = top_n,
    created_at = Sys.time()
  )

  class(comparison_df) <- c("term_comparison", "data.frame")
  return(comparison_df)
}

#' Print Method for Term Comparison
#'
#' @param x A term_comparison object
#' @param ... Further arguments passed to or from other methods
#' @return Invisibly returns the input object
#' @export
print.term_comparison <- function(x, ...) {
  cat("Term Effectiveness Comparison\n")
  cat("============================\n")

  info <- attr(x, "comparison_info")
  if (!is.null(info)) {
    cat("Strategies compared:", info$n_strategies, "\n")
    cat("Top terms per strategy:", info$top_n, "\n\n")
  }

  # Show top terms summary
  top_summary <- attr(x, "top_summary")
  if (!is.null(top_summary)) {
    cat("Top performing terms by strategy:\n")
    for (i in 1:nrow(top_summary)) {
      cat(sprintf("  %s: %s\n", top_summary$strategy[i], top_summary$term[i]))
    }
    cat("\n")
  }

  # Show detailed comparison
  cat("Detailed comparison:\n")
  print.data.frame(x, row.names = FALSE, ...)

  invisible(x)
}
