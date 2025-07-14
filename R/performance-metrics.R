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
#' @export
calc_efficiency <- function(search_time, results_count, relevant_count) {
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
#' @export
calc_coverage <- function(results_by_database, gold_standard) {
  coverage_stats <- map_dfr(results_by_database, function(db_results) {
    covered <- intersect(db_results, gold_standard)
    list(
      coverage_count = length(covered),
      coverage_rate = length(covered) / length(gold_standard),
      unique_coverage = length(setdiff(covered, unlist(results_by_database[names(results_by_database) != names(db_results)])))
    )
  }, .id = "database")

  # Calculate overlap statistics
  all_results <- unique(unlist(results_by_database))
  total_coverage <- length(intersect(all_results, gold_standard)) / length(gold_standard)

  list(
    by_database = coverage_stats,
    total_coverage = total_coverage,
    redundancy_rate = (sum(lengths(results_by_database)) - length(all_results)) / sum(lengths(results_by_database))
  )
}
