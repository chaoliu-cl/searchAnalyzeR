#' Benchmark Testing Framework for Search Strategies
#'
#' This file contains advanced benchmark testing capabilities including
#' cross-validation, statistical testing, and performance comparison methods.

#' Statistical Significance Testing for Search Performance
#'
#' @param strategy1_results Results from first search strategy
#' @param strategy2_results Results from second search strategy
#' @param gold_standard Vector of relevant article IDs
#' @param test_type Type of statistical test ("mcnemar", "paired_t", "wilcoxon")
#' @param alpha Significance level
#' @return Statistical test results
#' @importFrom stats mcnemar.test t.test wilcox.test
#' @export
compare_strategies <- function(strategy1_results, strategy2_results,
                                      gold_standard, test_type = "mcnemar",
                                      alpha = 0.05) {

  # Calculate performance metrics for both strategies
  metrics1 <- calc_precision_recall(strategy1_results, gold_standard)
  metrics2 <- calc_precision_recall(strategy2_results, gold_standard)

  # Create contingency table for McNemar's test
  if (test_type == "mcnemar") {
    # Articles found by strategy 1
    found_by_1 <- gold_standard %in% strategy1_results
    # Articles found by strategy 2
    found_by_2 <- gold_standard %in% strategy2_results

    # Create 2x2 contingency table
    contingency <- table(found_by_1, found_by_2)

    # Perform McNemar's test
    test_result <- stats::mcnemar.test(contingency)

    result <- list(
      test = "McNemar's Test",
      statistic = test_result$statistic,
      p_value = test_result$p.value,
      significant = test_result$p.value < alpha,
      contingency_table = contingency,
      strategy1_metrics = metrics1,
      strategy2_metrics = metrics2,
      difference = list(
        precision_diff = metrics2$precision - metrics1$precision,
        recall_diff = metrics2$recall - metrics1$recall,
        f1_diff = metrics2$f1_score - metrics1$f1_score
      )
    )
  } else if (test_type == "paired_t") {
    # For paired t-test, we need multiple samples or bootstrap
    # Here we'll use bootstrap sampling to create paired observations

    bootstrap_comparison <- bootstrap_compare(
      strategy1_results, strategy2_results, gold_standard, n_bootstrap = 1000
    )

    t_test <- stats::t.test(bootstrap_comparison$strategy1_f1,
                            bootstrap_comparison$strategy2_f1,
                            paired = TRUE)

    result <- list(
      test = "Paired t-test (Bootstrap)",
      statistic = t_test$statistic,
      p_value = t_test$p.value,
      significant = t_test$p.value < alpha,
      confidence_interval = t_test$conf.int,
      strategy1_metrics = metrics1,
      strategy2_metrics = metrics2,
      bootstrap_results = bootstrap_comparison
    )
  } else if (test_type == "wilcoxon") {
    # Wilcoxon signed-rank test for non-parametric comparison
    bootstrap_comparison <- bootstrap_compare(
      strategy1_results, strategy2_results, gold_standard, n_bootstrap = 1000
    )

    wilcox_test <- stats::wilcox.test(bootstrap_comparison$strategy1_f1,
                                      bootstrap_comparison$strategy2_f1,
                                      paired = TRUE)

    result <- list(
      test = "Wilcoxon Signed-Rank Test (Bootstrap)",
      statistic = wilcox_test$statistic,
      p_value = wilcox_test$p.value,
      significant = wilcox_test$p.value < alpha,
      strategy1_metrics = metrics1,
      strategy2_metrics = metrics2,
      bootstrap_results = bootstrap_comparison
    )
  } else {
    stop("Unsupported test type: ", test_type)
  }

  class(result) <- "strategy_comparison"
  return(result)
}

#' Bootstrap Comparison of Search Strategies
#'
#' @param strategy1_results Results from first strategy
#' @param strategy2_results Results from second strategy
#' @param gold_standard Vector of relevant article IDs
#' @param n_bootstrap Number of bootstrap samples
#' @return Bootstrap comparison results
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
bootstrap_compare <- function(strategy1_results, strategy2_results,
                                          gold_standard, n_bootstrap = 1000) {

  bootstrap_results <- purrr::map_dfr(1:n_bootstrap, function(i) {
    # Bootstrap sample of gold standard
    sample_size <- min(length(gold_standard), 50)  # Limit sample size for efficiency
    sample_indices <- sample(length(gold_standard), sample_size, replace = TRUE)
    bootstrap_gold <- gold_standard[sample_indices]

    # Calculate metrics for both strategies on bootstrap sample
    metrics1 <- calc_precision_recall(strategy1_results, bootstrap_gold)
    metrics2 <- calc_precision_recall(strategy2_results, bootstrap_gold)

    tibble::tibble(
      iteration = i,
      strategy1_precision = metrics1$precision,
      strategy1_recall = metrics1$recall,
      strategy1_f1 = metrics1$f1_score,
      strategy2_precision = metrics2$precision,
      strategy2_recall = metrics2$recall,
      strategy2_f1 = metrics2$f1_score
    )
  })

  return(bootstrap_results)
}

#' Cross-Validation Framework for Search Strategies
#'
#' @param search_strategy Search strategy object
#' @param validation_corpus Full corpus for validation
#' @param gold_standard Vector of relevant article IDs
#' @param k_folds Number of folds for cross-validation
#' @param stratified Whether to use stratified sampling
#' @return Cross-validation results
#' @importFrom purrr map_dfr map2
#' @importFrom dplyr summarise
#' @importFrom stats sd
#' @export
cv_strategy <- function(search_strategy, validation_corpus,
                                    gold_standard, k_folds = 5,
                                    stratified = TRUE) {

  # Create folds
  if (stratified) {
    # Stratify by relevance
    relevant_indices <- which(validation_corpus$id %in% gold_standard)
    non_relevant_indices <- which(!validation_corpus$id %in% gold_standard)

    relevant_folds <- split(sample(relevant_indices),
                            rep(1:k_folds, length.out = length(relevant_indices)))
    non_relevant_folds <- split(sample(non_relevant_indices),
                                rep(1:k_folds, length.out = length(non_relevant_indices)))

    folds <- purrr::map2(relevant_folds, non_relevant_folds, c)
  } else {
    all_indices <- sample(seq_len(nrow(validation_corpus)))
    folds <- split(all_indices, rep(1:k_folds, length.out = length(all_indices)))
  }

  # Perform cross-validation
  cv_results <- purrr::map_dfr(1:k_folds, function(fold_idx) {
    test_indices <- folds[[fold_idx]]
    train_indices <- setdiff(seq_len(nrow(validation_corpus)), test_indices)

    # Use training set to optimize strategy (if applicable)
    train_corpus <- validation_corpus[train_indices, ]
    test_corpus <- validation_corpus[test_indices, ]

    # Apply search strategy to test set
    test_results <- simulate_search_execution(search_strategy, test_corpus)
    test_gold_standard <- intersect(gold_standard, test_corpus$id)

    # Calculate metrics
    metrics <- calc_precision_recall(test_results, test_gold_standard)

    tibble::tibble(
      fold = fold_idx,
      precision = metrics$precision,
      recall = metrics$recall,
      f1_score = metrics$f1_score,
      true_positives = metrics$true_positives,
      false_positives = metrics$false_positives,
      false_negatives = metrics$false_negatives,
      test_size = nrow(test_corpus),
      relevant_in_test = length(test_gold_standard)
    )
  })

  # Calculate summary statistics
  summary_stats <- cv_results %>%
    dplyr::summarise(
      mean_precision = mean(.data$precision, na.rm = TRUE),
      sd_precision = stats::sd(.data$precision, na.rm = TRUE),
      mean_recall = mean(.data$recall, na.rm = TRUE),
      sd_recall = stats::sd(.data$recall, na.rm = TRUE),
      mean_f1 = mean(.data$f1_score, na.rm = TRUE),
      sd_f1 = stats::sd(.data$f1_score, na.rm = TRUE),
      cv_precision = .data$sd_precision / .data$mean_precision,  # Coefficient of variation
      cv_recall = .data$sd_recall / .data$mean_recall,
      cv_f1 = .data$sd_f1 / .data$mean_f1,
      .groups = "drop"
    )

  result <- list(
    fold_results = cv_results,
    summary = summary_stats,
    k_folds = k_folds,
    stratified = stratified,
    total_corpus_size = nrow(validation_corpus),
    total_relevant = length(gold_standard)
  )

  class(result) <- "cv_results"
  return(result)
}

#' Benchmark Suite Execution
#'
#' @param search_strategies List of search strategy objects
#' @param benchmark_datasets List of benchmark datasets
#' @param metrics_to_calculate Vector of metrics to calculate
#' @return Comprehensive benchmark results
#' @importFrom purrr map_dfr map_int
#' @importFrom dplyr group_by mutate ungroup arrange
#' @export
run_benchmarks <- function(search_strategies, benchmark_datasets,
                                metrics_to_calculate = c("precision", "recall", "f1", "efficiency")) {

  # Initialize progress tracking
  total_combinations <- length(search_strategies) * length(benchmark_datasets)
  current_combination <- 0

  # Run all strategy-benchmark combinations
  results <- purrr::map_dfr(names(search_strategies), function(strategy_name) {
    strategy <- search_strategies[[strategy_name]]

    purrr::map_dfr(names(benchmark_datasets), function(benchmark_name) {
      current_combination <<- current_combination + 1
      cat("Running benchmark", current_combination, "of", total_combinations,
          ":", strategy_name, "on", benchmark_name, "\n")

      benchmark <- benchmark_datasets[[benchmark_name]]

      # Execute search strategy on benchmark
      start_time <- Sys.time()
      retrieved_ids <- simulate_search_execution(strategy, benchmark$corpus)
      execution_time <- as.numeric(Sys.time() - start_time, units = "secs")

      # Calculate requested metrics
      basic_metrics <- calc_precision_recall(retrieved_ids, benchmark$relevant_ids)

      result_row <- tibble::tibble(
        strategy_name = strategy_name,
        benchmark_name = benchmark_name,
        execution_time = execution_time
      )

      # Add basic metrics
      if ("precision" %in% metrics_to_calculate) {
        result_row$precision <- basic_metrics$precision
      }
      if ("recall" %in% metrics_to_calculate) {
        result_row$recall <- basic_metrics$recall
      }
      if ("f1" %in% metrics_to_calculate) {
        result_row$f1_score <- basic_metrics$f1_score
      }

      # Add efficiency metrics
      if ("efficiency" %in% metrics_to_calculate) {
        efficiency_metrics <- calc_efficiency(
          execution_time, length(retrieved_ids), basic_metrics$true_positives
        )
        result_row$time_per_result <- efficiency_metrics$time_per_result
        result_row$time_per_relevant <- efficiency_metrics$time_per_relevant
        result_row$efficiency_score <- efficiency_metrics$efficiency_score
      }

      # Add additional metrics
      result_row$true_positives <- basic_metrics$true_positives
      result_row$false_positives <- basic_metrics$false_positives
      result_row$false_negatives <- basic_metrics$false_negatives
      result_row$total_retrieved <- length(retrieved_ids)
      result_row$total_relevant <- length(benchmark$relevant_ids)

      return(result_row)
    })
  })

  # Calculate rankings and relative performance
  results <- results %>%
    dplyr::group_by(.data$benchmark_name) %>%
    dplyr::mutate(
      precision_rank = rank(-.data$precision, ties.method = "min"),
      recall_rank = rank(-.data$recall, ties.method = "min"),
      f1_rank = rank(-.data$f1_score, ties.method = "min")
    ) %>%
    dplyr::ungroup()

  # Create summary statistics
  summary_stats <- results %>%
    dplyr::group_by(.data$strategy_name) %>%
    dplyr::summarise(
      n_benchmarks = dplyr::n(),
      mean_precision = mean(.data$precision, na.rm = TRUE),
      mean_recall = mean(.data$recall, na.rm = TRUE),
      mean_f1 = mean(.data$f1_score, na.rm = TRUE),
      mean_precision_rank = mean(.data$precision_rank, na.rm = TRUE),
      mean_recall_rank = mean(.data$recall_rank, na.rm = TRUE),
      mean_f1_rank = mean(.data$f1_rank, na.rm = TRUE),
      total_execution_time = sum(.data$execution_time, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data$mean_f1_rank)  # Best strategies first

  benchmark_suite_results <- list(
    detailed_results = results,
    summary = summary_stats,
    benchmark_info = purrr::map_dfr(benchmark_datasets, function(bm) {
      tibble::tibble(
        corpus_size = nrow(bm$corpus),
        relevant_count = length(bm$relevant_ids),
        relevance_rate = length(bm$relevant_ids) / nrow(bm$corpus)
      )
    }, .id = "benchmark_name"),
    execution_timestamp = Sys.time()
  )

  class(benchmark_suite_results) <- "benchmark_suite_results"
  return(benchmark_suite_results)
}

#' Simulate Search Strategy Execution
#'
#' @param strategy Search strategy object
#' @param corpus Data frame with article corpus
#' @return Vector of retrieved article IDs
#' @importFrom stringr str_detect
simulate_search_execution <- function(strategy, corpus) {
  # This is a simplified simulation - in practice, this would interface
  # with actual database APIs or search engines

  search_terms <- strategy$terms

  # Combine title and abstract for searching
  searchable_text <- paste(corpus$title, corpus$abstract, sep = " ")
  searchable_text <- tolower(searchable_text)

  # Create search pattern from terms
  if (length(search_terms) == 1) {
    search_pattern <- tolower(search_terms)
  } else {
    # Simple OR logic for multiple terms
    search_pattern <- paste(tolower(search_terms), collapse = "|")
  }

  # Find matching articles
  matches <- stringr::str_detect(searchable_text, search_pattern)
  retrieved_ids <- corpus$id[matches]

  # Apply date filters if specified
  if ("date_range" %in% names(strategy) && !is.null(strategy$date_range)) {
    date_filtered <- corpus$date >= strategy$date_range[1] &
      corpus$date <= strategy$date_range[2]
    retrieved_ids <- intersect(retrieved_ids, corpus$id[date_filtered])
  }

  # Apply additional filters if specified
  if ("filters" %in% names(strategy) && !is.null(strategy$filters)) {
    for (filter_name in names(strategy$filters)) {
      filter_value <- strategy$filters[[filter_name]]

      if (filter_name %in% names(corpus)) {
        filtered_ids <- corpus$id[corpus[[filter_name]] %in% filter_value]
        retrieved_ids <- intersect(retrieved_ids, filtered_ids)
      }
    }
  }

  return(retrieved_ids)
}

#' Power Analysis for Search Strategy Evaluation
#'
#' @param effect_size Expected effect size (difference in F1 scores)
#' @param alpha Significance level
#' @param power Desired statistical power
#' @param baseline_f1 Baseline F1 score
#' @return Required sample size
#' @importFrom stats qnorm
#' @export
calc_sample_size <- function(effect_size = 0.1, alpha = 0.05,
                                  power = 0.8, baseline_f1 = 0.7) {

  if (requireNamespace("pwr", quietly = TRUE)) {
    # Convert F1 scores to effect size for power analysis
    cohen_d <- effect_size / sqrt((baseline_f1 * (1 - baseline_f1)) * 2)

    power_result <- pwr::pwr.t.test(
      d = cohen_d,
      sig.level = alpha,
      power = power,
      type = "paired"
    )

    result <- list(
      required_sample_size = ceiling(power_result$n),
      effect_size = effect_size,
      alpha = alpha,
      power = power,
      baseline_f1 = baseline_f1,
      cohen_d = cohen_d
    )
  } else {
    # Simple approximation without pwr package
    z_alpha <- stats::qnorm(1 - alpha/2)
    z_beta <- stats::qnorm(power)

    # Approximate sample size calculation
    n_approx <- ceiling(((z_alpha + z_beta)^2 * 2 * baseline_f1 * (1 - baseline_f1)) / effect_size^2)

    result <- list(
      required_sample_size = n_approx,
      effect_size = effect_size,
      alpha = alpha,
      power = power,
      baseline_f1 = baseline_f1,
      method = "approximation"
    )
  }

  class(result) <- "power_analysis"
  return(result)
}

#' Meta-Analysis of Benchmark Results
#'
#' @param benchmark_results List of benchmark result objects
#' @param strategy_name Name of strategy to analyze across benchmarks
#' @param metric Metric to meta-analyze ("precision", "recall", "f1_score")
#' @return Meta-analysis results
#' @importFrom purrr map_dfr
#' @importFrom dplyr filter mutate case_when
#' @importFrom stats pchisq
#' @export
meta_analyze <- function(benchmark_results, strategy_name,
                                    metric = "f1_score") {

  # Extract relevant data from benchmark results
  meta_data <- purrr::map_dfr(benchmark_results, function(result) {
    if (strategy_name %in% result$detailed_results$strategy_name) {
      strategy_data <- result$detailed_results %>%
        dplyr::filter(.data$strategy_name == !!strategy_name)

      tibble::tibble(
        benchmark = unique(strategy_data$benchmark_name),
        metric_value = strategy_data[[metric]],
        n_relevant = strategy_data$total_relevant,
        n_retrieved = strategy_data$total_retrieved
      )
    }
  }, .id = "study")

  if (nrow(meta_data) == 0) {
    stop("No data found for strategy: ", strategy_name)
  }

  # Calculate weights (inverse variance weighting)
  meta_data <- meta_data %>%
    dplyr::mutate(
      variance = .data$metric_value * (1 - .data$metric_value) / .data$n_relevant,  # Approximate variance
      weight = 1 / .data$variance,
      weighted_metric = .data$metric_value * .data$weight
    )

  # Calculate pooled estimate
  pooled_estimate <- sum(meta_data$weighted_metric, na.rm = TRUE) /
    sum(meta_data$weight, na.rm = TRUE)

  # Calculate confidence interval
  pooled_variance <- 1 / sum(meta_data$weight, na.rm = TRUE)
  pooled_se <- sqrt(pooled_variance)

  ci_lower <- pooled_estimate - 1.96 * pooled_se
  ci_upper <- pooled_estimate + 1.96 * pooled_se

  # Test for heterogeneity (Q statistic)
  q_statistic <- sum(meta_data$weight * (meta_data$metric_value - pooled_estimate)^2, na.rm = TRUE)
  df_q <- nrow(meta_data) - 1
  p_value_q <- 1 - stats::pchisq(q_statistic, df_q)

  # I-squared statistic
  i_squared <- max(0, (q_statistic - df_q) / q_statistic) * 100

  result <- list(
    strategy = strategy_name,
    metric = metric,
    n_studies = nrow(meta_data),
    pooled_estimate = pooled_estimate,
    confidence_interval = c(ci_lower, ci_upper),
    standard_error = pooled_se,
    heterogeneity = list(
      q_statistic = q_statistic,
      p_value = p_value_q,
      i_squared = i_squared,
      interpretation = dplyr::case_when(
        i_squared < 25 ~ "Low heterogeneity",
        i_squared < 50 ~ "Moderate heterogeneity",
        i_squared < 75 ~ "Substantial heterogeneity",
        TRUE ~ "Considerable heterogeneity"
      )
    ),
    study_data = meta_data
  )

  class(result) <- "meta_analysis"
  return(result)
}
