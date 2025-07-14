#' Visualization Functions for Search Strategy Analysis
#'
#' This file contains all visualization functions used by the SearchAnalyzer class
#' and other components of the searchAnalyzeR package.

#' Create Overview Performance Plot
#'
#' @param metrics List of calculated metrics from SearchAnalyzer
#' @return ggplot object showing key performance indicators
#' @details
#' Creates a comprehensive overview plot displaying:
#' \itemize{
#'   \item Precision, Recall, and F1 Score (Accuracy category)
#'   \item Coverage metrics (Completeness category)
#'   \item Efficiency scores (Efficiency category)
#' }
#' The plot uses color coding to distinguish between metric categories and
#' displays exact values on top of each bar.
#' @examples
#' # Assume you have calculated metrics
#' metrics <- list(
#'   precision_recall = list(precision = 0.8, recall = 0.6, f1_score = 0.69),
#'   coverage = list(total_coverage = 0.75),
#'   efficiency = list(efficiency_score = 0.45)
#' )
#'
#' overview_plot <- plot_overview(metrics)
#' print(overview_plot)
#' @import ggplot2
#' @importFrom dplyr %>%
#' @importFrom tibble tibble
#' @seealso \code{\link{plot_pr_curve}}, \code{\link{plot_temporal}}
#' @export
plot_overview <- function(metrics) {
  # Extract key metrics for overview
  overview_data <- tibble::tibble(
    metric = c("Precision", "Recall", "F1 Score", "Coverage", "Efficiency"),
    value = c(
      metrics$precision_recall$precision,
      metrics$precision_recall$recall,
      metrics$precision_recall$f1_score,
      metrics$coverage$total_coverage,
      metrics$efficiency$efficiency_score
    ),
    category = c("Accuracy", "Accuracy", "Accuracy", "Completeness", "Efficiency")
  )

  ggplot2::ggplot(overview_data, ggplot2::aes(x = .data$metric, y = .data$value, fill = .data$category)) +
    ggplot2::geom_col(alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", .data$value)),
                       vjust = -0.5, size = 3.5) +
    ggplot2::scale_fill_manual(values = c("Accuracy" = "#2E86AB",
                                          "Completeness" = "#A23B72",
                                          "Efficiency" = "#F18F01")) +
    ggplot2::labs(
      title = "Search Strategy Performance Overview",
      subtitle = "Key performance metrics across different dimensions",
      x = "Metrics",
      y = "Score",
      fill = "Category"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12, color = "gray60"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    ggplot2::ylim(0, 1)
}

#' Create Precision-Recall Curve
#'
#' @param retrieved Vector of retrieved article IDs
#' @param relevant Vector of relevant article IDs
#' @param thresholds Vector of threshold values
#' @return ggplot object
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @export
plot_pr_curve <- function(retrieved, relevant, thresholds = seq(0, 1, 0.05)) {
  # Calculate precision and recall at different thresholds
  pr_data <- purrr::map_dfr(thresholds, function(threshold) {
    # Simulate ranking scores (in practice, these would come from actual search scores)
    n_retrieve <- round(length(retrieved) * (1 - threshold))
    if (n_retrieve == 0) n_retrieve <- 1

    subset_retrieved <- retrieved[1:min(n_retrieve, length(retrieved))]

    metrics <- calc_precision_recall(subset_retrieved, relevant)

    tibble::tibble(
      threshold = threshold,
      precision = metrics$precision,
      recall = metrics$recall,
      f1_score = metrics$f1_score
    )
  })

  # Create the plot
  ggplot2::ggplot(pr_data, ggplot2::aes(x = .data$recall, y = .data$precision)) +
    ggplot2::geom_line(color = "#2E86AB", size = 1.2) +
    ggplot2::geom_point(color = "#2E86AB", size = 2, alpha = 0.7) +
    ggplot2::geom_area(alpha = 0.2, fill = "#2E86AB") +
    ggplot2::labs(
      title = "Precision-Recall Curve",
      subtitle = "Trade-off between precision and recall at different thresholds",
      x = "Recall",
      y = "Precision"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12, color = "gray60")
    ) +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
}

#' Create Temporal Coverage Plot
#'
#' @param search_results Data frame with search results including date column
#' @param gold_standard Vector of relevant article IDs
#' @return ggplot object
#' @importFrom dplyr mutate count group_by summarise filter
#' @importFrom tidyr complete
#' @importFrom lubridate year
#' @export
plot_temporal <- function(search_results, gold_standard = NULL) {
  # Prepare temporal data
  temporal_data <- search_results %>%
    dplyr::mutate(
      date = as.Date(.data$date),
      year = lubridate::year(.data$date),
      is_relevant = if (!is.null(gold_standard)) .data$id %in% gold_standard else FALSE
    ) %>%
    dplyr::count(.data$year, .data$is_relevant) %>%
    tidyr::complete(.data$year, .data$is_relevant, fill = list(n = 0))

  # Create stacked bar chart
  p <- ggplot2::ggplot(temporal_data, ggplot2::aes(x = .data$year, y = .data$n, fill = .data$is_relevant)) +
    ggplot2::geom_col(alpha = 0.8) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#2E86AB", "FALSE" = "#E8E8E8"),
      labels = c("TRUE" = "Relevant", "FALSE" = "Not Relevant"),
      name = "Relevance"
    ) +
    ggplot2::labs(
      title = "Temporal Distribution of Search Results",
      subtitle = "Distribution of retrieved articles by publication year",
      x = "Publication Year",
      y = "Number of Articles"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12, color = "gray60"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )

  # Add relevance rate if gold standard is provided
  if (!is.null(gold_standard)) {
    relevance_data <- temporal_data %>%
      dplyr::group_by(.data$year) %>%
      dplyr::summarise(
        total = sum(.data$n),
        relevant = sum(.data$n[.data$is_relevant]),
        rate = .data$relevant / .data$total,
        .groups = "drop"
      ) %>%
      dplyr::filter(.data$total > 0)

    p <- p +
      ggplot2::geom_line(data = relevance_data,
                         ggplot2::aes(x = .data$year, y = .data$rate * max(temporal_data$n), color = "Relevance Rate"),
                         inherit.aes = FALSE, size = 1.2) +
      ggplot2::scale_color_manual(values = c("Relevance Rate" = "#F18F01")) +
      ggplot2::guides(color = ggplot2::guide_legend(title = ""))
  }

  return(p)
}

#' Create Database Performance Comparison
#'
#' @param results_by_database List of result sets by database
#' @param gold_standard Vector of relevant article IDs
#' @return ggplot object
#' @importFrom purrr map_dfr
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer
#' @export
plot_db_performance <- function(results_by_database, gold_standard = NULL) {
  # Calculate metrics for each database
  db_metrics <- purrr::map_dfr(results_by_database, function(db_results) {
    if (!is.null(gold_standard)) {
      metrics <- calc_precision_recall(db_results, gold_standard)
      tibble::tibble(
        total_results = length(db_results),
        relevant_results = metrics$true_positives,
        precision = metrics$precision,
        recall = metrics$recall,
        f1_score = metrics$f1_score
      )
    } else {
      tibble::tibble(
        total_results = length(db_results),
        relevant_results = NA,
        precision = NA,
        recall = NA,
        f1_score = NA
      )
    }
  }, .id = "database")

  # Create comparison plot
  if (!is.null(gold_standard)) {
    # Multi-metric comparison
    db_long <- db_metrics %>%
      dplyr::select(.data$database, .data$precision, .data$recall, .data$f1_score) %>%
      tidyr::pivot_longer(cols = c("precision", "recall", "f1_score"),
                          names_to = "metric", values_to = "value")

    ggplot2::ggplot(db_long, ggplot2::aes(x = .data$database, y = .data$value, fill = .data$metric)) +
      ggplot2::geom_col(position = "dodge", alpha = 0.8) +
      ggplot2::scale_fill_manual(values = c("precision" = "#2E86AB",
                                            "recall" = "#A23B72",
                                            "f1_score" = "#F18F01")) +
      ggplot2::labs(
        title = "Database Performance Comparison",
        subtitle = "Precision, recall, and F1 score by database",
        x = "Database",
        y = "Score",
        fill = "Metric"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12, color = "gray60"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      ) +
      ggplot2::ylim(0, 1)
  } else {
    # Just show result counts
    ggplot2::ggplot(db_metrics, ggplot2::aes(x = .data$database, y = .data$total_results)) +
      ggplot2::geom_col(fill = "#2E86AB", alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = .data$total_results), vjust = -0.5) +
      ggplot2::labs(
        title = "Database Result Counts",
        subtitle = "Number of results retrieved from each database",
        x = "Database",
        y = "Number of Results"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12, color = "gray60"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }
}

#' Create Keyword Effectiveness Analysis Plot
#'
#' @param search_results Data frame with search results
#' @param search_terms Vector of search terms
#' @param gold_standard Vector of relevant article IDs
#' @return ggplot object
#' @importFrom purrr map_dfr
#' @importFrom stringr str_detect
#' @importFrom dplyr filter pull
#' @importFrom stats reorder
#' @export
plot_keyword_eff <- function(search_results, search_terms, gold_standard = NULL) {
  # Analyze keyword effectiveness
  keyword_stats <- purrr::map_dfr(search_terms, function(term) {
    # Find articles containing this term
    containing_term <- search_results %>%
      dplyr::filter(stringr::str_detect(tolower(paste(.data$title, .data$abstract, sep = " ")),
                                        tolower(term)))

    if (!is.null(gold_standard)) {
      relevant_with_term <- intersect(containing_term$id, gold_standard)
      tibble::tibble(
        term = term,
        total_articles = nrow(containing_term),
        relevant_articles = length(relevant_with_term),
        precision = length(relevant_with_term) / nrow(containing_term),
        coverage = length(relevant_with_term) / length(gold_standard)
      )
    } else {
      tibble::tibble(
        term = term,
        total_articles = nrow(containing_term),
        relevant_articles = NA,
        precision = NA,
        coverage = NA
      )
    }
  })

  # Create effectiveness plot
  if (!is.null(gold_standard)) {
    # Bubble plot showing precision vs coverage
    ggplot2::ggplot(keyword_stats, ggplot2::aes(x = .data$coverage, y = .data$precision, size = .data$total_articles)) +
      ggplot2::geom_point(alpha = 0.7, color = "#2E86AB") +
      ggplot2::geom_text(ggplot2::aes(label = .data$term), hjust = 0, vjust = 0, size = 3, nudge_x = 0.01) +
      ggplot2::scale_size_continuous(range = c(2, 10), name = "Total Articles") +
      ggplot2::labs(
        title = "Keyword Effectiveness Analysis",
        subtitle = "Precision vs Coverage for each search term",
        x = "Coverage (Proportion of Relevant Articles Retrieved)",
        y = "Precision (Proportion of Retrieved Articles that are Relevant)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12, color = "gray60"),
        legend.position = "right"
      ) +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
  } else {
    # Just show article counts
    ggplot2::ggplot(keyword_stats, ggplot2::aes(x = stats::reorder(.data$term, .data$total_articles), y = .data$total_articles)) +
      ggplot2::geom_col(fill = "#2E86AB", alpha = 0.8) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Keyword Retrieval Counts",
        subtitle = "Number of articles retrieved for each search term",
        x = "Search Term",
        y = "Number of Articles"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12, color = "gray60")
      )
  }
}

#' Create PRISMA Flow Diagram with Proper Spacing and Text Enclosure
#'
#' @param flow_data List containing PRISMA flow numbers
#' @return ggplot object
#' @importFrom tibble tibble
#' @export
create_prisma <- function(flow_data) {
  # Carefully designed PRISMA flow with proper spacing and text containment

  boxes <- tibble::tibble(
    id = 1:7,
    # Main flow boxes positioned vertically with good spacing
    # Exclusion boxes positioned to the right with adequate separation
    x = c(5, 5, 5, 5, 5, 12, 12),  # Main at x=5, exclusions far right at x=12
    y = c(12, 10, 8, 6, 4, 8, 6),   # Main flow: 12,10,8,6,4 (2-unit gaps), exclusions align with relevant boxes
    # Much wider boxes and taller for specific boxes that need more space
    width = c(9, 9, 9, 9, 9, 6, 6),    # Wider main boxes (from 7 to 9), wider exclusion boxes (from 5 to 6)
    height = c(1.8, 1.5, 1.5, 1.8, 1.8, 1.2, 1.2),  # Taller for boxes 1, 4, and 5 (identification, full-text, inclusion)
    text = c(
      paste("Records identified through\ndatabase searching\n(n =", flow_data$identified, ")"),
      paste("Records after duplicates removed\n(n =", flow_data$after_duplicates, ")"),
      paste("Records screened\n(n =", flow_data$title_abstract_screened, ")"),
      paste("Full-text articles assessed\nfor eligibility\n(n =", flow_data$full_text_eligible, ")"),
      paste("Studies included in\nqualitative synthesis\n(n =", flow_data$included, ")"),
      paste("Records excluded\n(n =", flow_data$excluded_title_abstract, ")"),
      paste("Full-text articles excluded\n(n =", flow_data$excluded_full_text, ")")
    ),
    fill = c(rep("#E8F4FD", 5), rep("#FFF2E8", 2))
  )

  # Vertical arrows connecting main flow boxes
  # Positioned to connect box bottoms to box tops with clear spacing
  arrows <- tibble::tibble(
    x = c(5, 5, 5, 5),
    y = c(11.25, 9.25, 7.25, 5.25),    # Start from bottom of each box (y - height/2)
    xend = c(5, 5, 5, 5),
    yend = c(10.75, 8.75, 6.75, 4.75)  # End at top of next box (y + height/2)
  )

  # Horizontal arrows from main boxes to exclusion boxes
  # Connect from right edge of main boxes to left edge of exclusion boxes
  exclusion_arrows <- tibble::tibble(
    x = c(9.5, 9.5),        # Start from right edge of main boxes (5 + 9/2 = 9.5)
    y = c(8, 6),            # At height of screening and full-text boxes
    xend = c(9, 9),         # End before exclusion boxes (12 - 6/2 = 9)
    yend = c(8, 6)
  )

  # Create the plot with proper canvas size and spacing
  plot <- ggplot2::ggplot() +
    # Draw rectangular boxes
    ggplot2::geom_rect(
      data = boxes,
      ggplot2::aes(
        xmin = .data$x - .data$width/2,
        xmax = .data$x + .data$width/2,
        ymin = .data$y - .data$height/2,
        ymax = .data$y + .data$height/2,
        fill = .data$fill
      ),
      color = "black",
      size = 0.8,
      alpha = 1.0
    ) +
    # Add text inside boxes
    ggplot2::geom_text(
      data = boxes,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$text),
      size = 3.0,           # Moderate text size to fit in boxes
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 0.9,     # Good line spacing
      color = "black"
    ) +
    # Vertical flow arrows
    ggplot2::geom_segment(
      data = arrows,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm")),
      size = 0.6,
      color = "black"
    ) +
    # Horizontal exclusion arrows
    ggplot2::geom_segment(
      data = exclusion_arrows,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm")),
      size = 0.6,
      color = "red"
    ) +
    # Styling
    ggplot2::scale_fill_identity() +
    ggplot2::labs(
      title = "PRISMA Flow Diagram",
      subtitle = "Systematic review search and screening process"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, color = "gray60", hjust = 0.5),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    ) +
    # Set coordinate limits with adequate space around all elements
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::xlim(-1, 17) +    # Wider canvas for the larger boxes (main boxes now extend to 9.5, exclusions to 15)
    ggplot2::ylim(2, 14)       # Same vertical space

  return(plot)
}

#' Create Sensitivity Analysis Heatmap
#'
#' @param sensitivity_results Results from sensitivity analysis
#' @return ggplot object
#' @importFrom dplyr select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_remove
#' @importFrom purrr pmap_chr
#' @export
plot_sensitivity <- function(sensitivity_results) {
  # Prepare data for heatmap
  if ("sensitivity_analysis" %in% class(sensitivity_results)) {
    # Reshape for heatmap
    param_cols <- setdiff(names(sensitivity_results),
                          c("mean_precision", "mean_recall", "mean_f1"))

    # Create parameter combination labels
    sensitivity_results <- sensitivity_results %>%
      dplyr::mutate(
        param_combo = purrr::pmap_chr(dplyr::select(., dplyr::all_of(param_cols)),
                                      function(...) paste(paste(names(list(...)), list(...), sep = "="),
                                                          collapse = "; "))
      )

    # Reshape for plotting
    plot_data <- sensitivity_results %>%
      dplyr::select(.data$param_combo, .data$mean_precision, .data$mean_recall, .data$mean_f1) %>%
      tidyr::pivot_longer(cols = c("mean_precision", "mean_recall", "mean_f1"),
                          names_to = "metric", values_to = "value") %>%
      dplyr::mutate(metric = stringr::str_remove(.data$metric, "mean_"))

    ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$metric, y = .data$param_combo, fill = .data$value)) +
      ggplot2::geom_tile(color = "white", size = 0.5) +
      ggplot2::scale_fill_gradient2(low = "#F8F8F8", mid = "#A23B72", high = "#2E86AB",
                                    midpoint = 0.5, name = "Score") +
      ggplot2::labs(
        title = "Sensitivity Analysis Results",
        subtitle = "Performance metrics across different parameter combinations",
        x = "Metric",
        y = "Parameter Combination"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12, color = "gray60"),
        axis.text.y = ggplot2::element_text(size = 8),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }
}
