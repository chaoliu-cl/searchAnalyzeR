#' PRISMA-Compliant Report Generator
#'
#' @description
#' A comprehensive reporting system for generating PRISMA-compliant reports
#' from systematic review search analyses.
#'
#' @details
#' The PRISMAReporter class provides tools for:
#' \itemize{
#'   \item Generating comprehensive search strategy reports
#'   \item Creating PRISMA flow diagrams
#'   \item Documenting search strategies
#'   \item Exporting reports in multiple formats (HTML, PDF, Word)
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{Initialize a new PRISMAReporter instance}
#'   \item{\code{generate_report(search_analysis, output_format, template_type)}}{Generate comprehensive search strategy report}
#'   \item{\code{generate_prisma_diagram(screening_data)}}{Generate PRISMA flow diagram}
#'   \item{\code{document_search_strategy(search_strategy)}}{Generate search strategy documentation}
#' }
#'
#' @examples
#' # Create reporter
#' reporter <- PRISMAReporter$new()
#'
#' # Create sample search strategy for documentation
#' search_strategy <- list(
#'   terms = c("systematic review", "meta-analysis", "evidence synthesis"),
#'   databases = c("PubMed", "Embase", "Cochrane"),
#'   date_range = as.Date(c("2020-01-01", "2023-12-31")),
#'   filters = list(language = "English", study_type = "RCT")
#' )
#'
#' # Generate search strategy documentation
#' strategy_docs <- reporter$document_search_strategy(search_strategy)
#' print(strategy_docs)
#'
#' # Create sample screening data for PRISMA diagram
#' screening_data <- data.frame(
#'   id = 1:100,
#'   duplicate = c(rep(FALSE, 80), rep(TRUE, 20)),
#'   title_abstract_screened = c(rep(TRUE, 80), rep(FALSE, 20)),
#'   full_text_eligible = c(rep(TRUE, 25), rep(FALSE, 75)),
#'   included = c(rep(TRUE, 15), rep(FALSE, 85)),
#'   excluded_title_abstract = c(rep(FALSE, 25), rep(TRUE, 55), rep(FALSE, 20)),
#'   excluded_full_text = c(rep(FALSE, 15), rep(TRUE, 10), rep(FALSE, 75))
#' )
#'
#' # Generate PRISMA diagram
#' prisma_plot <- reporter$generate_prisma_diagram(screening_data)
#' print("PRISMA diagram created successfully")
#'
#' @export
PRISMAReporter <- R6::R6Class(
  "PRISMAReporter",
  public = list(
    #' @description
    #' Creates a new PRISMAReporter instance for generating PRISMA-compliant reports.
    #' Sets up the necessary template paths and configuration.
    #' @return No return value, called for side effects (initialization)
    initialize = function() {
      # Initialize any necessary configurations
      private$template_dir <- system.file("templates", package = "searchAnalyzeR")
      private$output_formats <- c("html", "pdf", "word")
      invisible(self)
    },

    #' Generate comprehensive search strategy report
    #' @param search_analysis SearchAnalyzer object
    #' @param output_format Output format ("html", "pdf", "word")
    #' @param template_type Type of report template
    #' @return Path to generated report
    generate_report = function(search_analysis, output_format = "html", template_type = "comprehensive") {
      template_path <- private$get_template_path(template_type)

      # Prepare report data
      report_data <- list(
        metadata = search_analysis$metadata,
        metrics = search_analysis$calculate_metrics(),
        timestamp = Sys.time(),
        package_version = utils::packageVersion("searchAnalyzeR")
      )

      # Generate visualizations
      plots <- list(
        overview = search_analysis$visualize_performance("overview"),
        precision_recall = search_analysis$visualize_performance("precision_recall"),
        temporal = search_analysis$visualize_performance("temporal")
      )

      # Render report to tempdir() by default
      output_file <- private$render_report(template_path, report_data, plots, output_format)

      return(output_file)
    },

    #' Generate PRISMA flow diagram
    #' @param screening_data Data frame with screening results
    #' @return ggplot object
    generate_prisma_diagram = function(screening_data) {
      # Extract PRISMA flow numbers
      flow_data <- private$extract_prisma_numbers(screening_data)

      # Create PRISMA flow diagram
      private$create_prisma(flow_data)
    },

    #' Generate search strategy documentation
    #' @param search_strategy Search strategy object
    #' @return Formatted documentation
    document_search_strategy = function(search_strategy) {
      documentation <- list(
        search_terms = private$format_search_terms(search_strategy$terms),
        databases = private$format_databases(search_strategy$databases),
        filters = private$format_filters(search_strategy$filters),
        execution_details = private$format_execution_details(search_strategy)
      )

      class(documentation) <- "search_documentation"
      return(documentation)
    }
  ),

  private = list(
    template_dir = NULL,
    output_formats = NULL,

    get_template_path = function(template_type) {
      system.file("templates", paste0(template_type, "_report.Rmd"),
                  package = "searchAnalyzeR")
    },

    render_report = function(template_path, data, plots, format) {
      # Check if rmarkdown is available
      if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        warning("rmarkdown package not available - creating simple text report")
        return(private$create_simple_report(data, plots, format))
      }

      # Create temporary environment with data and plots
      report_env <- new.env()
      list2env(data, envir = report_env)
      assign("plots", plots, envir = report_env)

      # Generate output filename in tempdir()
      output_file <- tempfile(fileext = paste0(".", format))

      # Render with rmarkdown
      tryCatch({
        rmarkdown::render(
          input = template_path,
          output_file = output_file,
          output_format = switch(format,
                                 "html" = "html_document",
                                 "pdf" = "pdf_document",
                                 "word" = "word_document"
          ),
          envir = report_env,
          quiet = TRUE
        )
      }, error = function(e) {
        warning("Could not render with rmarkdown: ", e$message)
        return(private$create_simple_report(data, plots, format))
      })

      return(output_file)
    },

    create_simple_report = function(data, plots, format) {
      # Fallback simple report creation - writes to tempdir()
      output_file <- tempfile(fileext = ".txt")

      report_lines <- c(
        "# Search Analysis Report",
        paste("Generated:", Sys.time()),
        "",
        "## Metrics Summary",
        paste("- Package Version:", data$package_version),
        "",
        "## Analysis Results",
        "Please use rmarkdown package for full report generation."
      )

      writeLines(report_lines, output_file)
      return(output_file)
    },

    extract_prisma_numbers = function(screening_data) {
      list(
        identified = nrow(screening_data),
        after_duplicates = sum(!screening_data$duplicate),
        title_abstract_screened = sum(screening_data$title_abstract_screened),
        full_text_eligible = sum(screening_data$full_text_eligible),
        included = sum(screening_data$included),
        excluded_title_abstract = sum(screening_data$excluded_title_abstract),
        excluded_full_text = sum(screening_data$excluded_full_text)
      )
    },

    create_prisma = function(flow_data) {
      # This would call the create_prisma function from visualization-functions.R
      create_prisma(flow_data)
    },

    format_search_terms = function(terms) {
      # Format search terms for documentation
      paste(terms, collapse = " OR ")
    },

    format_databases = function(databases) {
      # Format database list for documentation
      paste(databases, collapse = ", ")
    },

    format_filters = function(filters) {
      # Format filters for documentation
      if (is.null(filters)) return("No additional filters applied")
      paste(names(filters), filters, sep = ": ", collapse = "; ")
    },

    format_execution_details = function(strategy) {
      # Format execution details
      list(
        timestamp = strategy$timestamp,
        total_terms = length(strategy$terms),
        total_databases = length(strategy$databases)
      )
    }
  )
)
