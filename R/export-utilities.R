#' Export Utilities for Search Analysis Results
#'
#' This file contains functions for exporting search analysis results,
#' reports, and data in various formats.

#' Export Search Results to Multiple Formats
#'
#' @param search_results Data frame with search results
#' @param file_path Base file path (without extension). If NULL, uses tempdir()
#' @param formats Vector of formats to export ("csv", "xlsx", "ris", "bibtex")
#' @param include_metadata Logical, whether to include metadata sheets/files
#' @return Vector of created file paths
#' @details
#' This function exports search results to multiple standard formats used in
#' systematic reviews and reference management. Supported formats include:
#' \itemize{
#'   \item \strong{CSV}: Comma-separated values for data analysis
#'   \item \strong{Excel}: Multi-sheet workbook with metadata
#'   \item \strong{RIS}: Reference Information Systems format for reference managers
#'   \item \strong{BibTeX}: LaTeX bibliography format
#'   \item \strong{EndNote}: Thomson Reuters EndNote format
#' }
#' @examples
#' # Create sample search results
#' search_results <- data.frame(
#'   id = paste0("article_", 1:5),
#'   title = paste("Sample Article", 1:5),
#'   abstract = paste("Abstract for article", 1:5),
#'   source = "Sample Journal",
#'   date = Sys.Date(),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Export to multiple formats (writes to tempdir())
#' output_files <- export_results(search_results, formats = c("csv", "xlsx"))
#' print(output_files)
#' @export
export_results <- function(search_results, file_path = NULL,
                           formats = c("csv", "xlsx"),
                           include_metadata = TRUE) {

  # Use tempdir() by default to comply with CRAN policies
  if (is.null(file_path)) {
    file_path <- file.path(tempdir(), "search_results")
  }

  created_files <- c()

  for (format in formats) {
    output_file <- switch(format,
                          "csv" = export_to_csv(search_results, paste0(file_path, ".csv"), include_metadata),
                          "xlsx" = export_to_xlsx(search_results, paste0(file_path, ".xlsx"), include_metadata),
                          "ris" = export_to_ris(search_results, paste0(file_path, ".ris")),
                          "bibtex" = export_to_bibtex(search_results, paste0(file_path, ".bib")),
                          "endnote" = export_to_endnote(search_results, paste0(file_path, ".enw")),
                          stop("Unsupported export format: ", format)
    )

    if (!is.null(output_file)) {
      created_files <- c(created_files, output_file)
    }
  }

  return(created_files)
}

#' Export to CSV Format
#'
#' @param search_results Data frame with search results
#' @param file_path Output file path
#' @param include_metadata Logical, whether to create metadata file
#' @return File path of created file
#' @importFrom utils write.csv
export_to_csv <- function(search_results, file_path, include_metadata = TRUE) {
  # Export main data
  utils::write.csv(search_results, file_path, row.names = FALSE)

  # Export metadata if requested - FIXED: Handle vector values properly
  if (include_metadata && !is.null(attr(search_results, "merge_info"))) {
    metadata_path <- gsub("\\.csv$", "_metadata.csv", file_path)

    metadata_df <- data.frame(
      attribute = names(attr(search_results, "merge_info")),
      value = sapply(attr(search_results, "merge_info"), function(x) {
        # FIXED: Convert vectors and complex objects to single strings
        if (is.vector(x) && length(x) > 1) {
          paste(x, collapse = ", ")
        } else if (is.list(x)) {
          paste(names(x), x, sep = "=", collapse = "; ")
        } else {
          as.character(x)
        }
      }),
      stringsAsFactors = FALSE
    )

    utils::write.csv(metadata_df, metadata_path, row.names = FALSE)
  }

  return(file_path)
}

#' Export to Excel Format with Multiple Sheets
#'
#' @param search_results Data frame with search results
#' @param file_path Output file path
#' @param include_metadata Logical, whether to include metadata sheets
#' @return File path of created file
#' @importFrom openxlsx createWorkbook addWorksheet writeData addFilter freezePane createStyle addStyle saveWorkbook
export_to_xlsx <- function(search_results, file_path, include_metadata = TRUE) {
  # Create workbook
  wb <- openxlsx::createWorkbook()

  # Add main data sheet
  openxlsx::addWorksheet(wb, "Search Results")
  openxlsx::writeData(wb, "Search Results", search_results)

  # Format the main sheet
  openxlsx::addFilter(wb, "Search Results", row = 1, cols = 1:ncol(search_results))
  openxlsx::freezePane(wb, "Search Results", firstRow = TRUE)

  # Style headers
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#4F81BD",
    fontColour = "white",
    border = "TopBottomLeftRight"
  )
  openxlsx::addStyle(wb, "Search Results", header_style, rows = 1, cols = 1:ncol(search_results))

  # Add metadata sheets if requested - FIXED: Handle complex data properly
  if (include_metadata) {
    # Merge info - FIXED: Convert complex values to simple data frame
    if (!is.null(attr(search_results, "merge_info"))) {
      merge_info <- attr(search_results, "merge_info")

      openxlsx::addWorksheet(wb, "Merge Info")
      merge_df <- data.frame(
        Attribute = names(merge_info),
        Value = sapply(merge_info, function(x) {
          # FIXED: Properly handle all data types for Excel
          if (is.vector(x) && length(x) > 1) {
            paste(x, collapse = ", ")
          } else if (is.list(x)) {
            paste(names(x), x, sep = "=", collapse = "; ")
          } else if (inherits(x, "POSIXct") || inherits(x, "Date")) {
            as.character(x)
          } else {
            as.character(x)
          }
        }),
        stringsAsFactors = FALSE
      )
      openxlsx::writeData(wb, "Merge Info", merge_df)
      openxlsx::addStyle(wb, "Merge Info", header_style, rows = 1, cols = 1:2)
    }

    # Duplicate summary - This should work as-is since it's already a data frame
    if (!is.null(attr(search_results, "duplicate_summary"))) {
      openxlsx::addWorksheet(wb, "Duplicate Summary")
      openxlsx::writeData(wb, "Duplicate Summary", attr(search_results, "duplicate_summary"))
      openxlsx::addStyle(wb, "Duplicate Summary", header_style, rows = 1, cols = 1:2)
    }

    # Search statistics - FIXED: Create simple data frame
    if (exists("calc_search_stats")) {
      stats <- calc_search_stats(search_results)

      openxlsx::addWorksheet(wb, "Statistics")
      # FIXED: Convert complex stats to simple key-value pairs
      stats_df <- data.frame(
        Statistic = c("Total Records", "Unique Records", "Duplicates",
                      "Missing Abstracts", "Missing Dates", "Date Range"),
        Value = c(
          as.character(stats$total_records),
          as.character(stats$unique_records),
          as.character(stats$duplicates),
          as.character(stats$missing_abstracts),
          as.character(stats$missing_dates),
          paste(as.character(stats$date_range), collapse = " to ")
        ),
        stringsAsFactors = FALSE
      )
      openxlsx::writeData(wb, "Statistics", stats_df)
      openxlsx::addStyle(wb, "Statistics", header_style, rows = 1, cols = 1:2)
    }
  }

  # Save workbook
  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)

  return(file_path)
}

#' Export to RIS Format
#'
#' @param search_results Data frame with search results
#' @param file_path Output file path
#' @return File path of created file
#' @importFrom lubridate year
export_to_ris <- function(search_results, file_path) {
  # Convert to RIS format
  ris_content <- sapply(seq_len(nrow(search_results)), function(i) {
    row <- search_results[i, ]

    ris_entry <- c(
      "TY  - JOUR",  # Assuming journal articles
      paste0("TI  - ", row$title),
      if (!is.na(row$abstract)) paste0("AB  - ", row$abstract) else NULL,
      if (!is.na(row$source)) paste0("JF  - ", row$source) else NULL,
      if (!is.na(row$date)) paste0("PY  - ", lubridate::year(row$date)) else NULL,
      if ("doi" %in% names(row) && !is.na(row$doi)) paste0("DO  - ", row$doi) else NULL,
      if ("authors" %in% names(row) && !is.na(row$authors)) {
        # Split authors and create AU entries
        authors <- strsplit(row$authors, ";|,")[[1]]
        sapply(trimws(authors), function(x) paste0("AU  - ", x))
      } else NULL,
      paste0("UR  - ", row$id),
      "ER  - "
    )

    paste(ris_entry, collapse = "\n")
  })

  # Write to file
  writeLines(ris_content, file_path)

  return(file_path)
}

#' Export to BibTeX Format
#'
#' @param search_results Data frame with search results
#' @param file_path Output file path
#' @return File path of created file
#' @importFrom lubridate year
export_to_bibtex <- function(search_results, file_path) {
  # Convert to BibTeX format
  bibtex_content <- sapply(seq_len(nrow(search_results)), function(i) {
    row <- search_results[i, ]

    # Create citation key
    citation_key <- gsub("[^A-Za-z0-9]", "", row$id)

    # Build BibTeX entry
    entry_lines <- c(
      paste0("@article{", citation_key, ","),
      paste0("  title = {", row$title, "},")
    )

    if (!is.na(row$abstract)) {
      entry_lines <- c(entry_lines, paste0("  abstract = {", row$abstract, "},"))
    }

    if (!is.na(row$source)) {
      entry_lines <- c(entry_lines, paste0("  journal = {", row$source, "},"))
    }

    if (!is.na(row$date)) {
      entry_lines <- c(entry_lines, paste0("  year = {", lubridate::year(row$date), "},"))
    }

    if ("doi" %in% names(row) && !is.na(row$doi)) {
      entry_lines <- c(entry_lines, paste0("  doi = {", row$doi, "},"))
    }

    if ("authors" %in% names(row) && !is.na(row$authors)) {
      entry_lines <- c(entry_lines, paste0("  author = {", row$authors, "},"))
    }

    # Remove trailing comma from last line and add closing brace
    entry_lines[length(entry_lines)] <- gsub(",$", "", entry_lines[length(entry_lines)])
    entry_lines <- c(entry_lines, "}")

    paste(entry_lines, collapse = "\n")
  })

  # Write to file
  writeLines(bibtex_content, file_path)

  return(file_path)
}

#' Export to EndNote Format
#'
#' @param search_results Data frame with search results
#' @param file_path Output file path
#' @return File path of created file
#' @importFrom lubridate year
export_to_endnote <- function(search_results, file_path) {
  # Convert to EndNote format
  endnote_content <- sapply(seq_len(nrow(search_results)), function(i) {
    row <- search_results[i, ]

    entry_lines <- c(
      "%0 Journal Article",
      paste0("%T ", row$title)
    )

    if (!is.na(row$abstract)) {
      entry_lines <- c(entry_lines, paste0("%X ", row$abstract))
    }

    if (!is.na(row$source)) {
      entry_lines <- c(entry_lines, paste0("%J ", row$source))
    }

    if (!is.na(row$date)) {
      entry_lines <- c(entry_lines, paste0("%D ", lubridate::year(row$date)))
    }

    if ("doi" %in% names(row) && !is.na(row$doi)) {
      entry_lines <- c(entry_lines, paste0("%R ", row$doi))
    }

    if ("authors" %in% names(row) && !is.na(row$authors)) {
      authors <- strsplit(row$authors, ";|,")[[1]]
      for (author in trimws(authors)) {
        entry_lines <- c(entry_lines, paste0("%A ", author))
      }
    }

    paste(entry_lines, collapse = "\n")
  })

  # Write to file
  writeLines(endnote_content, file_path)

  return(file_path)
}

#' Export Analysis Metrics
#'
#' @param metrics List of calculated metrics
#' @param file_path Output file path
#' @param format Export format ("csv", "xlsx", "json")
#' @return File path of created file
#' @examples
#' # Create sample metrics
#' metrics <- list(
#'   basic = list(total_records = 100, unique_records = 95),
#'   precision_recall = list(precision = 0.8, recall = 0.6, f1_score = 0.69)
#' )
#'
#' # Export metrics (writes to tempdir())
#' output_file <- export_metrics(metrics, file.path(tempdir(), "metrics.xlsx"))
#' print(output_file)
#' @export
export_metrics <- function(metrics, file_path, format = "xlsx") {
  # FIXED: Add proper input validation
  if (is.null(metrics)) {
    stop("Metrics cannot be NULL")
  }

  if (!is.list(metrics)) {
    stop("Metrics must be a list")
  }

  if (length(metrics) == 0) {
    warning("Metrics list is empty")
  }

  # Use fixed functions
  result <- switch(format,
                   "csv" = export_metrics_csv(metrics, file_path),  # This one works
                   "xlsx" = export_metrics_xlsx(metrics, file_path),  # Fixed version
                   "json" = export_metrics_json(metrics, file_path),  # Fixed version
                   stop("Unsupported export format: ", format)
  )

  return(result)
}

#' Export Metrics to CSV
#'
#' @param metrics List of calculated metrics
#' @param file_path Output file path
#' @return File path of created file
#' @importFrom utils write.csv
export_metrics_csv <- function(metrics, file_path) {
  # Flatten metrics into a data frame
  metrics_df <- data.frame(
    metric_category = character(),
    metric_name = character(),
    metric_value = character(),
    stringsAsFactors = FALSE
  )

  for (category in names(metrics)) {
    category_metrics <- metrics[[category]]
    if (is.list(category_metrics)) {
      for (metric_name in names(category_metrics)) {
        metrics_df <- rbind(metrics_df, data.frame(
          metric_category = category,
          metric_name = metric_name,
          metric_value = as.character(category_metrics[[metric_name]]),
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  utils::write.csv(metrics_df, file_path, row.names = FALSE)
  return(file_path)
}

#' Export Metrics to Excel
#'
#' @param metrics List of calculated metrics
#' @param file_path Output file path
#' @return File path of created file
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle saveWorkbook
export_metrics_xlsx <- function(metrics, file_path) {
  wb <- openxlsx::createWorkbook()

  # Create header style
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#4F81BD",
    fontColour = "white"
  )

  # Create sheets for each metric category - FIXED: Handle complex data properly
  for (category in names(metrics)) {
    category_metrics <- metrics[[category]]

    if (is.list(category_metrics) && length(category_metrics) > 0) {
      openxlsx::addWorksheet(wb, category)

      # FIXED: Always convert to simple data frame that Excel can handle
      category_df <- tryCatch({
        # Check if all elements are simple scalars
        all_scalar <- all(sapply(category_metrics, function(x) {
          length(x) == 1 && !is.list(x) && !is.data.frame(x)
        }))

        if (all_scalar) {
          # Simple metrics - convert directly
          data.frame(
            Metric = names(category_metrics),
            Value = sapply(category_metrics, function(x) {
              if (is.numeric(x)) {
                format(x, digits = 4)
              } else {
                as.character(x)
              }
            }),
            stringsAsFactors = FALSE
          )
        } else {
          # Complex metrics - flatten to key-value pairs
          result_rows <- do.call(rbind, lapply(names(category_metrics), function(metric_name) {
            metric_value <- category_metrics[[metric_name]]

            if (is.list(metric_value) || length(metric_value) > 1) {
              # Convert complex values to descriptive strings
              value_str <- if (is.data.frame(metric_value)) {
                paste("Data frame:", nrow(metric_value), "rows,", ncol(metric_value), "cols")
              } else if (is.list(metric_value)) {
                paste("List with", length(metric_value), "elements:",
                      paste(names(metric_value), collapse = ", "))
              } else {
                paste(metric_value, collapse = ", ")
              }

              data.frame(
                Metric = metric_name,
                Value = value_str,
                stringsAsFactors = FALSE
              )
            } else {
              # Simple scalar value
              data.frame(
                Metric = metric_name,
                Value = if (is.numeric(metric_value)) {
                  format(metric_value, digits = 4)
                } else {
                  as.character(metric_value)
                },
                stringsAsFactors = FALSE
              )
            }
          }))

          result_rows
        }
      }, error = function(e) {
        # Fallback: Just list metric names and note they're complex
        data.frame(
          Metric = names(category_metrics),
          Value = sapply(category_metrics, function(x) {
            if (is.numeric(x) && length(x) == 1) {
              format(x, digits = 4)
            } else if (is.character(x) && length(x) == 1) {
              as.character(x)
            } else {
              paste("Complex data type:", class(x)[1])
            }
          }),
          stringsAsFactors = FALSE
        )
      })

      # Write the safely converted data
      openxlsx::writeData(wb, category, category_df)
      openxlsx::addStyle(wb, category, header_style, rows = 1, cols = 1:ncol(category_df))
    }
  }

  # Add summary sheet
  openxlsx::addWorksheet(wb, "Summary")
  summary_df <- data.frame(
    Category = names(metrics),
    Description = sapply(names(metrics), function(cat) {
      switch(cat,
             "basic" = "Basic search statistics",
             "precision_recall" = "Precision and recall metrics",
             "efficiency" = "Search efficiency measures",
             "coverage" = "Database coverage analysis",
             "temporal" = "Temporal distribution metrics",
             "Other metrics"
      )
    }),
    Metrics_Count = sapply(metrics, length),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "Summary", summary_df)
  openxlsx::addStyle(wb, "Summary", header_style, rows = 1, cols = 1:3)

  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  return(file_path)
}

#' Export Metrics to JSON
#'
#' @param metrics List of calculated metrics
#' @param file_path Output file path
#' @return File path of created file
export_metrics_json <- function(metrics, file_path) {
  # FIXED: Convert packageVersion to character before serialization
  export_data <- list(
    export_timestamp = as.character(Sys.time()),  # FIXED: Convert to character
    package_version = as.character(utils::packageVersion("searchAnalyzeR")),  # FIXED: Convert to character
    r_version = as.character(R.version.string),  # FIXED: Ensure it's character
    metrics = metrics
  )

  # FIXED: Also handle any other potential serialization issues in metrics
  export_data$metrics <- rapply(export_data$metrics, function(x) {
    if (inherits(x, c("POSIXct", "Date"))) {
      as.character(x)
    } else if (inherits(x, "numeric_version")) {
      as.character(x)
    } else {
      x
    }
  }, how = "replace")

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(export_data, file_path, pretty = TRUE, auto_unbox = TRUE)
  } else {
    # Fallback to base R JSON creation (simple)
    json_content <- paste0('{\n',
                           '  "export_timestamp": "', export_data$export_timestamp, '",\n',
                           '  "package_version": "', export_data$package_version, '",\n',
                           '  "r_version": "', export_data$r_version, '",\n',
                           '  "metrics": "Use jsonlite package for full metrics export"\n',
                           '}')
    writeLines(json_content, file_path)
  }

  return(file_path)
}

#' Create Data Package for Sharing
#'
#' @param search_results Data frame with search results
#' @param analysis_results List of analysis results
#' @param output_dir Directory to create the package (defaults to tempdir())
#' @param package_name Name of the package
#' @return Path to created package directory
#' @examples
#' # Create sample data
#' search_results <- data.frame(
#'   id = paste0("art", 1:10),
#'   title = paste("Study", 1:10),
#'   abstract = paste("Abstract", 1:10),
#'   source = "Journal",
#'   date = Sys.Date(),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Create data package (writes to tempdir())
#' package_path <- create_data_package(search_results)
#' print(package_path)
#' @export
create_data_package <- function(search_results, analysis_results = NULL,
                                output_dir = NULL, package_name = "search_analysis_package") {

  # Use tempdir() by default to comply with CRAN policies
  if (is.null(output_dir)) {
    output_dir <- tempdir()
  }

  # Create package directory
  package_dir <- file.path(output_dir, package_name)
  dir.create(package_dir, recursive = TRUE, showWarnings = FALSE)

  # Create subdirectories
  data_dir <- file.path(package_dir, "data")
  docs_dir <- file.path(package_dir, "documentation")
  code_dir <- file.path(package_dir, "code")

  dir.create(data_dir, showWarnings = FALSE)
  dir.create(docs_dir, showWarnings = FALSE)
  dir.create(code_dir, showWarnings = FALSE)

  # Export search results in multiple formats - use fixed functions
  base_path <- file.path(data_dir, "search_results")
  export_results(search_results, base_path,
                 formats = c("csv", "xlsx", "ris", "bibtex"))

  # Export analysis results if provided - use fixed functions
  if (!is.null(analysis_results)) {
    if ("metrics" %in% names(analysis_results)) {
      export_metrics(analysis_results$metrics,
                     file.path(data_dir, "analysis_metrics.xlsx"),
                     format = "xlsx")
    }

    # Save full analysis results as RDS
    saveRDS(analysis_results, file.path(data_dir, "analysis_results.rds"))
  }

  # Create README - use fixed function
  create_package_readme(package_dir, search_results, analysis_results)

  # Create data dictionary
  create_data_dictionary(file.path(docs_dir, "data_dictionary.csv"), search_results)

  # Create analysis script template
  create_analysis_template(file.path(code_dir, "analysis_template.R"))

  # Create manifest - use fixed function
  create_package_manifest(package_dir)

  return(package_dir)
}

#' Create Package README
#'
#' @param package_dir Package directory
#' @param search_results Search results data
#' @param analysis_results Analysis results
create_package_readme <- function(package_dir, search_results, analysis_results) {
  # FIXED: Build README content without problematic glue templates
  readme_lines <- c(
    "# Search Analysis Data Package",
    "",
    paste("Generated:", Sys.time()),
    paste("Package: searchAnalyzeR v", utils::packageVersion("searchAnalyzeR")),
    "",
    "## Contents",
    "",
    "### Data Files",
    "- `data/search_results.csv` - Main search results in CSV format",
    "- `data/search_results.xlsx` - Main search results in Excel format with metadata",
    "- `data/search_results.ris` - Search results in RIS format for reference managers",
    "- `data/search_results.bib` - Search results in BibTeX format"
  )

  # FIXED: Add analysis results files conditionally without glue
  if (!is.null(analysis_results)) {
    readme_lines <- c(readme_lines,
                      "- `data/analysis_metrics.xlsx` - Analysis metrics and performance measures",
                      "- `data/analysis_results.rds` - Complete analysis results (R format)"
    )
  }

  readme_lines <- c(readme_lines,
                    "",
                    "### Documentation",
                    "- `documentation/data_dictionary.csv` - Description of all data fields",
                    "- `README.md` - This file",
                    "",
                    "### Code",
                    "- `code/analysis_template.R` - Template R script for reproducing analysis",
                    "",
                    "## Dataset Summary",
                    "",
                    paste("- **Total Records**:", nrow(search_results))
  )

  # FIXED: Handle optional fields safely
  if ("duplicate" %in% names(search_results)) {
    unique_count <- sum(!search_results$duplicate, na.rm = TRUE)
    readme_lines <- c(readme_lines, paste("- **Unique Records**:", unique_count))
  } else {
    readme_lines <- c(readme_lines, "- **Unique Records**: Not calculated")
  }

  # FIXED: Handle date range safely
  date_range <- range(search_results$date, na.rm = TRUE)
  if (!any(is.na(date_range))) {
    readme_lines <- c(readme_lines, paste("- **Date Range**:", paste(date_range, collapse = " to ")))
  }

  readme_lines <- c(readme_lines,
                    paste("- **Sources**:", length(unique(search_results$source)), "unique sources"),
                    "",
                    "## Usage",
                    "",
                    "### Loading Data in R",
                    "```r",
                    "# Load search results",
                    "search_results <- read.csv(\"data/search_results.csv\")",
                    ""
  )

  # FIXED: Add analysis loading code conditionally without templates
  if (!is.null(analysis_results)) {
    readme_lines <- c(readme_lines,
                      "# Load analysis results",
                      "analysis_results <- readRDS(\"data/analysis_results.rds\")"
    )
  } else {
    readme_lines <- c(readme_lines, "# No analysis results included")
  }

  readme_lines <- c(readme_lines,
                    "```",
                    "",
                    "### Loading Data in Other Software",
                    "- **EndNote**: Import `data/search_results.enw`",
                    "- **Zotero/Mendeley**: Import `data/search_results.ris` or `data/search_results.bib`",
                    "- **Excel**: Open `data/search_results.xlsx`",
                    "",
                    "## Analysis Reproduction",
                    "",
                    "Run the analysis template script:",
                    "```r",
                    "source(\"code/analysis_template.R\")",
                    "```",
                    "",
                    "## Data Quality Notes",
                    ""
  )

  # FIXED: Add data quality notes safely
  if ("duplicate" %in% names(search_results)) {
    dup_count <- sum(search_results$duplicate, na.rm = TRUE)
    if (dup_count > 0) {
      readme_lines <- c(readme_lines, paste("- ", dup_count, "duplicate records have been identified"))
    } else {
      readme_lines <- c(readme_lines, "- No duplicate detection performed")
    }
  }

  readme_lines <- c(readme_lines,
                    paste("- ", sum(is.na(search_results$abstract)), "records missing abstracts"),
                    paste("- ", sum(is.na(search_results$date)), "records missing publication dates"),
                    "",
                    "## Contact",
                    "",
                    "For questions about this data package, please refer to the original search strategy documentation."
  )

  # Write README without using glue
  writeLines(readme_lines, file.path(package_dir, "README.md"))
}

#' Create Data Dictionary
#'
#' @param file_path Output file path
#' @param search_results Search results data
#' @importFrom utils write.csv head
create_data_dictionary <- function(file_path, search_results) {
  # Define column descriptions
  column_descriptions <- data.frame(
    column = c("id", "title", "abstract", "source", "date", "doi", "authors",
               "duplicate", "duplicate_group", "search_source", "mesh_terms",
               "emtree_terms", "keywords"),
    description = c("Unique identifier for each record",
                    "Article title",
                    "Article abstract",
                    "Publication source (journal, database)",
                    "Publication date",
                    "Digital Object Identifier",
                    "Author names",
                    "Flag indicating duplicate records",
                    "Group ID for duplicate records",
                    "Database or source of search",
                    "MeSH terms (PubMed)",
                    "Emtree terms (Embase)",
                    "Author keywords"),
    type = c("character", "character", "character", "character", "date",
             "character", "character", "logical", "integer", "character",
             "character", "character", "character"),
    required = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                 FALSE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Filter to only columns present in data
  present_columns <- column_descriptions[column_descriptions$column %in% names(search_results), ]

  # Add actual column information
  actual_info <- do.call(rbind, lapply(present_columns$column, function(col) {
    col_data <- search_results[[col]]
    data.frame(
      column = col,
      missing_count = sum(is.na(col_data)),
      missing_percent = round(sum(is.na(col_data)) / length(col_data) * 100, 2),
      unique_values = length(unique(col_data[!is.na(col_data)])),
      sample_values = paste(utils::head(unique(col_data[!is.na(col_data)]), 3), collapse = "; "),
      stringsAsFactors = FALSE
    )
  }))

  # Combine information
  data_dictionary <- merge(present_columns, actual_info, by = "column")

  utils::write.csv(data_dictionary, file_path, row.names = FALSE)
}

#' Create Analysis Template Script
#'
#' @param file_path Output file path
create_analysis_template <- function(file_path) {
  template_content <- '
# Search Analysis Template Script
# Generated by searchAnalyzeR package

library(searchAnalyzeR)

# Load the search results
search_results <- read.csv("../data/search_results.csv")

# Basic summary
cat("Dataset Summary:\\n")
cat("Total records:", nrow(search_results), "\\n")
cat("Date range:", paste(range(search_results$date, na.rm = TRUE), collapse = " to "), "\\n")
cat("Sources:", length(unique(search_results$source)), "\\n\\n")

# If you have a gold standard (known relevant articles), specify their IDs here
# gold_standard <- c("article_id_1", "article_id_2", "article_id_3")

# Initialize the search analyzer
# analyzer <- SearchAnalyzer$new(
#   search_results = search_results,
#   gold_standard = gold_standard  # Remove this line if no gold standard
# )

# Calculate performance metrics
# metrics <- analyzer$calculate_metrics()
# print(metrics)

# Create visualizations
# p1 <- analyzer$visualize_performance("overview")
# p2 <- analyzer$visualize_performance("temporal")

# Generate report
# reporter <- PRISMAReporter$new()
# report_path <- reporter$generate_report(analyzer, output_format = "html")
# cat("Report generated:", report_path, "\\n")

# Additional analysis examples:

# 1. Calculate search statistics
stats <- calc_search_stats(search_results)
print(stats)

# 2. Detect duplicates (if not already done)
# search_results_dedup <- detect_duplicates(search_results, method = "exact")

# 3. Export results in different formats
# export_results(search_results, "../output/processed_results",
#                       formats = c("csv", "xlsx", "ris"))

cat("Analysis template completed!\\n")
cat("Uncomment and modify the sections above to perform full analysis.\\n")
'

  writeLines(template_content, file_path)
}

#' Create Package Manifest
#'
#' @param package_dir Package directory
#' @importFrom utils write.csv packageVersion
create_package_manifest <- function(package_dir) {
  files <- list.files(package_dir, recursive = TRUE, full.names = TRUE)

  manifest <- do.call(rbind, lapply(files, function(file_path) {
    file_info <- file.info(file_path)

    # FIXED: Use basename() instead of regex to avoid Windows path issues
    relative_path <- basename(file_path)

    # For nested files, reconstruct path properly
    if (dirname(file_path) != package_dir) {
      # FIXED: Use file.path operations instead of string regex
      subdir <- basename(dirname(file_path))
      relative_path <- file.path(subdir, basename(file_path))
    }

    data.frame(
      file = relative_path,
      size_bytes = file_info$size,
      modified = file_info$mtime,
      checksum = digest::digest(file_path, file = TRUE),
      stringsAsFactors = FALSE
    )
  }))

  # Add package metadata
  package_info <- data.frame(
    attribute = c("created_date", "package_version", "r_version", "platform"),
    value = c(
      as.character(Sys.time()),
      as.character(utils::packageVersion("searchAnalyzeR")),  # FIXED: Convert to character
      as.character(R.version.string),
      as.character(Sys.info()["sysname"])
    ),
    stringsAsFactors = FALSE
  )

  # Write manifest
  utils::write.csv(manifest, file.path(package_dir, "MANIFEST.csv"), row.names = FALSE)
  utils::write.csv(package_info, file.path(package_dir, "PACKAGE_INFO.csv"), row.names = FALSE)
}

#' Export Validation Results
#'
#' @param validation_results Results from benchmark validation
#' @param file_path Output file path
#' @param format Export format ("xlsx", "csv", "json")
#' @return File path of created file
#' @examples
#' # Create sample validation results
#' validation_results <- list(
#'   precision = 0.8,
#'   recall = 0.6,
#'   f1_score = 0.69,
#'   true_positives = 24,
#'   false_positives = 6,
#'   false_negatives = 16
#' )
#'
#' # Export validation results (writes to tempdir())
#' output_file <- export_validation(
#'   validation_results,
#'   file.path(tempdir(), "validation.xlsx")
#' )
#' print(output_file)
#' @export
export_validation <- function(validation_results, file_path, format = "xlsx") {
  switch(format,
         "xlsx" = export_validation_xlsx(validation_results, file_path),
         "csv" = export_validation_csv(validation_results, file_path),
         "json" = export_validation_json(validation_results, file_path),
         stop("Unsupported export format: ", format)
  )
}

#' Export Validation Results to Excel
#'
#' @param validation_results Validation results
#' @param file_path Output file path
#' @return File path of created file
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle saveWorkbook
export_validation_xlsx <- function(validation_results, file_path) {
  wb <- openxlsx::createWorkbook()

  # Header style
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fgFill = "#4F81BD",
    fontColour = "white"
  )

  # If it's a list of validation results (multiple benchmarks)
  if (is.list(validation_results) &&
      !all(c("precision", "recall", "f1_score") %in% names(validation_results))) {

    # Summary sheet
    openxlsx::addWorksheet(wb, "Summary")

    summary_df <- do.call(rbind, lapply(names(validation_results), function(name) {
      result <- validation_results[[name]]
      data.frame(
        benchmark = name,
        precision = result$precision,
        recall = result$recall,
        f1_score = result$f1_score,
        true_positives = result$true_positives,
        false_positives = result$false_positives,
        false_negatives = result$false_negatives,
        stringsAsFactors = FALSE
      )
    }))

    openxlsx::writeData(wb, "Summary", summary_df)
    openxlsx::addStyle(wb, "Summary", header_style, rows = 1, cols = 1:ncol(summary_df))

    # Individual benchmark sheets
    for (benchmark_name in names(validation_results)) {
      if (nchar(benchmark_name) > 31) {
        sheet_name <- substr(benchmark_name, 1, 31)  # Excel sheet name limit
      } else {
        sheet_name <- benchmark_name
      }

      openxlsx::addWorksheet(wb, sheet_name)

      result_df <- data.frame(
        Metric = names(validation_results[[benchmark_name]]),
        Value = sapply(validation_results[[benchmark_name]], as.character),
        stringsAsFactors = FALSE
      )

      openxlsx::writeData(wb, sheet_name, result_df)
      openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:2)
    }
  } else {
    # Single validation result
    openxlsx::addWorksheet(wb, "Validation Results")

    result_df <- data.frame(
      Metric = names(validation_results),
      Value = sapply(validation_results, as.character),
      stringsAsFactors = FALSE
    )

    openxlsx::writeData(wb, "Validation Results", result_df)
    openxlsx::addStyle(wb, "Validation Results", header_style, rows = 1, cols = 1:2)
  }

  openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
  return(file_path)
}

#' Export Validation Results to CSV
#'
#' @param validation_results Validation results
#' @param file_path Output file path
#' @return File path of created file
#' @importFrom utils write.csv
export_validation_csv <- function(validation_results, file_path) {
  if (is.list(validation_results) &&
      !all(c("precision", "recall", "f1_score") %in% names(validation_results))) {
    # Multiple benchmarks
    validation_df <- do.call(rbind, lapply(names(validation_results), function(name) {
      result <- validation_results[[name]]
      data.frame(benchmark = name, as.data.frame(result), stringsAsFactors = FALSE)
    }))
  } else {
    # Single result
    validation_df <- data.frame(
      metric = names(validation_results),
      value = sapply(validation_results, as.character),
      stringsAsFactors = FALSE
    )
  }

  utils::write.csv(validation_df, file_path, row.names = FALSE)
  return(file_path)
}

#' Export Validation Results to JSON
#'
#' @param validation_results Validation results
#' @param file_path Output file path
#' @return File path of created file
export_validation_json <- function(validation_results, file_path) {
  # FIXED: Convert packageVersion to character before serialization
  export_data <- list(
    export_timestamp = as.character(Sys.time()),  # FIXED: Convert to character
    package_version = as.character(utils::packageVersion("searchAnalyzeR")),  # FIXED: Convert to character
    r_version = as.character(R.version.string),  # FIXED: Ensure it's character
    validation_results = validation_results
  )

  # FIXED: Handle any other potential serialization issues in validation results
  export_data$validation_results <- rapply(export_data$validation_results, function(x) {
    if (inherits(x, c("POSIXct", "Date"))) {
      as.character(x)
    } else if (inherits(x, "numeric_version")) {
      as.character(x)
    } else {
      x
    }
  }, how = "replace")

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(export_data, file_path, pretty = TRUE, auto_unbox = TRUE)
  } else {
    # Fallback to base R JSON creation (simple)
    json_content <- paste0('{\n',
                           '  "export_timestamp": "', export_data$export_timestamp, '",\n',
                           '  "package_version": "', export_data$package_version, '",\n',
                           '  "r_version": "', export_data$r_version, '",\n',
                           '  "validation_results": "Use jsonlite package for full results export"\n',
                           '}')
    writeLines(json_content, file_path)
  }

  return(file_path)
}
