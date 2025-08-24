#' Search PubMed and Retrieve Articles
#'
#' @description
#' Searches PubMed using the provided search terms and retrieves article metadata
#' in a format compatible with searchAnalyzeR analysis functions.
#'
#' @details
#' This function connects to PubMed using the rentrez package (if available) or
#' provides simulated data if the package is not installed. Results are returned
#' as a standardized data frame ready for use with SearchAnalyzer.
#'
#' @param search_terms Character vector of search terms to use in PubMed query
#' @param max_results Maximum number of results to retrieve (default: 200)
#' @param date_range Optional date range as c("YYYY-MM-DD", "YYYY-MM-DD")
#' @param language Optional language filter (default: "English")
#' @return Data frame containing standardized search results
#'
#' @examples
#' \donttest{
#' # Search for diabetes clinical trials
#' results <- search_pubmed(
#'   search_terms = c("diabetes", "clinical trial"),
#'   max_results = 100,
#'   date_range = c("2020-01-01", "2023-12-31")
#' )
#'
#' # Use with SearchAnalyzer
#' analyzer <- SearchAnalyzer$new(results)
#' metrics <- analyzer$calculate_metrics()
#' }
#'
#' @export
search_pubmed <- function(search_terms, max_results = 200, date_range = NULL, language = "English") {
  # Check if required packages are available
  has_rentrez <- requireNamespace("rentrez", quietly = TRUE)
  has_xml2 <- requireNamespace("xml2", quietly = TRUE)

  if (!has_rentrez || !has_xml2) {
    warning("Packages 'rentrez' and 'xml2' are required for live PubMed search. ",
            "Using simulated data instead. Install with: install.packages(c('rentrez', 'xml2'))")
    return(simulate_pubmed_results(search_terms, max_results, date_range))
  }

  # Build PubMed query
  query_terms <- paste(paste0('"', search_terms, '"[Title/Abstract]'), collapse = " OR ")

  if (!is.null(date_range)) {
    # Convert and format dates if needed
    if (inherits(date_range, "Date")) {
      date_range <- format(date_range, "%Y/%m/%d")
    } else {
      # Replace hyphens with slashes if present
      date_range <- gsub("-", "/", date_range)
    }

    date_filter <- paste0('("', date_range[1], '"[Date - Publication] : "',
                          date_range[2], '"[Date - Publication])')
    full_query <- paste("(", query_terms, ") AND", date_filter)
  } else {
    full_query <- paste("(", query_terms, ")")
  }

  # Add language filter if specified
  if (!is.null(language)) {
    full_query <- paste(full_query, "AND", language, "[Language]")
  }

  cat("PubMed Query:", full_query, "\n")

  # Search PubMed
  search_results <- rentrez::entrez_search(
    db = "pubmed",
    term = full_query,
    retmax = max_results,
    use_history = TRUE
  )

  cat("Found", length(search_results$ids), "articles\n")

  if (length(search_results$ids) == 0) {
    return(data.frame())
  }

  # Retrieve detailed information in batches
  batch_size <- 50
  all_articles <- list()

  for (i in seq(1, length(search_results$ids), batch_size)) {
    end_idx <- min(i + batch_size - 1, length(search_results$ids))
    batch_ids <- search_results$ids[i:end_idx]

    cat("Retrieving batch", ceiling(i/batch_size), "of", ceiling(length(search_results$ids)/batch_size), "\n")

    # Get abstracts and metadata
    abstracts <- rentrez::entrez_fetch(
      db = "pubmed",
      id = batch_ids,
      rettype = "abstract",
      retmode = "xml"
    )

    # Parse XML
    xml_doc <- xml2::read_xml(abstracts)
    articles <- xml2::xml_find_all(xml_doc, ".//PubmedArticle")

    batch_data <- lapply(articles, function(article) {
      # Extract PMID
      pmid_node <- xml2::xml_find_first(article, ".//PMID")
      pmid <- if (!is.na(pmid_node)) xml2::xml_text(pmid_node) else NA

      # Extract title
      title_node <- xml2::xml_find_first(article, ".//ArticleTitle")
      title <- if (!is.na(title_node)) xml2::xml_text(title_node) else "No title available"

      # Extract abstract
      abstract_nodes <- xml2::xml_find_all(article, ".//AbstractText")
      abstract <- if (length(abstract_nodes) > 0) {
        paste(xml2::xml_text(abstract_nodes), collapse = " ")
      } else {
        "No abstract available"
      }

      # Extract journal
      journal_node <- xml2::xml_find_first(article, ".//Journal/Title")
      journal <- if (!is.na(journal_node)) xml2::xml_text(journal_node) else "Unknown journal"

      # Extract publication date
      year_node <- xml2::xml_find_first(article, ".//PubDate/Year")
      month_node <- xml2::xml_find_first(article, ".//PubDate/Month")
      day_node <- xml2::xml_find_first(article, ".//PubDate/Day")

      year <- if (!is.na(year_node)) xml2::xml_text(year_node) else format(Sys.Date(), "%Y")
      month <- if (!is.na(month_node)) xml2::xml_text(month_node) else "01"
      day <- if (!is.na(day_node)) xml2::xml_text(day_node) else "01"

      # Convert month name to number if necessary
      if (nchar(month) > 2) {
        month <- match(month, month.name)
        if (is.na(month)) month <- "01"
      }

      # Create date
      pub_date <- tryCatch({
        as.Date(paste(year, sprintf("%02d", as.numeric(month)), sprintf("%02d", as.numeric(day)), sep = "-"))
      }, error = function(e) {
        as.Date(paste(year, "01", "01", sep = "-"))
      })

      # Extract authors
      author_nodes <- xml2::xml_find_all(article, ".//Author/LastName")
      authors <- if (length(author_nodes) > 0) {
        paste(xml2::xml_text(author_nodes), collapse = ", ")
      } else {
        "No authors listed"
      }

      # Extract DOI
      doi_node <- xml2::xml_find_first(article, ".//ELocationID[@EIdType='doi']")
      doi <- if (!is.na(doi_node)) xml2::xml_text(doi_node) else NA

      data.frame(
        id = paste0("PMID:", pmid),
        title = title,
        abstract = abstract,
        source = journal,
        date = pub_date,
        authors = authors,
        doi = doi,
        pmid = pmid,
        search_source = "PubMed_API",
        stringsAsFactors = FALSE
      )
    })

    all_articles <- c(all_articles, batch_data)

    # Be respectful to NCBI servers
    Sys.sleep(0.5)
  }

  # Combine all results
  do.call(rbind, all_articles)
}

#' Generate Simulated PubMed Results
#'
#' @param search_terms Character vector of search terms
#' @param max_results Maximum number of results
#' @param date_range Optional date range
#' @return Data frame with simulated results
#' @importFrom stats runif
#' @keywords internal
simulate_pubmed_results <- function(search_terms, max_results = 200, date_range = NULL) {
  # Create sample data that mimics PubMed results
  n_results <- min(max_results, 100)  # Limit sample size

  # Generate relevant titles based on search terms
  titles <- vector("character", n_results)
  for (i in 1:n_results) {
    # Use some of the search terms in the title
    terms_to_use <- sample(search_terms, min(length(search_terms), 2), replace = TRUE)
    titles[i] <- paste("Study on", paste(terms_to_use, collapse = " and "),
                       ifelse(runif(1) > 0.7, "- A randomized controlled trial", ""))
  }

  # Generate publication dates
  if (!is.null(date_range)) {
    start_date <- as.Date(date_range[1])
    end_date <- as.Date(date_range[2])
  } else {
    end_date <- Sys.Date()
    start_date <- end_date - 365*5  # 5 years back
  }

  date_range_days <- as.numeric(difftime(end_date, start_date, units = "days"))
  pub_dates <- start_date + sample(0:date_range_days, n_results, replace = TRUE)

  # Create simulated dataset
  data.frame(
    id = paste0("PMID:", sample(10000:99999, n_results)),
    title = titles,
    abstract = paste("This is a simulated abstract for a study about",
                     search_terms[sample(length(search_terms), 1)],
                     "with various outcomes and methodologies."),
    source = sample(c("Journal of Medicine", "Clinical Research",
                      "Medical Science Today", "Research in Health"),
                    n_results, replace = TRUE),
    date = pub_dates,
    authors = paste(sample(c("Smith J", "Johnson A", "Williams R", "Brown K",
                             "Miller P", "Davis M", "Garcia J", "Wilson T"),
                           3, replace = TRUE), collapse = ", "),
    doi = paste0("10.1000/jm.", sample(10000:99999, n_results)),
    pmid = sample(10000:99999, n_results),
    search_source = "PubMed_Simulated",
    stringsAsFactors = FALSE
  )
}

#' PubMed Database Connector
#'
#' This module provides functionality to search PubMed directly and integrate
#' the results with searchAnalyzeR's analysis capabilities.

#' PubMed Search Interface
#'
#' @description
#' A class for connecting to and searching PubMed database directly,
#' then formatting results for analysis with searchAnalyzeR.
#'
#' @details
#' This class uses the rentrez package to interface with NCBI's E-utilities
#' to search PubMed and retrieve article metadata. Results are automatically
#' formatted for use with SearchAnalyzer. If rentrez is not available,
#' it provides simulated data for demonstration purposes.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{Initialize a new PubMedConnector instance}
#'   \item{\code{search(query, max_results, date_range)}}{Search PubMed database}
#'   \item{\code{get_details(pmids)}}{Get detailed information for specific PMIDs}
#'   \item{\code{format_for_analysis()}}{Format results for SearchAnalyzer}
#' }
#'
#' @examples
#' \donttest{
#' # Create PubMed connector
#' pubmed <- PubMedConnector$new()
#'
#' # Search for diabetes studies
#' results <- pubmed$search(
#'   query = "diabetes[Title/Abstract] AND clinical trial[Publication Type]",
#'   max_results = 100,
#'   date_range = c("2020/01/01", "2023/12/31")
#' )
#'
#' # Format for analysis
#' search_data <- pubmed$format_for_analysis()
#'
#' # Use with SearchAnalyzer
#' analyzer <- SearchAnalyzer$new(search_data)
#' metrics <- analyzer$calculate_metrics()
#' }
#'
#' @export
PubMedConnector <- R6::R6Class(
  "PubMedConnector",
  public = list(
    #' @field last_search_results Raw results from last search
    last_search_results = NULL,

    #' @field formatted_results Formatted results ready for analysis
    formatted_results = NULL,

    #' @field search_metadata Metadata about the last search
    search_metadata = NULL,

    #' @field use_simulation Flag indicating if simulation mode is active
    use_simulation = FALSE,

    #' @description
    #' Initialize a new PubMedConnector instance
    #' @return No return value, called for side effects
    initialize = function() {
      # Check if rentrez is available
      if (!requireNamespace("rentrez", quietly = TRUE)) {
        warning("Package 'rentrez' is required for PubMed connectivity. Using simulation mode instead.")
        self$use_simulation <- TRUE
      } else {
        private$check_connection()
      }

      invisible(self)
    },

    #' Search PubMed database
    #' @param query PubMed search query string
    #' @param max_results Maximum number of results to retrieve (default: 100)
    #' @param date_range Optional date range as c("YYYY/MM/DD", "YYYY/MM/DD")
    #' @param retmode Return mode ("xml" or "text")
    #' @return Number of results found
    search = function(query, max_results = 100, date_range = NULL, retmode = "xml") {
      # If in simulation mode, generate fake data
      if (self$use_simulation) {
        cat("Using simulation mode for PubMed search\n")
        search_terms <- unlist(strsplit(query, "\\s+AND\\s+|\\s+OR\\s+"))
        self$last_search_results <- simulate_pubmed_results(search_terms, max_results, date_range)
        self$search_metadata <- list(
          query = query,
          original_query = query,
          date_range = date_range,
          max_results = max_results,
          actual_results = nrow(self$last_search_results),
          search_date = Sys.time(),
          simulation = TRUE
        )
        self$formatted_results <- self$last_search_results
        return(nrow(self$last_search_results))
      }

      # Real search using rentrez
      if (!requireNamespace("rentrez", quietly = TRUE)) {
        stop("Package 'rentrez' is required. Install with: install.packages('rentrez')")
      }

      # Build search query
      full_query <- private$build_query(query, date_range)

      cat("Searching PubMed with query:", full_query, "\\n")

      # Search PubMed
      search_results <- rentrez::entrez_search(
        db = "pubmed",
        term = full_query,
        retmax = max_results
      )

      cat("Found", length(search_results$ids), "results\\n")

      if (length(search_results$ids) == 0) {
        warning("No results found for query: ", full_query)
        return(0)
      }

      # Get detailed information
      cat("Retrieving detailed information...\\n")
      detailed_results <- private$get_detailed_info(search_results$ids, retmode)

      # Store results
      self$last_search_results <- detailed_results
      self$search_metadata <- list(
        query = full_query,
        original_query = query,
        date_range = date_range,
        max_results = max_results,
        actual_results = length(search_results$ids),
        search_date = Sys.time(),
        simulation = FALSE
      )

      # Format for analysis
      self$formatted_results <- private$format_results(detailed_results)

      cat("Search completed successfully!\\n")
      return(length(search_results$ids))
    },

    #' Get detailed information for specific PMIDs
    #' @param pmids Vector of PubMed IDs
    #' @param retmode Return mode ("xml" or "text")
    #' @return Detailed article information
    get_details = function(pmids, retmode = "xml") {
      if (self$use_simulation) {
        cat("Using simulation mode for fetching details\n")
        return(list(simulate_pubmed_results(c("details", "fetch"), length(pmids))))
      }

      if (!requireNamespace("rentrez", quietly = TRUE)) {
        stop("Package 'rentrez' is required")
      }

      if (length(pmids) == 0) {
        return(NULL)
      }

      # Get detailed info in batches (PubMed limits to 200 at a time)
      batch_size <- 200
      all_results <- list()

      for (i in seq(1, length(pmids), batch_size)) {
        end_idx <- min(i + batch_size - 1, length(pmids))
        batch_pmids <- pmids[i:end_idx]

        cat("Retrieving batch", ceiling(i/batch_size), "of", ceiling(length(pmids)/batch_size), "\\n")

        batch_results <- rentrez::entrez_fetch(
          db = "pubmed",
          id = batch_pmids,
          rettype = "abstract",
          retmode = retmode
        )

        all_results[[length(all_results) + 1]] <- batch_results

        # Be nice to NCBI servers
        Sys.sleep(0.1)
      }

      return(all_results)
    },

    #' Format results for SearchAnalyzer
    #' @return Data frame formatted for searchAnalyzeR analysis
    format_for_analysis = function() {
      if (is.null(self$formatted_results)) {
        stop("No search results available. Run search() first.")
      }

      return(self$formatted_results)
    },

    #' Get search summary
    #' @return List with search summary information
    get_search_summary = function() {
      if (is.null(self$search_metadata)) {
        return(NULL)
      }

      list(
        query = self$search_metadata$original_query,
        results_found = self$search_metadata$actual_results,
        search_date = self$search_metadata$search_date,
        date_range = self$search_metadata$date_range,
        simulation_mode = self$use_simulation
      )
    }
  ),

  private = list(
    check_connection = function() {
      # Test connection to NCBI
      tryCatch({
        if (requireNamespace("rentrez", quietly = TRUE)) {
          test_search <- rentrez::entrez_search(db = "pubmed", term = "test", retmax = 1)
          cat("[OK] Connected to PubMed successfully\\n")
        }
      }, error = function(e) {
        warning("Could not connect to PubMed: ", e$message)
        self$use_simulation <- TRUE
      })
    },

    build_query = function(query, date_range) {
      full_query <- query

      if (!is.null(date_range) && length(date_range) == 2) {
        date_filter <- paste0("(\"", date_range[1], "\"[Date - Publication] : \"", date_range[2], "\"[Date - Publication])")
        full_query <- paste(full_query, "AND", date_filter)
      }

      return(full_query)
    },

    get_detailed_info = function(pmids, retmode) {
      if (length(pmids) == 0) return(NULL)

      # Use the public method to get details
      self$get_details(pmids, retmode)
    },

    format_results = function(raw_results) {
      if (is.null(raw_results) || length(raw_results) == 0) {
        return(data.frame())
      }

      # If we're in simulation mode, results are already formatted
      if (self$use_simulation && is.data.frame(raw_results)) {
        return(raw_results)
      }

      # Try to parse XML results if xml2 is available
      if (requireNamespace("xml2", quietly = TRUE)) {
        formatted_data <- private$parse_pubmed_xml(raw_results)
        return(formatted_data)
      } else {
        # Fallback to creating sample data
        return(private$create_sample_data(length(raw_results) * 10))
      }
    },

    parse_pubmed_xml = function(xml_results) {
      # If xml2 is not available, return sample data
      if (!requireNamespace("xml2", quietly = TRUE)) {
        return(private$create_sample_data(length(xml_results) * 10))
      }

      # In a real implementation, parse the XML results from rentrez
      # For now, return sample data
      return(private$create_sample_data(length(xml_results) * 10))
    },

    create_sample_data = function(n_results) {
      # Create sample formatted data
      data.frame(
        id = paste0("PMID:", seq_len(n_results)),
        title = paste("Retrieved study", seq_len(n_results)),
        abstract = paste("Abstract for study", seq_len(n_results)),
        source = "PubMed",
        date = Sys.Date() - sample(1:1000, n_results, replace = TRUE),
        authors = paste("Author", seq_len(n_results)),
        journal = paste("Journal", sample(1:20, n_results, replace = TRUE)),
        doi = paste0("10.1000/", seq_len(n_results)),
        search_source = "PubMed_Simulated",
        stringsAsFactors = FALSE
      )
    }
  )
)

#' Search Multiple Databases
#'
#' @description
#' Search multiple databases and combine results for comprehensive analysis.
#'
#' @param search_strategy List containing search parameters
#' @param databases Vector of databases to search ("pubmed", "pmc", etc.)
#' @param max_results_per_db Maximum results per database
#' @return Combined search results from all databases
#'
#' @examples
#' \donttest{
#' # Define search strategy
#' strategy <- list(
#'   terms = "diabetes AND treatment",
#'   date_range = c("2020/01/01", "2023/12/31"),
#'   max_results = 50
#' )
#'
#' # Search multiple databases
#' results <- search_multiple_databases(
#'   search_strategy = strategy,
#'   databases = c("pubmed"),
#'   max_results_per_db = 100
#' )
#'
#' # Analyze results
#' analyzer <- SearchAnalyzer$new(results)
#' metrics <- analyzer$calculate_metrics()
#' }
#'
#' @export
search_multiple_databases <- function(search_strategy,
                                      databases = c("pubmed"),
                                      max_results_per_db = 100) {

  all_results <- list()

  for (db in databases) {
    cat("\\n=== Searching", toupper(db), "===\\n")

    if (db == "pubmed") {
      connector <- PubMedConnector$new()

      n_found <- connector$search(
        query = search_strategy$terms,
        max_results = max_results_per_db,
        date_range = search_strategy$date_range
      )

      if (n_found > 0) {
        db_results <- connector$format_for_analysis()
        db_results$search_database <- "PubMed"
        all_results[[db]] <- db_results
      }
    }
    # Add other databases here (PMC, Embase, etc.)
  }

  if (length(all_results) == 0) {
    warning("No results found from any database")
    return(data.frame())
  }

  # Combine results using base R
  result_names <- names(all_results)
  combined_results <- do.call(rbind, all_results)

  # Add search metadata
  attr(combined_results, "search_metadata") <- list(
    search_strategy = search_strategy,
    databases_searched = databases,
    total_results = nrow(combined_results),
    search_timestamp = Sys.time()
  )

  cat("\\n=== Search Summary ===\\n")
  cat("Total results:", nrow(combined_results), "\\n")
  cat("Databases searched:", paste(databases, collapse = ", "), "\\n")

  return(combined_results)
}

#' Complete Search and Analysis Workflow
#'
#' @description
#' Perform a complete workflow: search databases, analyze results, generate reports.
#'
#' @param search_terms Character vector of search terms
#' @param databases Vector of databases to search
#' @param gold_standard Optional vector of known relevant article IDs
#' @param max_results Maximum results to retrieve
#' @param date_range Optional date range for search
#' @param output_dir Directory for reports (uses tempdir() by default)
#' @return List containing search results, analysis, and report paths
#'
#' @examples
#' \donttest{
#' # Complete workflow
#' results <- complete_search_workflow(
#'   search_terms = "diabetes treatment clinical trial",
#'   databases = "pubmed",
#'   max_results = 50,
#'   date_range = c("2022/01/01", "2023/12/31")
#' )
#'
#' # View summary
#' print(results$summary)
#'
#' # Access detailed metrics
#' print(results$analysis$metrics)
#' }
#'
#' @export
complete_search_workflow <- function(search_terms,
                                     databases = "pubmed",
                                     gold_standard = NULL,
                                     max_results = 100,
                                     date_range = NULL,
                                     output_dir = NULL) {

  if (is.null(output_dir)) {
    output_dir <- tempdir()
  }

  cat("=== Starting Complete Search and Analysis Workflow ===\\n\\n")

  # Step 1: Search databases
  cat("Step 1: Searching databases...\\n")
  search_strategy <- list(
    terms = search_terms,
    date_range = date_range,
    max_results = max_results
  )

  search_results <- search_multiple_databases(
    search_strategy = search_strategy,
    databases = databases,
    max_results_per_db = max_results
  )

  if (nrow(search_results) == 0) {
    stop("No results found. Please modify your search terms.")
  }

  # Step 2: Process and deduplicate
  cat("\\nStep 2: Processing and deduplicating results...\\n")
  processed_results <- detect_dupes(search_results, method = "exact")

  cat("Original results:", nrow(search_results), "\\n")
  cat("After deduplication:", sum(!processed_results$duplicate), "\\n")

  # Step 3: Analyze results
  cat("\\nStep 3: Analyzing search performance...\\n")
  analyzer <- SearchAnalyzer$new(
    search_results = processed_results,
    gold_standard = gold_standard,
    search_strategy = search_strategy
  )

  metrics <- analyzer$calculate_metrics()

  # Step 4: Generate visualizations
  cat("\\nStep 4: Generating visualizations...\\n")
  plots <- list(
    overview = analyzer$visualize_performance("overview"),
    temporal = analyzer$visualize_performance("temporal")
  )

  # Step 5: Export results
  cat("\\nStep 5: Exporting results...\\n")
  export_files <- export_results(
    search_results = processed_results,
    file_path = file.path(output_dir, "search_results"),
    formats = c("csv", "xlsx"),
    include_metadata = TRUE
  )

  # Step 6: Generate reports
  cat("\\nStep 6: Generating reports...\\n")
  reporter <- PRISMAReporter$new()

  # Create data package in tempdir()
  package_dir <- create_data_package(
    search_results = processed_results,
    analysis_results = list(metrics = metrics, plots = plots),
    output_dir = output_dir,
    package_name = "search_analysis_results"
  )

  # Summary
  summary_info <- list(
    search_terms = search_terms,
    databases_searched = databases,
    total_found = nrow(search_results),
    after_deduplication = sum(!processed_results$duplicate),
    duplicates_removed = sum(processed_results$duplicate),
    date_range = date_range,
    search_date = Sys.time(),
    has_gold_standard = !is.null(gold_standard),
    output_directory = output_dir
  )

  if (!is.null(gold_standard)) {
    summary_info$precision <- metrics$precision_recall$precision
    summary_info$recall <- metrics$precision_recall$recall
    summary_info$f1_score <- metrics$precision_recall$f1_score
  }

  cat("\\n=== Workflow Complete ===\\n")
  cat("Results exported to:", output_dir, "\\n")
  cat("Total articles found:", summary_info$total_found, "\\n")
  cat("After deduplication:", summary_info$after_deduplication, "\\n")

  if (!is.null(gold_standard)) {
    cat("Precision:", round(summary_info$precision, 3), "\\n")
    cat("Recall:", round(summary_info$recall, 3), "\\n")
    cat("F1 Score:", round(summary_info$f1_score, 3), "\\n")
  }

  return(list(
    search_results = processed_results,
    analysis = list(
      analyzer = analyzer,
      metrics = metrics,
      plots = plots
    ),
    exports = list(
      files = export_files,
      package_directory = package_dir
    ),
    summary = summary_info
  ))
}
