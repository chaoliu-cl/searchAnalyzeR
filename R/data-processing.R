#' Standardize Search Results Format
#'
#' @param results Data frame with search results
#' @param source_format Character indicating the source format
#' @return Standardized data frame
#' @importFrom dplyr select mutate filter distinct rename
#' @importFrom purrr map_lgl map_int
#' @importFrom stringr str_detect str_remove_all str_squish
#' @export
std_search_results <- function(results, source_format = "generic") {
  # Define required columns
  required_cols <- c("id", "title", "abstract", "source", "date")

  # Standardize based on source format
  standardized <- switch(source_format,
                         "pubmed" = std_pubmed_results(results),
                         "embase" = std_embase_results(results),
                         "scopus" = std_scopus_results(results),
                         "web_of_science" = std_wos_results(results),
                         "cochrane" = std_cochrane_results(results),
                         "generic" = std_generic_results(results),
                         stop("Unsupported source format: ", source_format)
  )

  # Ensure all required columns exist
  for (col in required_cols) {
    if (!col %in% names(standardized)) {
      standardized[[col]] <- NA_character_
    }
  }

  # Clean and validate data
  standardized <- standardized %>%
    dplyr::select(dplyr::all_of(required_cols), dplyr::everything()) %>%
    dplyr::mutate(
      id = as.character(.data$id),
      title = clean_text(.data$title),
      abstract = clean_text(.data$abstract),
      source = as.character(.data$source),
      date = standardize_date(.data$date)
    ) %>%
    dplyr::filter(!is.na(.data$id), !is.na(.data$title)) %>%
    dplyr::distinct(.data$id, .keep_all = TRUE)

  return(standardized)
}

#' Standardize PubMed Results
#'
#' @param results Data frame with PubMed results
#' @return Standardized data frame
std_pubmed_results <- function(results) {
  # Common PubMed column mappings
  col_mapping <- c(
    "PMID" = "id",
    "Title" = "title",
    "Abstract" = "abstract",
    "Journal" = "source",
    "Publication.Date" = "date",
    "DOI" = "doi",
    "Authors" = "authors",
    "MeSH.Terms" = "mesh_terms"
  )

  # Apply mappings
  results_mapped <- rename_columns(results, col_mapping)

  # Additional PubMed-specific processing
  results_mapped %>%
    dplyr::mutate(
      source = ifelse(is.na(.data$source), "PubMed", paste("PubMed:", .data$source)),
      id = paste0("PMID:", .data$id)
    )
}

#' Standardize Embase Results
#'
#' @param results Data frame with Embase results
#' @return Standardized data frame
std_embase_results <- function(results) {
  col_mapping <- c(
    "Embase.ID" = "id",
    "Article.Title" = "title",
    "Abstract" = "abstract",
    "Source.Title" = "source",
    "Publication.Year" = "date",
    "DOI" = "doi",
    "Author.Names" = "authors",
    "Emtree.Terms" = "emtree_terms"
  )

  results_mapped <- rename_columns(results, col_mapping)

  results_mapped %>%
    dplyr::mutate(
      source = ifelse(is.na(.data$source), "Embase", paste("Embase:", .data$source)),
      id = paste0("EMBASE:", .data$id)
    )
}

#' Standardize Scopus Results
#'
#' @param results Data frame with Scopus results
#' @return Standardized data frame
std_scopus_results <- function(results) {
  col_mapping <- c(
    "EID" = "id",
    "Title" = "title",
    "Abstract" = "abstract",
    "Source.title" = "source",
    "Year" = "date",
    "DOI" = "doi",
    "Author.Names" = "authors",
    "Index.Keywords" = "keywords"
  )

  results_mapped <- rename_columns(results, col_mapping)

  results_mapped %>%
    dplyr::mutate(
      source = ifelse(is.na(.data$source), "Scopus", paste("Scopus:", .data$source)),
      id = paste0("SCOPUS:", .data$id)
    )
}

#' Standardize Web of Science Results
#'
#' @param results Data frame with Web of Science results
#' @return Standardized data frame
std_wos_results <- function(results) {
  col_mapping <- c(
    "UT" = "id",
    "TI" = "title",
    "AB" = "abstract",
    "SO" = "source",
    "PY" = "date",
    "DI" = "doi",
    "AU" = "authors",
    "DE" = "keywords"
  )

  results_mapped <- rename_columns(results, col_mapping)

  results_mapped %>%
    dplyr::mutate(
      source = ifelse(is.na(.data$source), "Web of Science", paste("WoS:", .data$source)),
      id = paste0("WOS:", .data$id)
    )
}

#' Standardize Cochrane Results
#'
#' @param results Data frame with Cochrane results
#' @return Standardized data frame
std_cochrane_results <- function(results) {
  col_mapping <- c(
    "ID" = "id",
    "Title" = "title",
    "Abstract" = "abstract",
    "Journal" = "source",
    "Year" = "date",
    "DOI" = "doi",
    "Authors" = "authors"
  )

  results_mapped <- rename_columns(results, col_mapping)

  results_mapped %>%
    dplyr::mutate(
      source = ifelse(is.na(.data$source), "Cochrane", paste("Cochrane:", .data$source)),
      id = paste0("COCHRANE:", .data$id)
    )
}

#' Standardize Generic Results
#'
#' @param results Data frame with generic results
#' @return Standardized data frame
std_generic_results <- function(results) {
  # Try to auto-detect column mappings
  auto_mapping <- auto_detect_columns(results)

  if (length(auto_mapping) > 0) {
    results <- rename_columns(results, auto_mapping)
  }

  return(results)
}

#' Auto-detect Column Mappings
#'
#' @param results Data frame to analyze
#' @return Named vector of column mappings
#' @importFrom stringr str_detect
auto_detect_columns <- function(results) {
  col_names <- tolower(names(results))
  mapping <- c()

  # ID detection
  id_patterns <- c("id", "pmid", "embase", "eid", "ut", "doi")
  id_match <- which(stringr::str_detect(col_names, paste(id_patterns, collapse = "|")))[1]
  if (!is.na(id_match)) mapping[names(results)[id_match]] <- "id"

  # Title detection
  title_patterns <- c("title", "ti")
  title_match <- which(stringr::str_detect(col_names, paste(title_patterns, collapse = "|")))[1]
  if (!is.na(title_match)) mapping[names(results)[title_match]] <- "title"

  # Abstract detection
  abstract_patterns <- c("abstract", "ab", "summary")
  abstract_match <- which(stringr::str_detect(col_names, paste(abstract_patterns, collapse = "|")))[1]
  if (!is.na(abstract_match)) mapping[names(results)[abstract_match]] <- "abstract"

  # Source detection
  source_patterns <- c("journal", "source", "so", "publication")
  source_match <- which(stringr::str_detect(col_names, paste(source_patterns, collapse = "|")))[1]
  if (!is.na(source_match)) mapping[names(results)[source_match]] <- "source"

  # Date detection
  date_patterns <- c("date", "year", "py", "published")
  date_match <- which(stringr::str_detect(col_names, paste(date_patterns, collapse = "|")))[1]
  if (!is.na(date_match)) mapping[names(results)[date_match]] <- "date"

  return(mapping)
}

#' Rename Columns Based on Mapping
#'
#' @param df Data frame to rename
#' @param mapping Named vector of column mappings
#' @return Data frame with renamed columns
#' @importFrom dplyr rename
rename_columns <- function(df, mapping) {
  # Only rename columns that exist in the data frame
  existing_mapping <- mapping[names(mapping) %in% names(df)]

  if (length(existing_mapping) > 0) {
    df <- df %>% dplyr::rename(!!!existing_mapping)
  }

  return(df)
}

#' Clean Text Fields
#'
#' @param text Character vector to clean
#' @return Cleaned character vector
#' @importFrom stringr str_remove_all str_squish
#' @importFrom dplyr na_if
clean_text <- function(text) {
  if (is.null(text) || all(is.na(text))) return(text)

  text %>%
    # Remove HTML tags
    stringr::str_remove_all("<[^>]*>") %>%
    # Remove extra whitespace
    stringr::str_squish() %>%
    # Remove non-printable characters
    stringr::str_remove_all("[^\x20-\x7E]") %>%
    # Convert empty strings to NA
    dplyr::na_if("")
}

#' Standardize Date Formats
#'
#' @param dates Character or Date vector
#' @return Date vector
#' @importFrom stringr str_detect
#' @importFrom dplyr case_when
standardize_date <- function(dates) {
  if (is.null(dates) || all(is.na(dates))) return(as.Date(NA))

  # Handle different date formats
  tryCatch({
    # First try parsing as Date if already in date format
    if (inherits(dates, "Date")) return(dates)

    # Convert to character for processing
    dates <- as.character(dates)

    # Try different date formats
    parsed_dates <- dplyr::case_when(
      # YYYY-MM-DD format
      stringr::str_detect(dates, "^\\d{4}-\\d{2}-\\d{2}$") ~ as.Date(dates, format = "%Y-%m-%d"),
      # MM/DD/YYYY format
      stringr::str_detect(dates, "^\\d{1,2}/\\d{1,2}/\\d{4}$") ~ as.Date(dates, format = "%m/%d/%Y"),
      # DD/MM/YYYY format
      stringr::str_detect(dates, "^\\d{1,2}/\\d{1,2}/\\d{4}$") ~ as.Date(dates, format = "%d/%m/%Y"),
      # YYYY only (assume January 1st)
      stringr::str_detect(dates, "^\\d{4}$") ~ as.Date(paste0(dates, "-01-01")),
      # Default: try automatic parsing
      TRUE ~ as.Date(dates)
    )

    return(parsed_dates)
  }, error = function(e) {
    warning("Could not parse some dates, returning original values")
    return(as.Date(NA))
  })
}

#' Detect and Remove Duplicate Records
#'
#' @param results Standardized search results data frame
#' @param method Method for duplicate detection ("exact", "fuzzy", "doi")
#' @param similarity_threshold Threshold for fuzzy matching (0-1)
#' @return Data frame with duplicates marked and removed
#' @details
#' This function provides three methods for duplicate detection:
#' \itemize{
#'   \item \strong{exact}: Matches on title and first 100 characters of abstract
#'   \item \strong{fuzzy}: Uses Jaro-Winkler string distance for similarity matching
#'   \item \strong{doi}: Matches based on cleaned DOI strings
#' }
#' For fuzzy matching, similarity_threshold should be between 0 and 1, where 1 means
#' identical strings. A threshold of 0.85 typically works well for academic titles.
#' @export
detect_dupes <- function(results, method = "exact", similarity_threshold = 0.85) {
  # Add duplicate flag column
  results$duplicate       <- FALSE
  results$duplicate_group <- NA_integer_

  # Apply duplicate detection method
  if (method == "exact") {
    results <- detect_exact_dupes(results)
  } else if (method == "fuzzy") {
    results <- detect_fuzzy_dupes(results, similarity_threshold)
  } else if (method == "doi") {
    results <- detect_doi_dupes(results)
  } else {
    stop("Unsupported duplicate detection method: ", method)
  }

  return(results)
}

#' Detect Exact Duplicates
#'
#' @param results Data frame with search results
#' @return Data frame with exact duplicates marked
#' @importFrom dplyr mutate group_by ungroup select
detect_exact_dupes <- function(results) {
  # Helper to trim and collapse whitespace (base R equivalent of str_squish)
  squish <- function(x) {
    # Trim leading/trailing whitespace
    x <- gsub("^\\s+|\\s+$", "", x)
    # Collapse internal runs of whitespace to a single space
    gsub("\\s+", " ", x)
  }

  # Create composite key for exact matching
  results <- results %>%
    dplyr::mutate(
      composite_key = paste(
        tolower(squish(.data$title)),
        tolower(squish(substr(.data$abstract, 1, 100))),
        sep = "|"
      )
    )

  # Identify duplicate groups
  duplicate_groups <- results %>%
    dplyr::group_by(.data$composite_key) %>%
    dplyr::mutate(
      group_size     = dplyr::n(),
      duplicate_group = dplyr::cur_group_id()
    ) %>%
    dplyr::ungroup()

  # Mark duplicates (keep first occurrence)
  results$duplicate       <- duplicate_groups$group_size > 1
  results$duplicate_group <- ifelse(results$duplicate,
                                    duplicate_groups$duplicate_group,
                                    NA_integer_)

  # Retain only the first row per composite_key, flag others as duplicates
  results <- results %>%
    dplyr::group_by(.data$composite_key) %>%
    dplyr::mutate(keep = dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      duplicate = !.data$keep & .data$duplicate
    ) %>%
    dplyr::select(-"composite_key", -"keep")

  return(results)
}

#' Detect Fuzzy Duplicates
#'
#' @param results Data frame with search results
#' @param threshold Similarity threshold
#' @return Data frame with fuzzy duplicates marked
#' @importFrom dplyr mutate group_by ungroup
detect_fuzzy_dupes <- function(results, threshold = 0.85) {
  # Base-R helper to trim and collapse whitespace (equivalent to stringr::str_squish)
  squish <- function(x) {
    # Trim leading/trailing whitespace
    x <- gsub("^\\s+|\\s+$", "", x)
    # Collapse internal runs of whitespace to a single space
    gsub("\\s+", " ", x)
  }

  # Warn on large inputs
  if (nrow(results) > 1000) {
    warning("Large dataset - fuzzy duplicate detection may be slow")
  }

  # Clean titles for comparison:
  # 1. tolower()
  # 2. remove non-alphanumeric characters (except spaces)
  # 3. squish()
  clean_titles <- tolower(results$title)
  clean_titles <- gsub("[^a-z0-9 ]", "", clean_titles)
  clean_titles <- squish(clean_titles)

  # Compute Jaro-Winkler distance matrix, then convert to similarity
  dist_mat <- stringdist::stringdistmatrix(
    clean_titles,
    clean_titles,
    method = "jw"
  )
  sim_mat <- 1 - dist_mat

  # Find all upper-triangular pairs with similarity ≥ threshold
  duplicate_pairs <- which(
    sim_mat >= threshold & upper.tri(sim_mat),
    arr.ind = TRUE
  )

  # Initialize grouping
  results$duplicate_group <- NA_integer_
  group_counter <- 1L

  # Assign group IDs
  for (k in seq_len(nrow(duplicate_pairs))) {
    i <- duplicate_pairs[k, 1]
    j <- duplicate_pairs[k, 2]

    existing <- stats::na.omit(c(results$duplicate_group[i],
                                 results$duplicate_group[j]))
    if (length(existing) > 0) {
      # merge into existing group
      gid <- existing[1]
    } else {
      # start a new group
      gid <- group_counter
      group_counter <- group_counter + 1L
    }
    results$duplicate_group[c(i, j)] <- gid
  }

  # Flag duplicates: within each group, keep the first row
  results <- results %>%
    dplyr::group_by(.data$duplicate_group) %>%
    dplyr::mutate(
      duplicate = !is.na(.data$duplicate_group) & dplyr::row_number() > 1
    ) %>%
    dplyr::ungroup()

  return(results)
}

#' Detect DOI-based Duplicates
#'
#' @param results Data frame with search results
#' @importFrom stringr str_extract str_remove_all
#' @importFrom dplyr mutate group_by ungroup select
#' @return Data frame with DOI duplicates marked
detect_doi_dupes <- function(results) {
  if (!"doi" %in% names(results)) {
    warning("No DOI column found - using exact duplicate detection instead")
    return(detect_exact_dupes(results))
  }

  # Clean DOIs
  results <- results %>%
    dplyr::mutate(
      clean_doi = stringr::str_extract(tolower(.data$doi), "10\\.\\d+/[^\\s]+"),
      clean_doi = stringr::str_remove_all(.data$clean_doi, "[^0-9a-z./]")
    )

  # Group by DOI
  results <- results %>%
    dplyr::group_by(.data$clean_doi) %>%
    dplyr::mutate(
      duplicate = !is.na(.data$clean_doi) & dplyr::n() > 1 & dplyr::row_number() > 1,
      duplicate_group = ifelse(!is.na(.data$clean_doi) & dplyr::n() > 1, dplyr::cur_group_id(), NA)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"clean_doi")

  return(results)
}

#' Merge Search Results from Multiple Sources
#'
#' @param result_list List of standardized search result data frames
#' @param deduplicate Logical, whether to remove duplicates
#' @param dedup_method Method for duplicate detection
#' @return Combined and deduplicated data frame
#' @importFrom purrr imap map_lgl map_int
#' @importFrom dplyr bind_rows count filter
#' @export
merge_results <- function(result_list, deduplicate = TRUE, dedup_method = "exact") {
  # Validate input
  if (!is.list(result_list) || length(result_list) == 0) {
    stop("result_list must be a non-empty list of data frames")
  }

  # Check that all elements are data frames
  if (!all(purrr::map_lgl(result_list, is.data.frame))) {
    stop("All elements in result_list must be data frames")
  }

  # Add source tracking
  named_results <- purrr::imap(result_list, function(df, name) {
    df$search_source <- if (is.null(name) || name == "") paste0("source_", match(list(df), result_list)) else name
    return(df)
  })

  # Combine results
  combined_results <- dplyr::bind_rows(named_results)

  # Remove duplicates if requested
  if (deduplicate && nrow(combined_results) > 0) {
    combined_results <- detect_dupes(combined_results, method = dedup_method)

    # Remove duplicates but keep metadata
    non_duplicates <- combined_results[!combined_results$duplicate, ]

    # Create summary of removed duplicates
    duplicate_summary <- combined_results %>%
      dplyr::filter(.data$duplicate) %>%
      dplyr::count(.data$search_source, name = "duplicates_removed")

    attr(non_duplicates, "duplicate_summary") <- duplicate_summary
    combined_results <- non_duplicates
  }

  # Add merge metadata
  attr(combined_results, "merge_info") <- list(
    sources = names(result_list),
    merge_timestamp = Sys.time(),
    total_before_dedup = sum(purrr::map_int(result_list, nrow)),
    total_after_dedup = nrow(combined_results),
    deduplication_method = if (deduplicate) dedup_method else "none"
  )

  return(combined_results)
}

#' Extract Screening Data Structure
#'
#' @param search_results Combined search results
#' @param screening_decisions Optional data frame with screening decisions
#' @return Data frame with screening structure for PRISMA
#' @importFrom dplyr mutate left_join
#' @importFrom tidyr pivot_wider
#' @export
extract_screening <- function(search_results, screening_decisions = NULL) {
  # Create base screening data structure
  screening_data <- search_results %>%
    dplyr::mutate(
      identified = TRUE,
      duplicate = ifelse("duplicate" %in% names(.), .data$duplicate, FALSE),
      title_abstract_screened = FALSE,
      full_text_eligible = FALSE,
      included = FALSE,
      excluded_title_abstract = FALSE,
      excluded_full_text = FALSE,
      exclusion_reason = NA_character_
    )

  # If screening decisions are provided, merge them
  if (!is.null(screening_decisions)) {
    # Validate screening decisions format
    required_screening_cols <- c("id", "decision", "stage")
    missing_cols <- setdiff(required_screening_cols, names(screening_decisions))

    if (length(missing_cols) > 0) {
      warning("Missing required columns in screening_decisions: ", paste(missing_cols, collapse = ", "))
    } else {
      # Process screening decisions
      screening_data <- screening_data %>%
        dplyr::left_join(
          screening_decisions %>%
            tidyr::pivot_wider(names_from = "stage", values_from = "decision", names_prefix = "decision_"),
          by = "id"
        ) %>%
        dplyr::mutate(
          title_abstract_screened = !is.na(.data$decision_title_abstract),
          full_text_eligible = .data$decision_title_abstract == "include" & !is.na(.data$decision_full_text),
          included = .data$decision_full_text == "include",
          excluded_title_abstract = .data$decision_title_abstract == "exclude",
          excluded_full_text = .data$decision_full_text == "exclude"
        )
    }
  }

  return(screening_data)
}

#' Calculate Search Result Statistics
#'
#' @param search_results Data frame with search results
#' @return List of summary statistics
#' @importFrom dplyr group_by summarise count filter mutate
#' @importFrom lubridate year
#' @export
calc_search_stats <- function(search_results) {
  stats <- list(
    total_records = nrow(search_results),
    unique_records = nrow(search_results[!search_results$duplicate, ]),
    duplicates = sum(search_results$duplicate, na.rm = TRUE),
    date_range = range(search_results$date, na.rm = TRUE),
    sources = unique(search_results$source),
    missing_abstracts = sum(is.na(search_results$abstract)),
    missing_dates = sum(is.na(search_results$date))
  )

  # Add source breakdown
  if ("search_source" %in% names(search_results)) {
    stats$by_search_source <- search_results %>%
      dplyr::group_by(.data$search_source) %>%
      dplyr::summarise(
        total = dplyr::n(),
        unique = sum(!.data$duplicate, na.rm = TRUE),
        duplicates = sum(.data$duplicate, na.rm = TRUE),
        .groups = "drop"
      )
  }

  # Add temporal distribution
  if (!all(is.na(search_results$date))) {
    stats$temporal_distribution <- search_results %>%
      dplyr::filter(!is.na(.data$date)) %>%
      dplyr::mutate(year = lubridate::year(.data$date)) %>%
      dplyr::count(.data$year, sort = TRUE)
  }

  class(stats) <- "search_statistics"
  return(stats)
}
