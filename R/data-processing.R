#' Standardize Search Results Format
#'
#' @param results Data frame with search results
#' @param source_format Character indicating the source format
#' @return Standardized data frame
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
  standardized <- standardized[, c(required_cols, setdiff(names(standardized), required_cols))]

  # Clean text fields
  standardized$title <- clean_text(standardized$title)
  standardized$abstract <- clean_text(standardized$abstract)
  standardized$source <- as.character(standardized$source)
  standardized$date <- standardize_date(standardized$date)

  # Filter out rows with missing ID or title and remove duplicates
  valid_rows <- !is.na(standardized$id) & !is.na(standardized$title)
  standardized <- standardized[valid_rows, ]

  # Remove duplicate IDs, keeping first occurrence
  standardized <- standardized[!duplicated(standardized$id), ]

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
  if ("source" %in% names(results_mapped)) {
    results_mapped$source <- ifelse(is.na(results_mapped$source),
                                    "PubMed",
                                    paste("PubMed:", results_mapped$source))
  } else {
    results_mapped$source <- "PubMed"
  }

  if ("id" %in% names(results_mapped)) {
    results_mapped$id <- paste0("PMID:", results_mapped$id)
  }

  return(results_mapped)
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

  if ("source" %in% names(results_mapped)) {
    results_mapped$source <- ifelse(is.na(results_mapped$source),
                                    "Embase",
                                    paste("Embase:", results_mapped$source))
  } else {
    results_mapped$source <- "Embase"
  }

  if ("id" %in% names(results_mapped)) {
    results_mapped$id <- paste0("EMBASE:", results_mapped$id)
  }

  return(results_mapped)
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

  if ("source" %in% names(results_mapped)) {
    results_mapped$source <- ifelse(is.na(results_mapped$source),
                                    "Scopus",
                                    paste("Scopus:", results_mapped$source))
  } else {
    results_mapped$source <- "Scopus"
  }

  if ("id" %in% names(results_mapped)) {
    results_mapped$id <- paste0("SCOPUS:", results_mapped$id)
  }

  return(results_mapped)
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

  if ("source" %in% names(results_mapped)) {
    results_mapped$source <- ifelse(is.na(results_mapped$source),
                                    "Web of Science",
                                    paste("WoS:", results_mapped$source))
  } else {
    results_mapped$source <- "Web of Science"
  }

  if ("id" %in% names(results_mapped)) {
    results_mapped$id <- paste0("WOS:", results_mapped$id)
  }

  return(results_mapped)
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

  if ("source" %in% names(results_mapped)) {
    results_mapped$source <- ifelse(is.na(results_mapped$source),
                                    "Cochrane",
                                    paste("Cochrane:", results_mapped$source))
  } else {
    results_mapped$source <- "Cochrane"
  }

  if ("id" %in% names(results_mapped)) {
    results_mapped$id <- paste0("COCHRANE:", results_mapped$id)
  }

  return(results_mapped)
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
auto_detect_columns <- function(results) {
  col_names <- tolower(names(results))
  mapping <- c()

  # ID detection
  id_patterns <- c("id", "pmid", "embase", "eid", "ut", "doi")
  id_match <- which(grepl(paste(id_patterns, collapse = "|"), col_names))[1]
  if (!is.na(id_match)) mapping[names(results)[id_match]] <- "id"

  # Title detection
  title_patterns <- c("title", "ti")
  title_match <- which(grepl(paste(title_patterns, collapse = "|"), col_names))[1]
  if (!is.na(title_match)) mapping[names(results)[title_match]] <- "title"

  # Abstract detection
  abstract_patterns <- c("abstract", "ab", "summary")
  abstract_match <- which(grepl(paste(abstract_patterns, collapse = "|"), col_names))[1]
  if (!is.na(abstract_match)) mapping[names(results)[abstract_match]] <- "abstract"

  # Source detection
  source_patterns <- c("journal", "source", "so", "publication")
  source_match <- which(grepl(paste(source_patterns, collapse = "|"), col_names))[1]
  if (!is.na(source_match)) mapping[names(results)[source_match]] <- "source"

  # Date detection
  date_patterns <- c("date", "year", "py", "published")
  date_match <- which(grepl(paste(date_patterns, collapse = "|"), col_names))[1]
  if (!is.na(date_match)) mapping[names(results)[date_match]] <- "date"

  return(mapping)
}

#' Rename Columns Based on Mapping
#'
#' @param df Data frame to rename
#' @param mapping Named vector of column mappings
#' @return Data frame with renamed columns
rename_columns <- function(df, mapping) {
  # Only rename columns that exist in the data frame
  existing_mapping <- mapping[names(mapping) %in% names(df)]

  if (length(existing_mapping) > 0) {
    # Create new column names
    old_names <- names(df)
    new_names <- old_names

    for (i in seq_along(existing_mapping)) {
      old_col <- names(existing_mapping)[i]
      new_col <- existing_mapping[i]
      new_names[old_names == old_col] <- new_col
    }

    # Apply new names
    names(df) <- new_names
  }

  return(df)
}

#' Clean Text Fields
#'
#' @param text Character vector to clean
#' @return Cleaned character vector
clean_text <- function(text) {
  if (is.null(text) || all(is.na(text))) return(text)

  # Remove HTML tags
  text <- gsub("<[^>]*>", "", text)

  # Remove extra whitespace
  text <- gsub("\\s+", " ", text)
  text <- gsub("^\\s+|\\s+$", "", text)

  # Remove non-printable characters
  text <- gsub("[^\x20-\x7E]", "", text)

  # Convert empty strings to NA
  text[text == ""] <- NA

  return(text)
}

#' Standardize Date Formats
#'
#' @param dates Character or Date vector
#' @return Date vector
standardize_date <- function(dates) {
  if (is.null(dates) || all(is.na(dates))) return(as.Date(NA))

  # Handle different date formats
  tryCatch({
    # First try parsing as Date if already in date format
    if (inherits(dates, "Date")) return(dates)

    # Convert to character for processing
    dates <- as.character(dates)

    # Try different date formats
    parsed_dates <- rep(as.Date(NA), length(dates))

    # YYYY-MM-DD format
    yyyy_mm_dd <- grepl("^\\d{4}-\\d{2}-\\d{2}$", dates)
    if (any(yyyy_mm_dd)) {
      parsed_dates[yyyy_mm_dd] <- as.Date(dates[yyyy_mm_dd], format = "%Y-%m-%d")
    }

    # MM/DD/YYYY format
    mm_dd_yyyy <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", dates)
    if (any(mm_dd_yyyy)) {
      parsed_dates[mm_dd_yyyy] <- as.Date(dates[mm_dd_yyyy], format = "%m/%d/%Y")
    }

    # YYYY only (assume January 1st)
    yyyy_only <- grepl("^\\d{4}$", dates)
    if (any(yyyy_only)) {
      parsed_dates[yyyy_only] <- as.Date(paste0(dates[yyyy_only], "-01-01"))
    }

    # Try automatic parsing for any remaining NA values
    still_na <- is.na(parsed_dates)
    if (any(still_na)) {
      auto_parsed <- as.Date(dates[still_na])
      parsed_dates[still_na] <- auto_parsed
    }

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
detect_exact_dupes <- function(results) {
  # Helper to trim and collapse whitespace (base R equivalent of str_squish)
  squish <- function(x) {
    # Trim leading/trailing whitespace
    x <- gsub("^\\s+|\\s+$", "", x)
    # Collapse internal runs of whitespace to a single space
    gsub("\\s+", " ", x)
  }

  # Create composite key for exact matching
  results$composite_key <- paste(
    tolower(squish(results$title)),
    tolower(squish(substr(results$abstract, 1, 100))),
    sep = "|"
  )

  # Identify duplicate groups
  # Group by composite key
  unique_keys <- unique(results$composite_key)
  group_sizes <- table(results$composite_key)
  group_ids <- seq_along(unique_keys)

  # Assign group IDs to results
  results$group_size <- as.numeric(group_sizes[results$composite_key])
  results$duplicate_group <- match(results$composite_key, unique_keys)

  # Mark duplicates
  results$duplicate <- results$group_size > 1

  # Keep only the first occurrence of each composite_key
  keep <- !duplicated(results$composite_key)
  results$duplicate <- !keep & results$duplicate

  # Clean up temporary columns
  results$composite_key <- NULL
  results$group_size <- NULL

  return(results)
}

#' Detect Fuzzy Duplicates
#'
#' @param results Data frame with search results
#' @param threshold Similarity threshold
#' @return Data frame with fuzzy duplicates marked
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
  if (requireNamespace("stringdist", quietly = TRUE)) {
    dist_mat <- stringdist::stringdistmatrix(
      clean_titles,
      clean_titles,
      method = "jw"
    )
    sim_mat <- 1 - dist_mat
  } else {
    # Fallback to simple exact matching if stringdist not available
    warning("stringdist package not available, falling back to exact duplicate detection")
    return(detect_exact_dupes(results))
  }

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
  if (any(!is.na(results$duplicate_group))) {
    # Get list of groups
    groups <- unique(stats::na.omit(results$duplicate_group))

    # For each group, mark all but first as duplicates
    for (group in groups) {
      group_rows <- which(results$duplicate_group == group)
      if (length(group_rows) > 1) {
        results$duplicate[group_rows[-1]] <- TRUE
      }
    }
  }

  return(results)
}

#' Detect DOI-based Duplicates
#'
#' @param results Data frame with search results
#' @return Data frame with DOI duplicates marked
detect_doi_dupes <- function(results) {
  if (!"doi" %in% names(results)) {
    warning("No DOI column found - using exact duplicate detection instead")
    return(detect_exact_dupes(results))
  }

  # Clean DOIs
  results$clean_doi <- tolower(results$doi)
  results$clean_doi <- gsub("[^0-9a-z./]", "", results$clean_doi)

  # Extract DOI pattern if possible
  doi_pattern <- gsub("^.*(10\\.\\d+/[^\\s]+).*$", "\\1", results$clean_doi)
  results$clean_doi <- ifelse(grepl("^10\\.\\d+/", doi_pattern), doi_pattern, results$clean_doi)

  # Initialize duplicate flags
  results$duplicate <- FALSE
  results$duplicate_group <- NA_integer_

  # Group by clean DOI (where not NA)
  if (any(!is.na(results$clean_doi))) {
    # Find duplicate groups
    doi_table <- table(results$clean_doi)
    duplicate_dois <- names(doi_table[doi_table > 1])

    # Assign group IDs
    if (length(duplicate_dois) > 0) {
      group_id <- 1

      for (doi in duplicate_dois) {
        if (doi != "NA") {  # Skip NA values
          doi_rows <- which(results$clean_doi == doi)
          results$duplicate_group[doi_rows] <- group_id
          results$duplicate[doi_rows[-1]] <- TRUE  # Mark all but first as duplicates
          group_id <- group_id + 1
        }
      }
    }
  }

  # Remove temporary column
  results$clean_doi <- NULL

  return(results)
}

#' Merge Search Results from Multiple Sources
#'
#' @param result_list List of standardized search result data frames
#' @param deduplicate Logical, whether to remove duplicates
#' @param dedup_method Method for duplicate detection
#' @return Combined and deduplicated data frame
#' @export
merge_results <- function(result_list, deduplicate = TRUE, dedup_method = "exact") {
  # Validate input
  if (!is.list(result_list) || length(result_list) == 0) {
    stop("result_list must be a non-empty list of data frames")
  }

  # Check that all elements are data frames
  if (!all(sapply(result_list, is.data.frame))) {
    stop("All elements in result_list must be data frames")
  }

  # Add source tracking
  named_results <- list()
  for (i in seq_along(result_list)) {
    df <- result_list[[i]]
    name <- names(result_list)[i]
    if (is.null(name) || name == "") {
      name <- paste0("source_", i)
    }
    df$search_source <- name
    named_results[[i]] <- df
  }

  # Combine results
  combined_results <- do.call(rbind, named_results)

  # Remove duplicates if requested
  if (deduplicate && nrow(combined_results) > 0) {
    combined_results <- detect_dupes(combined_results, method = dedup_method)

    # Get non-duplicates
    non_duplicates <- combined_results[!combined_results$duplicate, ]

    # Create summary of removed duplicates
    duplicate_summary <- NULL
    if (any(combined_results$duplicate)) {
      dup_counts <- table(combined_results$search_source[combined_results$duplicate])
      duplicate_summary <- data.frame(
        search_source = names(dup_counts),
        duplicates_removed = as.vector(dup_counts),
        stringsAsFactors = FALSE
      )
    }

    attr(non_duplicates, "duplicate_summary") <- duplicate_summary
    combined_results <- non_duplicates
  }

  # Add merge metadata
  attr(combined_results, "merge_info") <- list(
    sources = names(result_list),
    merge_timestamp = Sys.time(),
    total_before_dedup = sum(sapply(result_list, nrow)),
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
#' @importFrom stats reshape
#' @export
extract_screening <- function(search_results, screening_decisions = NULL) {
  # Create base screening data structure
  screening_data <- search_results
  screening_data$identified <- TRUE
  screening_data$duplicate <- if ("duplicate" %in% names(search_results)) {
    search_results$duplicate
  } else {
    FALSE
  }
  screening_data$title_abstract_screened <- FALSE
  screening_data$full_text_eligible <- FALSE
  screening_data$included <- FALSE
  screening_data$excluded_title_abstract <- FALSE
  screening_data$excluded_full_text <- FALSE
  screening_data$exclusion_reason <- NA_character_

  # If screening decisions are provided, merge them
  if (!is.null(screening_decisions)) {
    # Validate screening decisions format
    required_screening_cols <- c("id", "decision", "stage")
    missing_cols <- setdiff(required_screening_cols, names(screening_decisions))

    if (length(missing_cols) > 0) {
      warning("Missing required columns in screening_decisions: ",
              paste(missing_cols, collapse = ", "))
    } else {
      # Process screening decisions
      # Convert from long to wide format using base R
      decisions_wide <- stats::reshape(screening_decisions,
                                       idvar = "id",
                                       timevar = "stage",
                                       direction = "wide")

      # Rename columns
      names(decisions_wide) <- gsub("decision\\.", "decision_", names(decisions_wide))

      # Merge with screening data
      screening_data <- merge(screening_data, decisions_wide, by = "id", all.x = TRUE)

      # Update screening flags
      screening_data$title_abstract_screened <- !is.na(screening_data$decision_title_abstract)
      screening_data$full_text_eligible <- !is.na(screening_data$decision_title_abstract) &
        screening_data$decision_title_abstract == "include" &
        !is.na(screening_data$decision_full_text)
      screening_data$included <- !is.na(screening_data$decision_full_text) &
        screening_data$decision_full_text == "include"
      screening_data$excluded_title_abstract <- !is.na(screening_data$decision_title_abstract) &
        screening_data$decision_title_abstract == "exclude"
      screening_data$excluded_full_text <- !is.na(screening_data$decision_full_text) &
        screening_data$decision_full_text == "exclude"
    }
  }

  return(screening_data)
}

#' Calculate Search Result Statistics
#'
#' @param search_results Data frame with search results
#' @return List of summary statistics
#' @importFrom stats aggregate
#' @export
calc_search_stats <- function(search_results) {
  stats <- list(
    total_records = nrow(search_results),
    unique_records = if ("duplicate" %in% names(search_results)) {
      sum(!search_results$duplicate, na.rm = TRUE)
    } else {
      nrow(search_results)
    },
    duplicates = if ("duplicate" %in% names(search_results)) {
      sum(search_results$duplicate, na.rm = TRUE)
    } else {
      0
    },
    date_range = range(search_results$date, na.rm = TRUE),
    sources = unique(search_results$source),
    missing_abstracts = sum(is.na(search_results$abstract)),
    missing_dates = sum(is.na(search_results$date))
  )

  # Add source breakdown
  if ("search_source" %in% names(search_results)) {
    source_breakdown <- stats::aggregate(
      cbind(
        total = rep(1, nrow(search_results)),
        unique = !search_results$duplicate
      ),
      by = list(search_source = search_results$search_source),
      FUN = sum,
      na.rm = TRUE
    )

    source_breakdown$duplicates <- source_breakdown$total - source_breakdown$unique
    stats$by_search_source <- source_breakdown
  }

  # Add temporal distribution
  if (!all(is.na(search_results$date))) {
    if (requireNamespace("lubridate", quietly = TRUE)) {
      years <- lubridate::year(search_results$date)
    } else {
      years <- as.numeric(format(search_results$date, "%Y"))
    }

    years <- years[!is.na(years)]
    stats$temporal_distribution <- table(years)
  }

  class(stats) <- "search_statistics"
  return(stats)
}
