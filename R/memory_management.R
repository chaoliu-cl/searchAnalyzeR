#' Get Current Memory Usage
#'
#' @description
#' Reports the current memory usage of the R session.
#'
#' @param units Units for reporting memory usage ("MB", "GB", or "KB")
#' @param include_gc Logical, whether to run garbage collection before measuring
#' @return Named list with memory usage information
#' @export
mem_usage <- function(units = "MB", include_gc = FALSE) {
  # Run garbage collection if requested
  if (include_gc) {
    gc(verbose = FALSE)
  }

  # Get memory usage from gc() stats
  gc_info <- gc(verbose = FALSE)

  # Calculate total memory usage (used + free)
  mem_used <- sum(gc_info[, "used"])

  # Convert to requested units
  divisor <- switch(toupper(units),
                    "KB" = 1024,
                    "MB" = 1024^2,
                    "GB" = 1024^3,
                    1024^2)  # Default to MB

  mem_used_formatted <- mem_used / divisor

  result <- list(
    mem_used = mem_used_formatted,
    units = toupper(units),
    timestamp = Sys.time()
  )

  return(result)
}

#' Memory-Efficient Data Frame
#'
#' @description
#' Converts a data frame to a memory-efficient format by optimizing column types.
#'
#' @param df Data frame to optimize
#' @param compress_strings Logical, whether to convert character columns to factors
#' @param verbose Logical, whether to print memory savings information
#' @return Memory-efficient version of the input data frame
#' @export
opt_df <- function(df, compress_strings = FALSE, verbose = TRUE) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  # Get initial memory usage
  initial_size <- utils::object.size(df)

  # Function to convert columns to optimal types
  optimize_columns <- function(df) {
    for (col_name in names(df)) {
      col <- df[[col_name]]

      # Optimize integers
      if (is.integer(col)) {
        # Check range to see if we can downcast
        if (all(is.na(col) | (col >= -128 & col <= 127), na.rm = TRUE)) {
          # Can be stored as 1-byte integer
          storage.mode(df[[col_name]]) <- "integer"
          attr(df[[col_name]], "bytes") <- 1
        } else if (all(is.na(col) | (col >= -32768 & col <= 32767), na.rm = TRUE)) {
          # Can be stored as 2-byte integer
          storage.mode(df[[col_name]]) <- "integer"
          attr(df[[col_name]], "bytes") <- 2
        }
      }

      # Optimize numeric columns with integer values
      if (is.numeric(col) && !is.integer(col)) {
        # Check if column contains only integers
        is_integer_valued <- all(is.na(col) | (col == round(col)), na.rm = TRUE)
        if (is_integer_valued) {
          # Convert to integer if range allows
          col_range <- range(col, na.rm = TRUE)
          if (col_range[1] >= -2147483647 && col_range[2] <= 2147483647) {
            df[[col_name]] <- as.integer(col)
          }
        }
      }

      # Optimize character columns
      if (is.character(col) && compress_strings) {
        unique_values <- unique(col)
        # If number of unique values is much less than total values, convert to factor
        if (length(unique_values) < length(col) * 0.5) {
          df[[col_name]] <- as.factor(col)
        }
      }
    }

    return(df)
  }

  # Optimize column types
  df <- optimize_columns(df)

  # Report memory usage if verbose
  if (verbose) {
    final_size <- utils::object.size(df)
    savings_pct <- (1 - (final_size / initial_size)) * 100

    message(sprintf("Memory optimization: %.2f%% reduction", savings_pct))
    message(sprintf("Original: %.2f MB, Optimized: %.2f MB",
                    initial_size / 1024^2, final_size / 1024^2))
  }

  return(df)
}

#' Clean up Search Analysis Objects to Free Memory
#'
#' @description
#' Removes intermediate and temporary objects created during analysis to free memory.
#' This is particularly useful for large-scale analyses.
#'
#' @param keep_results Logical, whether to keep final results
#' @param verbose Logical, whether to print memory freed information
#' @return Amount of memory freed in MB
#' @export
mem_cleanup <- function(keep_results = TRUE, verbose = TRUE) {
  # Get initial memory usage
  initial_mem <- mem_usage(include_gc = TRUE)

  # List of common pattern names for temporary objects
  temp_patterns <- c(
    "\\.tmp$", "^temp", "^tmp", "_cache$", "^intermediate", "^working_",
    "^chunk_", "^batch_", "_copy$", "^raw_", "^processed_", "^.*_matrix$"
  )

  # Get list of objects in global environment
  all_objects <- ls(envir = .GlobalEnv)

  # Find objects that match temporary patterns
  temp_objects <- character(0)
  for (pattern in temp_patterns) {
    matches <- grep(pattern, all_objects, value = TRUE)
    temp_objects <- c(temp_objects, matches)
  }

  # Don't remove result objects if keep_results is TRUE
  if (keep_results) {
    result_patterns <- c("results$", "^final_", "^output_", "report$", "^.*_results$")
    for (pattern in result_patterns) {
      matches <- grep(pattern, temp_objects, value = TRUE)
      temp_objects <- setdiff(temp_objects, matches)
    }
  }

  # Optionally print objects to be removed
  if (verbose && length(temp_objects) > 0) {
    message("Removing temporary objects:")
    message(paste(" -", temp_objects, collapse = "\n"))
  } else if (verbose) {
    message("No temporary objects found to remove")
  }

  # Remove the temporary objects
  if (length(temp_objects) > 0) {
    for (obj in temp_objects) {
      if (exists(obj, envir = .GlobalEnv)) {
        rm(list = obj, envir = .GlobalEnv)
      }
    }
  }

  # Run garbage collection
  gc(verbose = FALSE)

  # Get final memory usage
  final_mem <- mem_usage(include_gc = FALSE)

  # Calculate memory freed
  mem_freed <- initial_mem$mem_used - final_mem$mem_used

  if (verbose) {
    message(sprintf("Memory freed: %.2f %s",
                    max(0, mem_freed), initial_mem$units))
  }

  return(invisible(mem_freed))
}

#' Process Large Dataset in Chunks
#'
#' @description
#' Generic function to process a large dataset in manageable chunks to reduce memory usage.
#'
#' @param data Large data frame to process
#' @param chunk_size Number of rows per chunk
#' @param fn Function to apply to each chunk
#' @param combine_fn Function to combine results from chunks
#' @param ... Additional arguments passed to fn
#' @return Combined results after processing all chunks
#' @export
chunk_process <- function(data, chunk_size = 10000, fn, combine_fn = rbind, ...) {
  if (!is.data.frame(data)) {
    stop("Input data must be a data frame")
  }

  if (!is.function(fn)) {
    stop("fn must be a function to apply to each chunk")
  }

  if (!is.function(combine_fn)) {
    stop("combine_fn must be a function to combine results")
  }

  # Calculate number of chunks
  n_rows <- nrow(data)
  n_chunks <- ceiling(n_rows / chunk_size)

  if (n_chunks <= 1) {
    # If only one chunk, process entire dataset
    return(fn(data, ...))
  }

  # Initialize results list
  results <- list()

  # Create a simple progress indicator
  message(sprintf("Processing %d chunks...", n_chunks))
  prog_step <- max(1, floor(n_chunks / 10))

  # Process data in chunks
  for (i in 1:n_chunks) {
    # Define chunk boundaries
    start_idx <- ((i - 1) * chunk_size) + 1
    end_idx <- min(i * chunk_size, n_rows)

    # Extract chunk
    chunk <- data[start_idx:end_idx, , drop = FALSE]

    # Process chunk
    chunk_result <- fn(chunk, ...)

    # Store result
    if (!is.null(chunk_result)) {
      results[[i]] <- chunk_result
    }

    # Update progress
    if (i %% prog_step == 0 || i == n_chunks) {
      message(sprintf("Processed chunk %d of %d", i, n_chunks))
    }

    # Clean up to free memory
    rm(chunk, chunk_result)
    gc(verbose = FALSE)
  }

  # Combine results if there are any
  if (length(results) > 0) {
    final_result <- do.call(combine_fn, results)
    return(final_result)
  } else {
    return(NULL)
  }
}

#' Monitor Memory Usage During Function Execution
#'
#' @description
#' Wraps a function call with memory usage monitoring, reporting memory usage
#' before, during, and after execution.
#'
#' @param fn Function to execute
#' @param interval Time interval in seconds for memory checks during execution
#' @param ... Arguments passed to fn
#' @return Result of fn with memory usage statistics as an attribute
#' @export
mem_monitor <- function(fn, interval = 1, ...) {
  if (!is.function(fn)) {
    stop("fn must be a function")
  }

  # Initial memory check
  start_mem <- mem_usage(include_gc = TRUE)

  # Start memory tracking
  memory_tracking <- FALSE

  # Try to set up monitoring thread using base parallel package
  if (requireNamespace("parallel", quietly = TRUE)) {
    tryCatch({
      # Create a socket cluster with 1 worker
      cl <- parallel::makeCluster(1)

      # Export necessary functions
      parallel::clusterExport(cl, c("mem_usage"), envir = environment())

      # Start memory tracking (runs in background)
      memory_tracking <- TRUE
      monitor_call <- parallel::parLapply(cl, 1, function(x) {
        memory_track <- list()
        counter <- 1
        monitoring <- TRUE

        while (monitoring) {
          mem <- mem_usage()
          memory_track[[counter]] <- list(
            timestamp = Sys.time(),
            memory_mb = mem$mem_used
          )
          counter <- counter + 1
          Sys.sleep(interval)

          # Exit after a reasonable amount of time (5 minutes)
          if (counter > 300 / interval) monitoring <- FALSE
        }

        return(memory_track)
      })
    }, error = function(e) {
      # If monitoring setup fails, continue without it
      message("Memory monitoring thread could not be started: ", e$message)
      if (exists("cl")) parallel::stopCluster(cl)
      memory_tracking <- FALSE
    })
  }

  # Execute the function
  start_time <- Sys.time()
  result <- tryCatch({
    fn(...)
  }, error = function(e) {
    # If error occurs, stop monitoring and propagate error
    if (memory_tracking && exists("cl")) {
      parallel::stopCluster(cl)
    }
    stop("Error in monitored function: ", e$message)
  }, finally = {
    end_time <- Sys.time()
    elapsed <- difftime(end_time, start_time, units = "secs")

    # Final memory check
    end_mem <- mem_usage(include_gc = TRUE)

    # Add memory usage information to result
    attr(result, "memory_usage") <- list(
      start = start_mem$mem_used,
      end = end_mem$mem_used,
      change = end_mem$mem_used - start_mem$mem_used,
      units = end_mem$units,
      elapsed_time = as.numeric(elapsed)
    )

    # Get memory track if available
    if (memory_tracking && exists("cl")) {
      tryCatch({
        memory_track <- parallel::parLapply(cl, 1, function(x) x)[[1]]
        parallel::stopCluster(cl)

        # Add memory profile if tracking worked
        if (length(memory_track) > 0) {
          memory_profile <- data.frame(
            timestamp = sapply(memory_track, function(x) x$timestamp),
            memory_mb = sapply(memory_track, function(x) x$memory_mb)
          )
          attr(result, "memory_profile") <- memory_profile
        }
      }, error = function(e) {
        if (exists("cl")) parallel::stopCluster(cl)
        message("Could not retrieve memory tracking data: ", e$message)
      })
    }
  })

  # Print summary
  mem_usage <- attr(result, "memory_usage")
  cat(sprintf("Function executed in %.2f seconds\n", mem_usage$elapsed_time))
  cat(sprintf("Memory usage: Start: %.2f %s, End: %.2f %s, Change: %+.2f %s\n",
              mem_usage$start, mem_usage$units,
              mem_usage$end, mem_usage$units,
              mem_usage$change, mem_usage$units))

  return(result)
}

#' Create a Temporary Analysis Environment
#'
#' @description
#' Creates a temporary environment for analysis that isolates objects from
#' the global environment. This helps prevent memory leaks and allows for
#' easy cleanup after analysis.
#'
#' @param parent_env Environment to use as parent (default: parent.frame())
#' @param cleanup Logical, whether to automatically clean up on exit
#' @return New environment for analysis
#' @export
analysis_env <- function(parent_env = parent.frame(), cleanup = TRUE) {
  # Create new environment
  analysis_env <- new.env(parent = parent_env)

  # Add utility functions to the environment
  analysis_env$mem_usage <- mem_usage
  analysis_env$cleanup <- function() {
    # Get all objects in this environment
    obj_names <- ls(envir = analysis_env, all.names = TRUE)

    # Keep utility functions
    utility_funcs <- c("mem_usage", "cleanup", "done")
    obj_names <- setdiff(obj_names, utility_funcs)

    # Remove all objects
    if (length(obj_names) > 0) {
      rm(list = obj_names, envir = analysis_env)
    }

    # Run garbage collection
    gc(verbose = FALSE)

    # Return memory usage
    return(invisible(mem_usage(include_gc = TRUE)))
  }

  # Add function to signal completion and clean up
  analysis_env$done <- function(result = NULL) {
    if (cleanup) {
      analysis_env$cleanup()
    }
    return(result)
  }

  # Set up automatic cleanup if requested
  if (cleanup) {
    do.call("on.exit", list(quote(analysis_env$cleanup()), add = TRUE),
            envir = parent_env)
  }

  return(analysis_env)
}

#' Stream Process Large Files
#'
#' @param file_path Path to the file to process
#' @param process_fn Function to process each chunk/line
#' @param chunk_size Number of lines to read at once
#' @param skip Number of lines to skip at beginning of file
#' @param max_lines Maximum number of lines to process (NULL = all)
#' @param progress Logical, whether to show progress
#' @return Result of processing
#' @export
stream_file <- function(file_path, process_fn, chunk_size = 10000,
                        skip = 0, max_lines = NULL, progress = TRUE) {
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }

  if (!is.function(process_fn)) {
    stop("process_fn must be a function")
  }

  # Get total line count if needed for progress reporting
  if (progress) {
    if (is.null(max_lines)) {
      message("Counting lines in file...")
      # Use base R to count lines
      total_lines <- length(readLines(file_path, warn = FALSE)) - skip
    } else {
      total_lines <- max_lines
    }

    # Simple progress reporting
    progress_step <- max(1, floor(total_lines / chunk_size / 10))
    progress_counter <- 0
    message(sprintf("Processing %d lines in chunks of %d...",
                    total_lines, chunk_size))
  }

  # Open file connection
  con <- file(file_path, "r")
  on.exit(close(con))

  # Skip lines if needed
  if (skip > 0) {
    readLines(con, n = skip)
  }

  # Initialize results
  results <- list()
  result_counter <- 1

  # Initialize line count
  lines_processed <- 0

  # Process file in chunks
  repeat {
    # Read chunk
    lines <- readLines(con, n = chunk_size)

    # Check if we reached end of file
    if (length(lines) == 0) {
      break
    }

    # Check if we've reached max_lines
    if (!is.null(max_lines) && (lines_processed + length(lines)) > max_lines) {
      # Truncate to max_lines
      lines <- lines[1:(max_lines - lines_processed)]
    }

    # Process chunk
    chunk_result <- process_fn(lines)

    # Store result if not NULL
    if (!is.null(chunk_result)) {
      results[[result_counter]] <- chunk_result
      result_counter <- result_counter + 1
    }

    # Update line count
    lines_processed <- lines_processed + length(lines)

    # Update progress
    if (progress) {
      progress_counter <- progress_counter + 1
      if (progress_counter %% progress_step == 0) {
        message(sprintf("Processed %d of %d lines (%.1f%%)",
                        lines_processed, total_lines,
                        100 * lines_processed / total_lines))
      }
    }

    # Check if we've reached max_lines
    if (!is.null(max_lines) && lines_processed >= max_lines) {
      break
    }

    # Clean up to free memory
    rm(lines, chunk_result)
    gc(verbose = FALSE)
  }

  # Combine results if there are any
  if (length(results) > 0) {
    # If results are data frames, try to combine them
    if (all(sapply(results, is.data.frame))) {
      # Use base R rbind
      final_result <- do.call(rbind, results)
    } else {
      # Return list of results
      final_result <- results
    }

    return(final_result)
  } else {
    return(NULL)
  }
}

#' Manage Search Results Cache
#'
#' @description
#' Manages a cache of search results to avoid redundant database queries while
#' keeping memory usage under control.
#'
#' @param operation Operation to perform ("add", "get", "clear", "status")
#' @param key Cache key (usually search query)
#' @param value Value to cache (for "add" operation)
#' @param max_size Maximum cache size in MB (default: 500)
#' @param max_items Maximum number of items to cache (default: 50)
#' @return Varies by operation
#' @export
cache_manage <- function(operation, key = NULL, value = NULL,
                         max_size = 500, max_items = 50) {
  # Use package environment instead of global environment
  pkg_env <- environment()

  # Create cache environment if it doesn't exist
  if (!exists("search_cache_env", envir = pkg_env)) {
    search_cache_env <- new.env()
    search_cache_env$items <- list()
    search_cache_env$metadata <- list()
    search_cache_env$total_size <- 0
    assign("search_cache_env", search_cache_env, envir = pkg_env)
  } else {
    search_cache_env <- get("search_cache_env", envir = pkg_env)
  }

  # Handle different operations
  if (operation == "add") {
    if (is.null(key) || is.null(value)) {
      stop("Both key and value must be provided for 'add' operation")
    }

    # Calculate item size
    item_size <- utils::object.size(value)

    # Check if adding this item would exceed max_size
    if ((search_cache_env$total_size + item_size) / 1024^2 > max_size) {
      # Need to make room - remove oldest items
      items <- search_cache_env$items
      metadata <- search_cache_env$metadata

      # Sort by timestamp
      if (length(metadata) > 0) {
        timestamps <- sapply(metadata, function(x) x$timestamp)
        item_keys <- names(timestamps)

        # Get items sorted by age (oldest first)
        sorted_keys <- item_keys[order(timestamps)]

        # Remove items until we have enough space
        space_needed <- (search_cache_env$total_size + item_size) / 1024^2 - max_size
        space_freed <- 0

        for (old_key in sorted_keys) {
          if (space_freed >= space_needed) break

          # Calculate size of this item
          item_size_mb <- utils::object.size(items[[old_key]]) / 1024^2

          # Remove the item
          search_cache_env$total_size <- search_cache_env$total_size - utils::object.size(items[[old_key]])
          search_cache_env$items[[old_key]] <- NULL
          search_cache_env$metadata[[old_key]] <- NULL

          space_freed <- space_freed + item_size_mb
        }
      }
    }

    # Check if we have too many items
    if (length(search_cache_env$items) >= max_items) {
      # Remove oldest item
      metadata <- search_cache_env$metadata
      timestamps <- sapply(metadata, function(x) x$timestamp)
      oldest_key <- names(timestamps)[which.min(timestamps)]

      search_cache_env$total_size <- search_cache_env$total_size -
        utils::object.size(search_cache_env$items[[oldest_key]])
      search_cache_env$items[[oldest_key]] <- NULL
      search_cache_env$metadata[[oldest_key]] <- NULL
    }

    # Add the new item
    search_cache_env$items[[key]] <- value
    search_cache_env$metadata[[key]] <- list(
      timestamp = Sys.time(),
      size = item_size
    )
    search_cache_env$total_size <- search_cache_env$total_size + item_size

    return(invisible(TRUE))
  } else if (operation == "get") {
    if (is.null(key)) {
      stop("Key must be provided for 'get' operation")
    }

    # Check if key exists
    if (key %in% names(search_cache_env$items)) {
      # Update timestamp
      search_cache_env$metadata[[key]]$timestamp <- Sys.time()

      # Return cached value
      return(search_cache_env$items[[key]])
    } else {
      return(NULL)
    }
  } else if (operation == "clear") {
    if (is.null(key)) {
      # Clear entire cache
      search_cache_env$items <- list()
      search_cache_env$metadata <- list()
      search_cache_env$total_size <- 0
      return(invisible(TRUE))
    } else {
      # Clear specific key
      if (key %in% names(search_cache_env$items)) {
        search_cache_env$total_size <- search_cache_env$total_size -
          utils::object.size(search_cache_env$items[[key]])
        search_cache_env$items[[key]] <- NULL
        search_cache_env$metadata[[key]] <- NULL
        return(invisible(TRUE))
      } else {
        return(invisible(FALSE))
      }
    }
  } else if (operation == "status") {
    # Get cache status
    items <- search_cache_env$items
    metadata <- search_cache_env$metadata

    if (length(items) == 0) {
      return(list(
        item_count = 0,
        total_size_mb = 0,
        max_size_mb = max_size,
        utilization_pct = 0
      ))
    }

    # Calculate sizes
    total_size_mb <- search_cache_env$total_size / 1024^2

    # Get item details
    item_details <- lapply(names(items), function(k) {
      list(
        key = k,
        size_mb = utils::object.size(items[[k]]) / 1024^2,
        timestamp = metadata[[k]]$timestamp,
        age_mins = difftime(Sys.time(), metadata[[k]]$timestamp, units = "mins")
      )
    })

    return(list(
      item_count = length(items),
      total_size_mb = total_size_mb,
      max_size_mb = max_size,
      utilization_pct = total_size_mb / max_size * 100,
      items = item_details
    ))
  } else {
    stop("Invalid operation. Must be one of: 'add', 'get', 'clear', 'status'")
  }
}
