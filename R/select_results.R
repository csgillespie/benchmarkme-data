#' Selecting results
#' 
#' Selects and aggregates over the \code{past_results_v2} data set or the 
#' \code{results} input data set..
#' @return A data frame
#' @param results Default \code{NULL}. If \code{NULL} the \code{past_results_v2}
#' data set is used. Otherwise, the input data set.
#' @inheritParams plot_past
#' @export
#' @examples 
#' select_results("prog", blas_optimize = FALSE)
select_results = function(test_group, 
                          results = NULL,
                          blas_optimize = NULL, 
                          cores = 0, 
                          normalize = FALSE) {
  
  ## Sort out arguments
  if(cores == 0) {
    cores = 1
    parallel = FALSE
  } else {
    parallel = TRUE
  }
  
  if(isTRUE(normalize) && isFALSE(parallel)) {
    stop("normalize can only be used when parallel is TRUE", call. = FALSE)
  }
  
  if(isTRUE(normalize)) cores = c(1, cores)
  if(is.null(blas_optimize)) blas_optimize = c(FALSE, TRUE)
  
  ## Load past data
  tmp_env = new.env()
  data(past_results_v2, package="benchmarkmeData", envir = tmp_env)

  # Format data
  results = results %>%
    bind_rows(tmp_env$past_results_v2) %>%
    filter(blas_optimize %in% !!blas_optimize) %>% 
    filter(test_group == !!test_group[1]) %>%
    filter(cores %in% !!cores) %>%
    filter(parallel == !!parallel) %>%
    filter(!is.na(time)) %>%
    group_by(id, cpu, date, sysname, blas_optimize, test_group, ram, cores) %>%
    summarise(time = sum(time)) %>%
    ungroup()
  
  if (normalize) {
   results = results %>%
      group_by(id) %>%
      summarise(n_time = (time/time[cores == 1])[cores != 1], 
                cores = max(cores)) %>%
      left_join(results, by = c("id", "cores")) %>%
      select(-time) %>%
      rename(time = n_time)
  }

  results = results %>%
    arrange(time) %>%
    mutate(rank = 1:length(time)) %>%
    select(rank, everything())
  results
}
