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
#' select_results("prog", blas_optimize = NULL)
select_results = function(test_group, 
                          results = NULL,
                          blas_optimize = NULL, 
                          cores = 0) {
  
  if (is.null(blas_optimize)) blas_optimize = c(FALSE, TRUE)
  
  ## Load past data
  tmp_env = new.env()
  data(past_results_v2, package = "benchmarkmeData", envir = tmp_env)

  # Format data
  results = results %>%
    bind_rows(tmp_env$past_results_v2) %>%
    filter(blas_optimize %in% !!blas_optimize) %>% 
    filter(test_group == !!test_group) %>%
    filter(cores %in% !!cores) %>%
    filter(!is.na(time)) %>%
    group_by(id, cpu, date, sysname, blas_optimize, test_group, ram, cores) %>%
    summarise(time = sum(time)) %>%
    ungroup()

  results = results %>%
    arrange(time) %>%
    mutate(rank = 1:length(time)) %>%
    select(rank, everything())
  results
}
