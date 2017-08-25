#' BLAS optimize
#' 
#' Try to determine parallel BLAS, which implies non-standard R!
#' Compare user with elapsed time. If user >> elapsed, then parallel BLAS
#' @param results The output from a \code{benchmark_*} call.
#' @export
is_blas_optimize = function(results){
  ## 
  user_times = tapply(results$user, results[, 5], sum)
  elapsed_times = tapply(results$elapsed, results[, 5], sum)*1.1
  blas_optimize = any(user_times > elapsed_times)
  blas_optimize  
}


#' @rdname select_results
#' @param res A list containing benchmark results and system information.
#' @export
summarise_results = function(res) {
  id = res$id
  date = res$date
  results = res$results
  
  ## Make past versions consistent with current
  if(is.null(results$cores)) 
    results$cores = 0
  colnames(results)[5] = "test_group"
  
  
  
  #no_of_rep = nrow(results)/length(unique(results$test))
 # timings = tapply(results[,3], results[,5], function(i) sum(i)/no_of_rep)
  timings = aggregate(x = results$elapsed, 
                      by = list(test_group = results$test_group, cores =results$cores), 
                      FUN = "mean")
  
  
  tests = timings$test_group
  cores = timings$cores
  values = timings$x
  
  blas_optimize = is_blas_optimize(results)
  cpus = gsub("(?<=[\\s])\\s*|^\\s+$", "", unique(res$cpu$model_name), perl=TRUE)
  ram = res$ram
  byte_optimize = as.vector(res$byte_compiler)
  
  r_major = res$r_version$major
  r_minor = res$r_version$minor
  
  if(length(res$sys_info) == 1 && is.na(res$sys_info)) {
      release = NA
    if(length(res$platform_info) == 1 && is.na(res$platform_info)) {
      sysname = NA
    } else {
      sysname = res$platform_info$OS.type
    }
  } else{
    sysname = res$sys_info$sysname
    release = res$sys_info$release
  } 
  
  if(!is.na(sysname) && sysname == "windows") 
    sysname = "Windows"
  
  data.frame(id, date, time=values, test_group=tests, 
             cpu=cpus, ram=as.numeric(ram), byte_optimize, 
             r_major, r_minor, 
             sysname, release, blas_optimize, cores,
             stringsAsFactors = FALSE)
}

#' @rdname move_files
#' @export
make_data_set = function(from) {
  files = list_files(from)
  l = lapply(files, function(i) summarise_results(readRDS(i)))
  all_res = Reduce("rbind", l)
  rownames(all_res) = 1:nrow(all_res)
  all_res
}



