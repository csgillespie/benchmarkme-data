summarise_results = function(res) {
  id = res$id
  date = res$date
  
  results = res$results
  no_of_rep = nrow(results)/length(unique(results$test))
  timings = tapply(results[,3], results[,5], function(i) sum(i)/no_of_rep)
  tests = names(timings)
  values = as.vector(timings)
  
  cpus = gsub("(?<=[\\s])\\s*|^\\s+$", "", res$cpu$model_name, perl=TRUE)
  ram = res$ram
  byte_optimize = as.vector(res$byte_compiler)
  
  r_major = res$r_version$major
  r_minor = res$r_version$minor
  
  if(length(res$sys_info) ==1 && is.na(res$sys_info)){
    sysname = release = NA
  } else{
    sysname = res$sys_info$sysname
    release = res$sys_info$release
  } 
  data.frame(id, date, time=timings, test=tests, 
             cpu=cpus, ram=as.numeric(ram), byte_optimize, 
             r_major, r_minor, 
             sysname, release,
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
