
pretty_ram = function(x) {
 # x = x[!is.na(x)]
  x = x / (1000^3)
  2^round(log2(x), 0)
}
get_test = function(type) {
  if (type == "Programming") "prog"
  else if (type == "Matrix functions") "matrix_fun"
  else if (type == "Matrix calulations") "matrix_cal"
  else if (type == "Read 5MB") "read5"
  else if (type == "Read 50MB") "read50"
  else if (type == "Read 200MB") "read200"
  else if (type == "Write 5MB") "write5"
  else if (type == "Write 50MB") "write50"
  else if (type == "Write 200MB") "write200"
}

get_option = function(type) {
  if (type == "All") NULL
  else if (type == "Standard") FALSE
  else TRUE
}

#res_table = res
clean_table = function(res_table) {

  res_table$time = signif(res_table$time, 4)

  res_table = res_table[, c("id", "rank", "time", "cpu", "byte_optimize", "blas_optimize",
                            "sysname", "ram", "test_group", "cores", "is_user")]

  res_table$byte_optimize  = res_table$byte_optimize > 0
  res_table$sysname[res_table$sysname == "Darwin"] = "Apple"
  res_table$sysname[res_table$sysname == "unix"] = "Unix"
  res_table$ram = pretty_ram(res_table$ram)

  res_table
}
