# foliage height density
fun_fhd <- function(a) {
  
  sum(calc(x = a, fun = function(a){-1 * ((a/a[[length(a)]]) * log(a / (a/a[[length(a)]])))}))
}
# vertical distribution ratio
fun_vdr <- function(max,med) {
  (max - med) / max
}

