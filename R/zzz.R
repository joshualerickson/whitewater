.onAttach <- function(libname, pkgname){
  packageStartupMessage(paste0(crayon::black(" Due to potentially crashing (USGS Water Services) REST services \n parallel processing is kept to "), crayon::red(crayon::bold(" 120 requests/min. ")),crayon::black("\n Thank you!")))
}
