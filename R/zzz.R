.onAttach <- function(libname, pkgname){
  packageStartupMessage(paste0(crayon::black(" Due to potentially crashing (USGS Water Services) REST services keep your parallel processing to \n"), crayon::red(crayon::bold(" 10 cores/workers or less \n")),crayon::black(" until further notice. Thank you!")))
}
