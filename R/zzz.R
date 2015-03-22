.onUnload <- function(libpath) {
  library.dynam.unload("GenomicTuples", libpath)
}
