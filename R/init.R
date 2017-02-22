.global <- new.env()

initResourcePaths <- function() {
  if (is.null(.global$loaded)) {
    shiny::addResourcePath(
      prefix = 'pointR',
      directoryPath = system.file('www', package='pointR'))
     .global$loaded <- TRUE
  }
  HTML("")
}