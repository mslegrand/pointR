.global <- new.env()

initResourcePaths <- function() {
  if (is.null(.global$loaded)) {
    shiny::addResourcePath(
      prefix = 'pointR',
      directoryPath = system.file('www', package='pointR')
    )
    shiny::addResourcePath(
      prefix = 'shinyAce',
      directoryPath = system.file('www', package='shinyAce')
    )
     .global$loaded <- TRUE
  }
  HTML("")
}