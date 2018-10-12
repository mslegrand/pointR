#' starts the app
#' 
#' @export
ptRRun<-function(pointRProject=NULL){
  #clear out any noise
  library(shiny)
  shinyOptions(initialPointRProject=pointRProject)

  
  if (!("package:pointR" %in% search()))
  if (!require(package:pointR)) stop("Calling pointR start function but pointR is not installed.")
  
  shiny::runApp(system.file("App", package = "pointR"), launch.browser = TRUE)
}
