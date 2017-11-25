
.global <- new.env()

initResourcePaths <- function() {
  if (is.null(.global$loaded)) {
    shiny::addResourcePath(
      prefix = 'pointR',
      directoryPath = system.file('App', package='pointR'))
    .global$loaded <- TRUE
  }
  HTML("")
}



# pkgsCRAN<-c("stringr", "svDialogs", "devtools",  "lintr")
# for(pkgName in pkgsCRAN){  
#   if(!require(pkgName, character.only=TRUE)){
#     #install.packages(pkgName)
#     library(pkgName, character.only = TRUE)
#   }  
# }            

# pkgs<-list(
#            "shinyjs"="daattali", 
#            "svgR"="mlegrand",
#            "shinyDMDMenu"="mlegrand",
#            "shinyAce"="trestletech")
# for(pkgName in names(pkgs)){  
#   if(!require(pkgName, character.only=TRUE)){
#     #install_github(pkgName, pkg[[pkgName]])
#     library(pkgName, character.only=TRUE)
#   }  
# }


r_pkgs<-c('shiny','shinyjs', 'R.utils', 'svgR', 'shinyAce', 'stringr', 'jsonlite', 'shinyDMDMenu', 'shinyFiles', 'shinythemes')
sapply(r_pkgs, library, character.only=TRUE)


# options(shiny.error = recover)
# options(shiny.trace=TRUE)

#----begin external resources loaded prior to server------------
#source("utilStyle.R")
source("util/configIO.R") # must be loaded prior
source("util/utilFormat.R") 
source("util/format.R") 
source("util/utilParser.R")
source("util/utilptR.R")
source("util/utilTransform.R")
source("leftPanel/shinyAce4Ptr.R")

#some consts
defTag<-"ptR"
#this should be ultimately place in another file
getSVGWH<-function(){ c(650,620)}


#---load modules source -------------
source("rightPanel/modulePointsBar.R")
source("rightPanel/moduleTagValue.R")
source("rightPanel/moduleTagDrag.R")
source("rightPanel/moduleSVGR.R")

