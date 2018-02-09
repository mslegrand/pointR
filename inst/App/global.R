
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

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

list.entry.at.index<-function(ll, indx){
  if(!is(ll,'list')){
    cat('1\n')
    return(NULL)
  }
  if(!(class(indx) %in% c('integer', 'numeric'))){ 
    cat('2\n')
    return(NULL)
  } 
  if(!(indx>=1 && indx<= length(ll))){
    cat('3\n')
    return(NULL)
  }
  ll[[indx]] 
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


r_pkgs<-c('shiny','shinyjs', 'R.utils', 'svgR', 'shinyAce', 'stringr', 'jsonlite', 'shinyDMDMenu', 'shinyFiles', 'shinythemes', 'colourpicker', 'shinyWidgets', 'bsplus')
sapply(r_pkgs, library, character.only=TRUE)


# options(shiny.error = recover)
# options(shiny.trace=TRUE)
#some consts
defTag<-"ptR"
transformTag<-"Transforms"
logTag<-"logPanel"

getSVGWH<-function(){ c(650,620)} #this should be ultimately place in another file

#----begin external resources loaded prior to server------------
#source("utilStyle.R")

source("util/configIO.R") # must be loaded prior
source("util/utilFormat.R") 
source("util/format.R") 
source("util/utilParser.R")
source("util/utilptR.R")
source("util/utilTibble.R")
source("util/utilColumnType.R")
source("util/utilTransform.R")
source("leftPanel/menu/buildLeftMenuUI.R")
source("rightPanel/menu/buildRightMenuUI.R")
source("leftPanel/mid/shinyAce4Ptr.R")



#---load modules source -------------

source("rightPanel/footer/moduleFooterRight.R")
source("rightPanel/header/moduleEdTib.R")
source("rightPanel/mid/moduleSVGR.R")

