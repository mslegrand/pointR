


.global <- new.env()


version=paste0("v.", read.dcf(system.file('DESCRIPTION', package='pointR'))[,'Version'])


if(!is.null(getShinyOption("HOME"))){
  homeDir=getShinyOption("HOME")
} else {
  homeDir="~"
}

# for debugging
containsNA<-function(d){
  length(d)==0 || any(unlist(sapply(d,is.na)))
}

initResourcePaths <- function() {
  if (is.null(.global$loaded)) {
    shiny::addResourcePath(
      prefix = 'pointR',
      directoryPath = system.file('App', package='pointR'))
    .global$loaded <- TRUE
  }
  HTML("")
}


`%$$%`<-function(x,y){
  if(is.null(x) || is.na(x) || is.null(y) ){ 
    NULL
  } else {
    x[[y]]
  }		
}

allGood<-function(...) {
  all(sapply(list(...),length)!=0) && !any(sapply(list(...),is.na)!=0)
}

notNull<-function(...) {
  all(sapply(list(...),length)!=0) 
}


sapply(
       c('shiny','shinyjs', 'shinyalert', 'R.utils', 'svgR', 'shinyAce', 'jsonlite', 'rowPicker', 'fs',
          'shinyDMDMenu', 'shinyFiles', 'shinythemes', 'colourpicker', 'shinyWidgets', 'jqScrollBar',
          'bsplus','shinyjqui', 'knitr', 'tidyverse'), 
       library, character.only=TRUE)
# 'stringr', , in "tidyverse"

#library("RColorBrewer")
# options(shiny.error = recover)
# options(shiny.trace=TRUE)
#some constants


# the following constants would be better if placed in a list or 
# alteratively use lockBinding to fix the value
defTag<-"ptR"

# appears in serverPanelDispatch
transformTag<-"Transforms"
svgPanelTag<-'svgPanel'
errorPanelTag<-"errorPanel"
RPanelTag='RPanel'
appPanelTag<-'appPanel'
rmdPanelTag<-'rmdPanel'  
textPanelTag<-'textPanel'
snippetPanelTag<-'snippetPanel'
javascriptPanelTag<-'javascriptPanel'



tibTag<-'tib'
resourceDir='aux' 


preprocChoices<-list(points=c("onNewPt",  "onMovePt", "onMoveMat"), attrs=c('onNewRow', 'onChangeRow'))

#----begin external resources loaded prior to server------------
# must be loaded prior to alles
source("util/configIO.R") 
source("util/loadTemplates.R") 
source("util/extNmode.R") 
source("util/logger.R")

#---used to build the UI portion
source('util/pointRLogoSVG.R')
source("fileIO/genShinyFilesOpenButtons.R")
source("fileIO/genShinyFilesSaveButtons.R")
source("leftPanel/menu/UIProjectTemplateMenu.R")

source("leftPanel/menu/UIbuildLeftMenu.R")
source("leftPanel/mid/UIcontextMenu.R")

source("leftPanel/toolbar/buildLeftHToolBarUI.R")

source("rightPanel/preProc/pointPreprocessor.R")
source("rightPanel/menu/buildRightMenuUI.R")
source("leftPanel/mid/shinyAce4Ptr.R")




#---load modules source -------------
source("rightPanel/footer/moduleFooterRight.R")
source("rightPanel/header/moduleEdAsset.R")
source("rightPanel/header/moduleEdTib.R")
source("rightPanel/header/moduleEdTransform.R")
source("rightPanel/mid/moduleLog.R")
#source("rightPanel/mid/moduleRowDND.R")
source("rightPanel/mid/moduleSVGR.R")

