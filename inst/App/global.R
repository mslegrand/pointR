


.global <- new.env()

version="v.0.3.9.17"



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
       c('shiny','shinyjs', 'R.utils', 'svgR', 'shinyAce', 'stringr', 'jsonlite', 'fs',
          'shinyDMDMenu', 'shinyFiles', 'shinythemes', 'colourpicker', 'shinyWidgets', 
          'bsplus','shinyjqui', 'knitr', 'tidyverse'), 
       library, character.only=TRUE)

#library("RColorBrewer")
# options(shiny.error = recover)
# options(shiny.trace=TRUE)
#some constants


# the following constants would be better if placed in a list or 
# alteratively use lockBinding to fix the value
defTag<-"ptR"
transformTag<-"Transforms"
errorPanelTag<-"errorPanel"
RPanelTag='RPanel'
svgPanelTag<-'svgPanel'
rmdPanelTag<-'rmdPanel'
textPanelTag<-'textPanel'
javascriptPanelTag<-'javascriptPanel'
snippetPanelTag<-'snippetPanel'
tibTag<-'tib'

preprocChoices<-c("onNewPt",  "onMovePt", "onMoveMat")

#----begin external resources loaded prior to server------------
# must be loaded prior to alles
source("util/configIO.R") 
source("util/loadTemplates.R") 
source("util/dbInitiatizers.R") 
source("util/extNmode.R") 
source("util/format.R") 
source("util/utilParser.R")
source("util/utilptR.R")
source("util/utilTibble.R")
source("util/utilColumnType.R")
source("util/utilTransform.R")
source("util/copyAndRenameProject.R")
source("fileIO/dndSnippetLoader.R")
source("fileIO/genShinyFilesOpenButtons.R")
source("fileIO/genShinyFilesSaveButtons.R")
source("leftPanel/menu/UIbuildLeftMenu.R")
source("leftPanel/toolbar/buildLeftHToolBarUI.R")
source("rightPanel/preProc/preProcValidate.R")
source("rightPanel/preProc/pointPreprocessor.R")
source("rightPanel/menu/buildRightMenuUI.R")
source("leftPanel/mid/shinyAce4Ptr.R")



#---load modules source -------------
source("rightPanel/footer/moduleFooterRight.R")
source("rightPanel/header/moduleEdAsset.R")
source("rightPanel/header/moduleEdTib.R")
source("rightPanel/header/moduleEdTransform.R")
source("rightPanel/mid/moduleLog.R")
source("rightPanel/mid/moduleRowDND.R")
source("rightPanel/mid/moduleSVGR.R")

