
pkgsCRAN<-c("stringr", "svDialogs", "devtools",  "lintr")
for(pkgName in pkgsCRAN){  
  if(!require(pkgName, character.only=TRUE)){
    install.packages(pkgName)
    library(pkgName, character.only = TRUE)
  }  
}            

pkgs<-list(
           "shinyjs"="daattali", 
           "svgR"="mlegrand","shinyAce"="trestletech")
for(pkgName in names(pkgs)){  
  if(!require(pkgName, character.only=TRUE)){
    install_github(pkgName, pkg[[pkgName]])
    library(pkgName, character.only=TRUE)
  }  
}

# library(shiny)
# library(stringr)
# library(svDialogs) #!!!todo: replace this

#options(shiny.error = recover)

#----begin external rc------------
#source("utilStyle.R")
source("utilFormat.R")
source("utilParser.R")
source("utilptR.R")
source("utilTransform.R")

defTag<-"ptR"

js.scripts<-list(
  Points=readFile("www/pointsIO.js"),
  TagVal=readFile("www/tagValIO.js"),
  Translate=readFile("www/transIO.js"),
  Rotate=readFile("www/rotIO.js"),
  Scale=readFile("www/scaleIO.js"),
  TagDrag=readFile("www/transTag.js")
)

#---begin external---------
#these functions should be ultimately place in another file
getSVGWH<-function(){ c(650,620)}

pts2Source<-function(txt,ptRList){
  replacement<-formatPtDefs(defTag=defTag, ptRList=ptRList)
  txt<-replaceDef(txt, replacement, defTag=defTag) 
}

df2Source<-function(txt, dfList){
  if(length(dfList)>0){
    replacement<-formatDFDefs(dfList)
  } else {
    replacement<-""
  }
  txt<-replaceDef(txt, replacement, defTag="tagR") 
}

#used by open and commit
preProcCode<-function(src){
  ptDefs<-ex.getPtDefs(src)
  ptRList<-ptDefs$pts
  dfList<-ptDefs$df
  src<-pts2Source(src,ptRList)
  if(!is.null(dfList)){
    src<-df2Source(src, dfList)
  }
  return(src)
} 

# called by either a new/load source or upon a commit
ex.getSelectInfo<-function(ptRList, selected, point.index){
  choices<-names(ptRList)
  if(length(choices)==0 ){
    rtv<-list(selected=NULL, point.index =0 )  
    return(rtv)
  }
  
  if(length(selected)<1 || !(selected %in% choices) ){ # a new choice
    #pick the first choice candidate
    selected=choices[1]
    pts<-ptRList[[selected]]
    point.index<-length(pts)/2
    rtv<-list(
      selected=selected,
      point.index=point.index
    )
    return(rtv)
  }
  #default: an existing choice
  point.index<-min(point.index, length( ptRList[[selected]])/2 ) #cannot be longer than the number of points
  rtv<-list(
    selected=selected, 
    point.index=point.index
  )
  return(rtv)  
}
#
#----end external ------------

source("modulePointsBar.R")
source("moduleTagValue.R")
source("moduleTagDrag.R")
source("moduleSVGR.R")
source("ptRAce.R")
