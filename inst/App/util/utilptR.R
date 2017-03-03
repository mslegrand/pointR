
# defines
#-------------------------------
paste(names(svgR:::eleDefs), collapse=" ")->element.names

paste0("#svgR elements: ", element.names, "\n",
"library(svgR)
WH<-c(1500,1500)

#Defined by mouse: edit with care!
ptR<-list(
  x=c()
)

svgR(wh=WH, 
#your custom code goes here

  NULL



)
")->codeTemplate

# "library(svgR)
# WH<-c(1500,1500)
# 
# #Defined by mouse: edit with care!
# ptR<-list(
#   x=c()
# )
# 
# svgR(wh=WH, 
# #your custom code goes here
# 
# NULL
# 
# 
# )
# "->codeTemplates2
#codeTemplates2 fails in utilTransform, I need to
# rewrite utilTransform before we can use this.

#------------------------

#---external fns----
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



as.text<-function(q){
  paste(deparse(q), collapse="\n")
}

getScript<-function(file){
  paste0(readLines(file),collapse="\n")
}

readFile<-function(fileName){
  paste0(readLines(fileName),collapse="\n")
}

getDefPos<-function(txt, defTag){
  p.df<-getParseDataFrame(txt)
  cumCharLines<-getcumCharLines(txt)
  tag.df<-extractTagDF(p.df, tag=defTag)
  if( !is.null(tag.df) ){
    pos<-extractPositions(cumCharLines, tag.df)
  } else {
    pos<-NULL
  }
}


replaceDef<-function(txt, replacement, defTag){
  pos<-getDefPos(txt, defTag)
  if(is.null(pos)){
    return(txt)
  }
  if(length(replacement)==0){
    replacement=" "
  }
  s<-paste0(
    substr(txt, 1, pos[1]-1),
    replacement,
    substr(txt,pos[2]+1, nchar(txt) ),
    sep=""
  )
  return(s)
}

replaceTxt<-function(txt, replacements, positions){
  stopifnot(length(replacments)+1==ncol(positions))
  txtKeep<-textOutsidePos(txt, positions)
  replacements<-c(replacements,"")
  newtxt<-rbind(txtKeep,replacements)
  newtxt<-paste0(nextxt, collapse="")
}

replaceDefs<-function(txt, replacements, defTags){
  positions<-sapply(defTags, function(defTag)
    getDefsPos(txt, defTag)
  )
  replaceTxt(txt, replacements, defTags)
}
  
getDef<-function(txt, defTag ){
  pos<-getDefPos(txt, defTag)
  if(is.null(pos)){
    return(NULL)
  }
  return(substr(txt, pos[1], pos[2]))
}


ex.getPtDefs<-function(src, ptTag="ptR", dfTag="tagR"){
  ptDefs<-list(pts=NULL, df=NULL)
  #defTag<-"Pts" #ptDefs"
  if( any(grepl(ptTag,src) ) ){
    try({
      ptDefTxt1<-getDef(src, defTag=ptTag)
      stopifnot(!is.null(ptDefTxt1))
      eval(parse(text=ptDefTxt1))
      ptDefs$pts<-get(ptTag)
      
      ptDefTxt2<-getDef(src, defTag=dfTag)
      
      if(!is.null(ptDefTxt2)){ # ptR.df is optional!
        #1. replace data.frame with list
        dfListText<-sub("data.frame","list",ptDefTxt2)
        eval(parse(text=dfListText))
        #2 dfList<-get(dfTag)
        dfList<-get(dfTag)
        3 #pad list back as data.frame
        df<-sapply(dfList, list2DF,
          simplify = FALSE
        )
        # 4 set ptDefs to tmp.df
        ptDefs$df<-df
        
      }
    })
  }
  return(ptDefs)
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



formatTrs<-function(tr){ #not used
  paste0('"',tr,'"')
}


