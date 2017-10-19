
# defines
#-------------------------------

"library(svgR)
WH<-c(600,400)

#Defined by mouse: edit with care!
ptR<-list(

  x=c()

)

svgR(wh=WH, 
#your custom code goes here

  NULL



)
"->codeTemplate

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
  if(length(ptRList)>0){
    replacement<-formatPtDefs(defTag=defTag, ptRList=ptRList)
    txt<-replaceDef(txt, replacement, defTag=defTag) 
  } else {
    txt
  }
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
    getDefPos(txt, defTag)
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
  if(length(ptDefs)==0){
    return(list(pts=c(), df=c()))
  }
  if( any(grepl(ptTag,src) ) ){
    try({
      ptDefTxt1<-getDef(src, defTag=ptTag)
      if( is.null(ptDefTxt1)){
        #stop("failed to fint ptR")
        # WITH DISABLE GRAPH BAR PTR CONTROLS
        # PTR SELECTION, GROUPS, EDITS, ...
        # KEEP TRANSFORMATIONS FOR THE TIME BEING
        ptDefs$pts<-list()
      } else {
        eval(parse(text=ptDefTxt1))
        ptDefs$pts<-get(ptTag)
      }
      
            
      ptDefTxt2<-getDef(src, defTag=dfTag)
      if(length(ptDefTxt2)>0){ # ptR.df is optional!
        #1. replace data.frame with list
        dfListText<-sub("data.frame","list",ptDefTxt2)
        eval(parse(text=dfListText))
        #2 dfList<-get(dfTag)
        dfList<-get(dfTag)
        #3 pad list back as data.frame
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
# inserts ptDefs into src code 
# !!! to be obsolete soon
# preProcCode<-function(src){
#   ptDefs<-ex.getPtDefs(src)
#   ptRList<-ptDefs$pts
#   dfList<-ptDefs$df
#   src<-pts2Source(src,ptRList)
#   if(!is.null(dfList)){
#     src<-df2Source(src, dfList)
#   }
#   return(src)
# } 



formatTrs<-function(tr){ #not used
  paste0('"',tr,'"')
}


## proposed replacement for point2src
defTag2replace<-function(defTag, defList, txt){
  if(defTag=='ptR'){
    replacement<-formatPtDefs(defTag=defTag, ptRList=defList)
  } else {
    replacement<-formatDFDefs(defTag=defTag, dfDefsTag=defList)
  }
  p.df<-getParseDataFrame(txt)
  tag.df<-extractTagDF(p.df, tag=defTag)
  if( !is.null(tag.df) ){
    pos<- list(
      startRow= tag.df$line1 -1,
      startColumn=tag.df$col1 -1 ,
      endRow= tag.df$line2 -1,
      endColumn=tag.df$col2 
    )
  } else {
    pos<-NULL # !!! To rewrite, if defTag=='tagR' and tagR not in txt, place after ptR and add some empty lines
  }
  if(!is.null(pos)){
    replacementList<-list(
      list(
        rng=pos,
        txt= replacement
      )
    )
  } else {
    replacementList<-NULL
  }
  #print(replacementList)
  replacementList
}

ptDef2ReplacementList<-function(newPtDef, txt){
  replacementList<-list()
  pt.repl<-formatPtDefs(defTag='ptR', ptRList=newPtDef$pts)
  p.df<-getParseDataFrame(txt)
  ptR.df<-extractTagDF(p.df, tag='ptR')
  pt.Pos<-list(
    startRow= ptR.df$line1 -1,
    startColumn=ptR.df$col1 -1 ,
    endRow= ptR.df$line2 -1,
    endColumn=ptR.df$col2 
  )
  
  if(length(newPtDef$df)>0){
    tag.repl<-formatDFDefs(newPtDef$df)
    tagR.df<-extractTagDF(p.df, tag='tagR')
    if( !is.null(tagR.df) ){
      tag.Pos<-list(
        startRow= tagR.df$line1 -1,
        startColumn=tagR.df$col1 -1 ,
        endRow= tagR.df$line2 -1,
        endColumn=tagR.df$col2 
      )
      }else{
        tag.Pos<-list(
          startRow= ptR.df$line2,
          startColumn=ptR.df$col2+1 ,
          endRow= ptR.df$line2,
          endColumn=ptR.df$col2+1 
        )
        tag.repl<-paste0("\n",tag.repl,"\n")
      }
    replacementList<-c(replacementList, list(list(rng=tag.Pos, txt= tag.repl)))
  }
  replacementList<-c(replacementList, list(list(rng=pt.Pos, txt= pt.repl)))
  replacementList
}

# Testing code

# txt<-codeTemplate
# 
# newPtDef<-list(
#   pts=list(x=matrix(1:8,2))
# )
# 
# t1<-ptDef2ReplacementList(newPtDef, txt )
# t1
# 
# newPtDef<-list(
#   pts=list(x=matrix(1:8,2)),
#   df=list(x=data.frame(tag=c(1,3), stringsAsFactors = FALSE))
# )
# 
# t2<-ptDef2ReplacementList(newPtDef, txt )
# t2
# 

# "library(svgR)
# WH<-c(600,400)
# 
# #Defined by mouse: edit with care!
# ptR<-list(
#   x=matrix(c(1,2,3,4),2)
# )
# 
# tagR<-list(
#   x=data.frame(
#     tag=c(1),
#     stringsAsFactors = FALSE
#   )
# )
# 
# svgR(wh=WH,
# #your custom code goes here
# 
#   NULL
# 
# 
# 
# )
# "->txt3
# 
# newPtDef<-list(
#   pts=list(
#     x=matrix(c(1,2,3,4,5,6),2)
#   ),
#   tagR=list(
#     x=data.frame(
#       tag=c(1),
#       stringAsFactors=FALSE
#     )
#   )
# )
# 
# t3<-ptDef2ReplacementList(newPtDef, txt3 )
# t3
