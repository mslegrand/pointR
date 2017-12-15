
# defines
#-------------------------------

"library(svgR)
library(tidyverse)
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


# defines
#-------------------------------

"library(svgR)
library(tidyverse)
WH<-c(600,400)

# Defined by mouse: edit with care!
ptR<-list(
  x=tribble(
   ~pts,
   matrix(NA,2,0)
  )
)

svgR(wh=WH,
#your custom code goes here
NULL
)
"->codeTemplate

# defines
#-------------------------------

"library(svgR)
library(tidyverse)
WH<-c(600,400)

# Defined by mouse: edit with care!
ptR<-list(
  x=tribble(
   ~x,             ~fill,
   matrix(NA,2,0), 'red'
  )
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


matrices2tags<-function(mats){
  tmp<-cumsum(sapply(mats,ncol))+1
  c(1,tmp[-length(tmp)])
}


ex.getPtDefs<-function(src, ptTag="ptR", dfTag="tagR"){
  ptDefs<-list(pts=NULL, df=NULL)
  if(length(ptDefs)==0){
    return(list(pts=c(), df=c(), tib=c()))
  }
  if( any(grepl(ptTag,src) ) ){
    try({
      ptDefTxt1<-getDef(src, defTag=ptTag)
      if( is.null(ptDefTxt1)){
        #stop("failed to fint ptR")
        # WITH DISABLE GRAPH BAR PTR CONTROLS
        # PTR SELECTION, GROUPS, EDITS, ...
        # KEEP TRANSFORMATIONS FOR THE TIME BEING
        #ptDefs$pts<-list()
        ptDefs$tib<-list()
      } else {
        eval(parse(text=ptDefTxt1)) #stupid eval to obtain the points
        
        #!!!KLUDGE first kludge (undo later)
        ptDefs$tib<-get(ptTag) #at this stage we have ptR as a list of tibbles, each tibble containings points with name same as tib
        # TODO add columnInfo to ptDefs
        # for each named tib, add a named vector with
        # rows correspoinding to the names of the tib and values appropriately guessed
        
        
        # ptDefs$pts<-lapply(names(ptDefs$tib), function(nm){
        #   pts<-unlist(ptDefs$tib[[nm]][[nm]]) #!!!temporary change this to be more general later
        #   #pts<-lapply(ptR, function(x)do.call(cbind, x)) 
        #   if(length(pts)>0)
        #     matrix(unlist(pts),2)
        #   else
        #     matrix(NA,2,0)
        # })
        # names(ptDefs$pts)<-names(ptDefs$tib) #!!! or put in structure
        # ptDefs$df<-lapply(names(ptDefs$tib), function(nm){
        #   df<-ptDefs$tib[[nm]]
        #   pts<-df[[nm]] #!!!temporary change this to be more general later
        #   tdf<-tibble(tag=matrices2tags(pts))
        #   df<-cbind(tdf,df)
        #   df[,-which(names(df)=='pts')] #!!!temporary change this to be more general later
        # })
        #names(ptDefs$df)<-names(ptDefs$tib)
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


## proposed replacement for point2src !!!NEVER USED !!!DELETE THIS FUNCTION
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
  replacementList
}

ptDef2ReplacementList<-function(name, newPtDef, txt){
  replacementList<-list()
  # get the text for the point replacement  
  #pt.repl<-fmtPtR(newPtDef$pts)
  pt.repl<-fmtTribbleList(newPtDef$tib)
  
  p.df<-getParseDataFrame(txt)
  ptR.df<-extractTagDF(p.df, tag='ptR')
  pt.Pos<-list(
    startRow= ptR.df$line1 -1,
    startColumn=ptR.df$col1 -1 ,
    endRow= ptR.df$line2 -1,
    endColumn=ptR.df$col2 
  )
  
  # if(length(newPtDef$df)>0){
  #   tag.repl<-formatDFDefs(newPtDef$df)
  #   tagR.df<-extractTagDF(p.df, tag='tagR')
  #   if( !is.null(tagR.df) ){
  #     tag.Pos<-list(
  #       startRow= tagR.df$line1 -1,
  #       startColumn=tagR.df$col1 -1 ,
  #       endRow= tagR.df$line2 -1,
  #       endColumn=tagR.df$col2 
  #     )
  #     }else{
  #       tag.Pos<-list(
  #         startRow= ptR.df$line2,
  #         startColumn=ptR.df$col2+1 ,
  #         endRow= ptR.df$line2,
  #         endColumn=ptR.df$col2+1 
  #       )
  #       tag.repl<-paste0("\n",tag.repl,"\n")
  #     }
  #   replacementList<-c(replacementList, list(list(rng=tag.Pos, txt= tag.repl)))
  # }
  replacementList<-c(replacementList, list(list(rng=pt.Pos, txt= pt.repl)))
  replacementList
}


panel2svgid<-function(panelName){
  prefix2<-'ptR_SVG_'
  paste0(prefix2, "_", toupper(paneName))
}

panel2onmousedown<-function(panelName, transformOption=NULL){
  prefix1<-'ptRPlotter_'
  mid=''
  if(!is.null(transformOption)){
    mid=paste('_',toupper(transformOption))
  }
  paste0(prefix1,panel2svgid,mid,".selectElement(evt)")
}

panel2var<-function(panelName, transformOption=NULL){
  prefix1<-'ptRPlotter_'
  mid=''
  if(!is.null(transformOption)){
    mid=paste('_',toupper(transformOption))
  }
  paste0(prefix1,panel2svgid,mid)
}

panel2script<-function(panelName, transformOption=NULL){
  #'var ptRPlotter_ptR_SVG_TRANSFORM_TRANSLATE = new PtRPanelTranslate("ptR_SVG_TRANSFORM");'
  paste0( 'var ', panel2var(panelName, transformOption), ' = new ', jsConstr, "(\"", panel2svgid, "\");" )
}

tags2listmatrices<-function( matrixPts, tags){
  # assume tags start with 1
  
  if(length(matrixPts)==0){
    matrixPts=matrix(0,2,0)
  } else {
    matrixPts<-matrix(matrixPts,2)
  }
  if(length(tags)>0){
    tags<-c(tags, (ncol(matrixPts)+1))
    indx<-lapply(1:(length(tags)-1), function(x){tags[x]:(tags[x+1]-1) })
    tmp<-lapply(indx, function(i) matrixPts[,i])
    tmp<-lapply(tmp, function(x)as.matrix(x,2))
  } else {
    tmp<-list(matrixPts)
  }
  tmp
}

#!!! TODO this is temp: refactor or remove
# Replaces pts by indexed set
olde2newFmtPtDef2ListOfLists<-function(newPtDef){
  pts<-newPtDef$pts # should be a list of list of matrices!!!
  
  newPtDef$pts<-sapply(names(pts), function(nm){

    pts<-as.integer(unlist(newPtDef$pts[[nm]]))
    tags<-newPtDef$df[[nm]]$tag
    ptsL<-tags2listmatrices(pts,tags)
  }, simplify=FALSE)
  newPtDef
}

#!!! kludge to convert output to ints
pts2Integers<-function(newtib){
  #ptsCol<-'pts' #!!!kludge: must change shortly CHANGED: REPLACED ptsCol with mn
  
  for(nm in names(newtib)){
    ptsll<-newtib[[nm]][[nm]] #  !!!KLUDGE  ptsll is a list of matrices
    newtib[[nm]][[nm]]<-sapply(ptsll, function(mm){
      matrix(as.integer(mm),2)
    }, 
    simplify=FALSE)
  }
  newtib
}



## looks at the class of the last entry to return selectorType
# tib2ColType<-function( tib, column){
#   lastVal<-last(tib[[column]])
#   cv=class(lastVal)
#   if(is.character(cv) && isColor(lastVal)){
#     cv<-'color'
#   }
#   if(cv)
#   cv
# }

#this would be used when opening a file
## choices 
choices2ColType<-function( choices, column){
  ct<-NULL
  lastVal<-last(choices)
  cv=class(lastVal)
  ct<-list(cv)
  if(is.character(cv) && isColor(lastVal)){
    ct<-list('colorPicker')
  }
  if(column %in% c('opacity' )){
    ct<-list('slider','decimal',0,1)
  }
  ct
}

#this would be used when adding an new column
val2ColType<-function(val, column){
  tmp<-type.convert(val)
  cv=class(lastVal)
  ct<-list(cv)
  if(is.character(cv) && isColor(lastVal)){
    ct<-list('colorPicker')
  }
  if(column %in% c('opacity' )){
    ct<-list('slider','decimal',0,1)
  }
  ct
}

 # #to test
# ptDef2PtTibList<-function(newPtDef){
#   ptTibList<-sapply( names(newPtDef$tib), function(nm){
#     pts<-as.integer(unlist(newPtDef$pts[[nm]]))
#     tags<-newPtDef$df[[nm]]$tag
#     tibble(pts=list(tags2listmatrices(pts,tags)))
#   }, simplify=FALSE)
#   ptTibList
# }
# 
# #to test
# ptDefs2DfTibList<-function(newPtDef){
#   dfTibList<-sapply(names(newPtDef$df),function(nm){
#       df<-newPtDef[[nm]]$df
#       df[-which(names(df=='tag'))]
#   },simplify=FALSE)
#   newPtDef
# }

# newPtDef<-list(
#   tib=list(
#     x=tribble(
#     ~fill, ~pts,
#     'red', matrix((1:6+.01),2)
#     ),
#     y=tribble(
#       ~stroke, ~pts,
#       'blue', matrix(1:4,2)
#     )
#   ),
#   pts=list(
#     x=matrix(1:6,2),
#     y=matrix(1:4,2)
#   ),
#   df=list(
#     x=tribble(
#       ~fill, ~tag,
#       'red',   1,
#       'green', 2
#     ),
#     y=tribble(
#         ~stroke, ~tag,
#         'blue', 1
#     )
#   )
#   
# )
# 
# 
# ptDefs_pts2tib(newPtDef)->temp
# 
# 
# tibb<-newPtDef$tib
# 
# tibb2<-pts2Integers(tibb)