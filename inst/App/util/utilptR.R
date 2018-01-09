
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
   matrix(c(100,200,300,200),2), 'red',
   matrix(c(100,300,300,300),2), 'red',
   matrix(0,2,0), 'blue'
  ),
  y=tribble(
   ~y,             ~stroke,
   matrix(0,2,0), 'black'
  ),
  z=matrix(c(100,100,100,200,300,200),2)
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
# pts2Source<-function(txt,ptRList){
#   if(length(ptRList)>0){
#     replacement<-formatPtDefs(defTag=defTag, ptRList=ptRList)
#     txt<-replaceDef(txt, replacement, defTag=defTag) 
#   } else {
#     txt
#   }
# }

# df2Source<-function(txt, dfList){
#   if(length(dfList)>0){
#     replacement<-formatDFDefs(dfList)
#   } else {
#     replacement<-""
#   }
#   txt<-replaceDef(txt, replacement, defTag="tagR") 
# }


# 
# as.text<-function(q){
#   paste(deparse(q), collapse="\n")
# }

# getScript<-function(file){
#   paste0(readLines(file),collapse="\n")
# }

# readFile<-function(fileName){
#   paste0(readLines(fileName),collapse="\n")
# }

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


# replaceDef<-function(txt, replacement, defTag){
#   pos<-getDefPos(txt, defTag)
#   if(is.null(pos)){
#     return(txt)
#   }
#   if(length(replacement)==0){
#     replacement=" "
#   }
#   s<-paste0(
#     substr(txt, 1, pos[1]-1),
#     replacement,
#     substr(txt,pos[2]+1, nchar(txt) ),
#     sep=""
#   )
#   return(s)
# }

# replaceTxt<-function(txt, replacements, positions){
#   stopifnot(length(replacments)+1==ncol(positions))
#   txtKeep<-textOutsidePos(txt, positions)
#   replacements<-c(replacements,"")
#   newtxt<-rbind(txtKeep,replacements)
#   newtxt<-paste0(nextxt, collapse="")
# }

# replaceDefs<-function(txt, replacements, defTags){
#   positions<-sapply(defTags, function(defTag)
#     getDefPos(txt, defTag)
#   )
#   replaceTxt(txt, replacements, defTags)
# }
  
getDef<-function(txt, defTag ){
  pos<-getDefPos(txt, defTag)
  if(is.null(pos)){
    return(NULL)
  }
  return(substr(txt, pos[1], pos[2]))
}


# matrices2tags<-function(mats){
#   tmp<-cumsum(sapply(mats,ncol))+1
#   c(1,tmp[-length(tmp)])
# }


ex.getPtDefs<-function(src, useTribbleFormat, ptTag="ptR"  ){
  if(is.null(useTribbleFormat)){
    useTribbleFormat=FALSE
  }
  ptDefs<-list(tib=NULL, useTribbleFormat=useTribbleFormat)
  if(length(ptDefs)==0){
    return(list( tib=list(), useTribbleFormat=useTribbleFormat))
  }
  if( any(grepl(ptTag,src) ) ){
    try({
      ptDefTxt1<-getDef(src, defTag=ptTag)
      if( is.null(ptDefTxt1)){
        #cat('\n===========ptDefTxt1 is NULL\n')
        #stop("failed to fint ptR")
        
        ptDefs$tib<-list()
      } else {
       # cat('\n===========ptDefTxt1 is NOT NULL\n')
        eval(parse(text=ptDefTxt1)) #stupid eval to obtain the points
        
        #!!!KLUDGE first kludge (undo later)
        
        
        ptDefs$tib<-get(ptTag) #at this stage we have ptR as a list of tibbles, each tibble containings points with name same as tib
        ptDefs$mats<-sapply(ptDefs$tib,is.matrix) #record what is a matrix
        nms<-names(ptDefs$tib)
        for(n in nms){ #convert matrics to tibbles
          v<-ptDefs$tib[[n]]
          if(is.matrix(v)){
            tt<-tibble(key=list(v))
            names(tt)=n
            ptDefs$tib[[n]]<-tt
          }
        }
        ptDefs$useTribbleFormat=useTribbleFormat
      }
    })
  }
  ptDefs$useTribbleFormat=useTribbleFormat
  return(ptDefs)
}


# formatTrs<-function(tr){ #not used
#   paste0('"',tr,'"')
# }


ptDef2ReplacementList<-function(name, newPtDef, txt){
  replacementList<-list()
  # get the text for the point replacement  
  pt.repl<-fmtTibbleList(newPtDef$tib, newPtDef$mats, as.Tribble=newPtDef$useTribbleFormat)
  
  p.df<-getParseDataFrame(txt)
  ptR.df<-extractTagDF(p.df, tag='ptR')
  pt.Pos<-list(
    startRow= ptR.df$line1 -1,
    startColumn=ptR.df$col1 -1 ,
    endRow= ptR.df$line2 -1,
    endColumn=ptR.df$col2 
  )
  replacementList<-c(replacementList, list(list(rng=pt.Pos, txt= pt.repl)))
  replacementList
}


# panel2svgid<-function(panelName){
#   prefix2<-'ptR_SVG_'
#   paste0(prefix2, "_", toupper(paneName))
# }

# panel2onmousedown<-function(panelName, transformType=NULL){
#   prefix1<-'ptRPlotter_'
#   mid=''
#   if(!is.null(transformType)){
#     mid=paste('_',toupper(transformType))
#   }
#   paste0(prefix1,panel2svgid,mid,".selectElement(evt)")
# }

# panel2var<-function(panelName, transformType=NULL){
#   prefix1<-'ptRPlotter_'
#   mid=''
#   if(!is.null(transformType)){
#     mid=paste('_',toupper(transformType))
#   }
#   paste0(prefix1,panel2svgid,mid)
# }

#' panel2script<-function(panelName, transformType=NULL){
#'   #'var ptRPlotter_ptR_SVG_TRANSFORM_TRANSLATE = new PtRPanelTranslate("ptR_SVG_TRANSFORM");'
#'   paste0( 'var ', panel2var(panelName, transformType), ' = new ', jsConstr, "(\"", panel2svgid, "\");" )
#' }

# tags2listmatrices<-function( matrixPts, tags){
#   # assume tags start with 1
#   
#   if(length(matrixPts)==0){
#     matrixPts=matrix(0,2,0)
#   } else {
#     matrixPts<-matrix(matrixPts,2)
#   }
#   if(length(tags)>0){
#     tags<-c(tags, (ncol(matrixPts)+1))
#     indx<-lapply(1:(length(tags)-1), function(x){tags[x]:(tags[x+1]-1) })
#     tmp<-lapply(indx, function(i) matrixPts[,i])
#     tmp<-lapply(tmp, function(x)as.matrix(x,2))
#   } else {
#     tmp<-list(matrixPts)
#   }
#   tmp
# }

# #!!! TODO this is temp: refactor or remove
# # Replaces pts by indexed set
# olde2newFmtPtDef2ListOfLists<-function(newPtDef){
#   pts<-newPtDef$pts # should be a list of list of matrices!!!
#   
#   newPtDef$pts<-sapply(names(pts), function(nm){
# 
#     pts<-as.integer(unlist(newPtDef$pts[[nm]]))
#     tags<-newPtDef$df[[nm]]$tag
#     ptsL<-tags2listmatrices(pts,tags)
#   }, simplify=FALSE)
#   newPtDef
# }


#!!!REDO KLUDGE to convert output to ints
pts2Integers<-function(newtib){
  for(nm in names(newtib)){
    for( j in ncol( newtib[[nm]] )){
      if( is.matrix(newtib[[nm]][[1,j]]) &&  dim(newtib[[nm]][[1,j]])[1]==2){
        for( i in nrow(  newtib[[nm]] )){
          newtib[[nm]][[i,j]]<-matrix( as.integer(newtib[[nm]][[i,j]] ), 2)
        }
      }
    }
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

#this could be used when opening a file
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