

# sole caller:  getDef (below)
getDefPos<-function(txt, defTag){
  pos<-NULL
  if(!is.null(txt)){
    p.df<-getParseDataFrame(txt)
    cumCharLines<-getcumCharLines(txt)
    tag.df<-extractTagDF(p.df, tag=defTag)
    if( !is.null(tag.df) ){
      pos<-extractPositions(cumCharLines, tag.df)
    } else {
      pos<-NULL
    }
  }
  pos
}

# sole caller:  ex.getPtDefs (below)
getDef<-function(txt, defTag ){
  pos<-getDefPos(txt, defTag)
  if(is.null(pos)){
    return(NULL)
  }
  return(substr(txt, pos[1], pos[2]))
}


# sole caller: server.R:: getPtDefs
# TODO: move useTribbleFormat out
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


# sole caller: updateAceExtDef
ptDef2ReplacementList<-function(name, newPtDef, txt){ # name arg not used???
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




# sole caller: updateAceExtDef 
trimPtDigits<-function(newtib){ #!!! trim to 3 significant  digits
  for(nm in names(newtib)){
    for( j in ncol( newtib[[nm]] )){
      if( is.matrix(newtib[[nm]][[1,j]]) &&  dim(newtib[[nm]][[1,j]])[1]==2){
        for( i in nrow(  newtib[[nm]] )){
          newtib[[nm]][[i,j]]<-matrix( signif(newtib[[nm]][[i,j]],3 ), 2)
        }
      }
    }
  }
  newtib
}

# 
# # NOT Used!!  
# # this could be used when opening a file
# ## choices 
# choices2ColType<-function( choices, column){
#   ct<-NULL
#   lastVal<-last(choices)
#   cv=class(lastVal)
#   ct<-list(cv)
#   if(is.character(cv) && isColor(lastVal)){
#     ct<-list('colorPicker')
#   }
#   if(column %in% c('opacity' )){
#     ct<-list('slider','decimal',0,1)
#   }
#   ct
# }
# 
# # NOT Used!!
# # this would be used when adding an new column
# val2ColType<-function(val, column){
#   tmp<-type.convert(val)
#   cv=class(lastVal)
#   ct<-list(cv)
#   if(is.character(cv) && isColor(lastVal)){
#     ct<-list('colorPicker')
#   }
#   if(column %in% c('opacity' )){
#     ct<-list('slider','decimal',0,1)
#   }
#   ct
# }
# 
