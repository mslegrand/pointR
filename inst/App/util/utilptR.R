
#returns col indices of tib corresponding to points
# used by serverAssetSellection.R and serverAssetSellectionDB.R
extractPointColumnIndices<-function(tib){
  if(length(names(tib))==0){
    vals<-F
  } else{
    vals<-sapply(names(tib),
                 function(n){
                   isPoints(tib[[n]])
                 }
    )
  }
  which(vals)
}

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
        ptDefs$tib<-list()
      } else {
        eval(parse(text=ptDefTxt1)) #stupid eval to obtain the points
        
        #!!!KLUDGE first kludge (undo later)
        ptDefs$tib<-get(ptTag) #at this stage we have ptR as a list of tibbles, each tibble containings points with name same as tib
        ptDefs$mats<-sapply(ptDefs$tib,is.matrix) #record what is a matrix (vs a tib)
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
  if(!is.null(ptR.df)){
     pt.Pos<-list(
      startRow= ptR.df$line1 -1,
      startColumn=ptR.df$col1 -1 ,
      endRow= ptR.df$line2 -1,
      endColumn=ptR.df$col2 
    )
  } else {
    svgR.df<-extractSVGRDF(p.df)
    if(nrow(svgR.df)>0){
      pt.Pos<-list(
        startRow= svgR.df$line1 -1,
        startColumn=0 ,
        endRow= svgR.df$line1 -1,
        endColumn= 0
      )
      pt.repl<-paste0(pt.repl,' \n \n')
    }
    else {
      pt.Pos<-list(
        startRow= 0,
        startColumn=0 ,
        endRow= 0,
        endColumn= 0
      )
      pt.repl<-paste0(pt.repl,' \n \n')
    }
  }
  replacementList<-c(replacementList, list(list(rng=pt.Pos, txt= pt.repl)))
  replacementList
}


trimPtDigits<-function(tibs){ #!!! trim to 3 significant  digits
  for(nm in names(tibs)){
    for( j in ncol( tibs[[nm]] )){
      if( isPoints(tibs[[nm]][[j]])){
        for( i in nrow(  tibs[[nm]] )){
          tibs[[nm]][[j]][[i]]<-matrix( signif(tibs[[nm]][[j]][[i]],3 ), 2)
        }
      }
    }
  }
  tibs
}
