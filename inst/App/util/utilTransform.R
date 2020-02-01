
# Not used!!!
#helper fn 
stop.unless<-function(expr, mssg){
  if(!(expr)){
    stop(mssg, call.=FALSE)
    
  }
}



# sole caller: getCodeTransform(r)  in plotBarTransform
# 
# detects transforms with draggable and returns a modified copy of txt with tid for the Draggable
usingDraggable<-function(txt, transformType){
  log.fin(usingDraggable)
  log.val(transformType)
  

  if(is.null(transformType) || is.null(txt) || !grepl('adjustable',txt )){
    return(txt)
  }
  
  if(transformType=='Scale'){
    onMouseDownTxt = "', onmousedown='ptRPlotter_ptR_SVG_TRANSFORM_SCALE.selectElement(evt)'"
  } else if(transformType=='Rotate'){
    onMouseDownTxt = "', onmousedown='ptRPlotter_ptR_SVG_TRANSFORM_ROTATE.selectElement(evt)'"
  }else{
    onMouseDownTxt = "', onmousedown='ptRPlotter_ptR_SVG_TRANSFORM_TRANSLATE.selectElement(evt)'"
  }
  
  sf <- srcfile("txt")
  try(parse(text = txt, srcfile = sf))
  df<-getParseData(sf)
  
  drag<-subset(df, text %in% c('"adjustable"', "'adjustable'"))
  
  pDrag<-subset(df, id %in% drag$parent) 
  gpDrag<-pDrag$parent
  
  tr<-subset(df, text=="transform" & terminal==TRUE & parent %in% gpDrag)
  
  sibNodes<-subset(df, id %in% (tr$id+2))
  if(nrow(sibNodes)==0){
    return(txt)
  }
  
  lapply(split(sibNodes, 1:nrow(sibNodes)), function(x){
    if(x$text=='matrix'){ #get grandparent
      px<-subset(df, id ==x$parent )
      x<-subset(df, id ==px$parent )
    }
    x 
  })->tmp
  
  
  #save sib node table, and original source
  lapply(tmp, function(x)paste("tid",x$line1,x$col1,x$col2,sep="-"))->tr.id
  
  #form tid from sib node values
  names(tr.id)<-tr$id
  
  
  #get end pos of parent Nodes of tr ( or sib nodes) for insertion of tid and mousedown
  # form list in increasing order of names of sib nodes and endPos of parent Nodes
  insertions<-lapply(tr$id, function(i){ 
    parent.id<-subset(df, id==i)$parent
    x<-subset(df, id==parent.id)
    list(id=i, 
         row=as.numeric(x$line2), 
         pos=as.numeric(x$col2), 
         txt=paste0(", tid='",tr.id[[as.character(i)]], onMouseDownTxt )) 
  })
  
  
  # sub into source to produce sourceX 
  lines<-strsplit(txt,"\n")[[1]]
  
  N<-length(insertions)
  if(N>0){
    
    for(i in N:1){
      item<-insertions[[i]]
      row<-item$row
      line<-lines[ row ]
      pos<-item$pos
      pre<-substr(line, 1, pos-1) #upto but not including pos
      post<-substr(line,   pos, nchar(line)) #the rest
      line<-paste0(pre, item$txt, post)
      lines[row]<-line
    }
  }
  
  src<-paste(lines, collapse="\n")
  src
}

# sole caller: mouseCmdTransform
tid2replacementCoord<-function(tid){
  strsplit(tid,'-')[[1]]->coords
  as.numeric(coords[2])->row
  as.numeric(coords[3])->start
  as.numeric(coords[4])->end
  pos<- list(
    startRow= row -1,
    startColumn=start -1 ,
    endRow= row -1,
    endColumn=end
  )
  pos  
}

