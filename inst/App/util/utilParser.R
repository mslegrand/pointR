
colLine2charPos<-function(nCharLines, lineNum, colNum){ #not needed!!!
  colNum + sum(nCharLines[1:(lineNum-1)])
}

colLine2charPositions<-function(csnCharLines, lineNums, colNums){ 
  colNums + csnCharLines[lineNums]
}


#data2source
#source2data
#source2sourceExtension

#todo?? make reactive expr: parseDataFrame<-reactive({getParseDataFrame(user$code)})
getParseDataFrame<-function(txt){

  ep<-parse(text=txt, keep.source = T)
  p.df<-getParseData(ep)
  id<-p.df$id
  pid<-p.df$parent
  gid<-pid[match(pid,id)]
  p.df$gid<-gid
  ggid<-pid[match(gid,id)]
  p.df$ggid<-ggid
  p.df
}


getNCharLines<-function(txt){
  lines<-strsplit(txt,"\n")
  nCharLines<-sapply(lines, nchar)
  nn<-length(nCharLines)
  nCharLines<-nCharLines+1
  nCharLines[nn]<-nCharLines[nn]-1
  nCharLines
}

#todo?? make reactive expr: codeLineCnt<-reactive({getNCharLines(user$code)})
getcumCharLines<-function(txt){
  nCharLines<-getNCharLines(txt)
  csnCharLines<-cumsum(c(0,nCharLines))
}



extractTagDF<-function(p.df, tag="ptR"){
  
  df1<-subset(p.df, token=='SYMBOL' & text ==tag & ggid==0)
  
  if(nrow(df1)==0){ #use this as a check
    return(NULL)
  }
  stopifnot(nrow(df1)==1)
  # todo: some additional checks (such as <-)
  # check list
  lid<-3+df1$id
  list.df<-subset(p.df, id==lid)
  stopifnot( list.df$token=="SYMBOL_FUNCTION_CALL")
  stopifnot( list.df$text=="list")  
  #then compute and return
  tag.gid<-df1$gid
  g.df<-subset(p.df, id==tag.gid)
}

extractPositions<-function(csnCharLines, df){
  pos1<-colLine2charPositions(csnCharLines, df$line1, df$col1)
  pos2<-colLine2charPositions(csnCharLines, df$line2, df$col2)
  rbind(pos1=pos1, pos2=pos2)  
}

# returns all parse ids of attributes of given name and
# given value. 
getAttrParId<-function(p.df, name="class", value='"draggable"'){
  df1<-subset(p.df, token='SYMBOL_SUB', text=name)
  df2<-subset(p.df, token='EQ_SUB', text="=")
  df3<-subset(p.df, token='STR_CONST', text=value)
  id1<-df1$id; id2<-df2$id; id3<-df3$id
  id<-intersect(id3-1, id2)
  id1<-intersect(id-1, id1)
  p1<-tmp.df[id==id1,]$parent
  p2<-tmp.df[id==(id1+1)]$parent
  p3<-tmp.df[id==(id1+2)]$gid
  id1<-id1[(p1==p2)&(p2==p3)]
  id1$parent
}


childExpr<-function(p.df,sid, eid=rep(0,length(sid))){
  id<-p.df$id
  pid<-p.df$parent
  rid<-sid
  #browser()
  idx0<-match(rid, id)
  idx<-pid[idx0]!=eid
  count<-0
  while(any(idx)){
    rid[idx]<-pid[idx0][idx]
    idx0<-match(rid, id)
    idx<-pid[idx0]!=eid
    count<-count+1
    if(count==200){
      stop("failed to find")
    }
  }
  rid 
}

# returns dataframe corresponding to the values
# for attribute values for  given attribute name and
# given expression. 
extract.AttrValue.DF<-function(p.df, parentExprId=NULL, name='transform'){
  #extrach id for name
  df1<-subset(p.df, token=='SYMBOL_SUB' & text==name)
  df2<-subset(p.df, token=='EQ_SUB')
  id1<-df1$id; id2<-df2$id
  #restrict to parentExprId
  if(!is.null(parentExprId)){
    df3<-subset(p.df, parent %in% parentExprId )
    id3<-df3$id
    id1<-intersect(id1, id3)
    id2<-intersect(id2,id3)
  }
  
  #restrict to adjacency
  id1<-intersect(id1, id2-1)
  #now get positions of values form pos of name
  df1<-subset(p.df, id %in% id1)
  id1<-df1$id
  idp<-df1$parent
  idm<-id1+2
  ids<-childExpr(p.df, idm, idp)
  subset(p.df, id %in% ids)
}

textInsidePos<-function(txt, pos){
  if(length(pos)<2){
    return(c())
  }
  pos<-matrix(pos,2)
  apply(pos, 2, function(x){
    substr(txt, x[1], x[2])
  })
}

textOutsidePos<-function(txt, pos){
  if(length(pos)<2){
    return(txt)
  }
  pos<-matrix(pos,2)
  pos2<-rbind(
    c(0,pos[2,]+1),
    c(pos[1,]-1,nchar(txt))
  )
  apply(pos2, 2, function(x){
    substr(txt, x[1], x[2])
  })
}

extractSVGRDF<-function(p.df, tag="svgR"){
  df1<-subset(p.df, token=='SYMBOL_FUNCTION_CALL' & text ==tag & ggid==0)
  stopifnot(nrow(df1)==1)  
  subset(p.df, id==df1$gid)
}

