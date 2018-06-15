#helper fn
stop.unless<-function(expr, mssg){
  if(!(expr)){
    stop(mssg, call.=FALSE)
    
  }
}


# txt and ancestors can be used for debugging
# 
# txt<-'#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src getNode script
# WH<-c(600,620)
# 
# #points defined by mouse clicks, edit with care!
# ptDefs<-list(
#    x=c(c( 123.5,392 ),c( 110.5,180 ),c( 329.5,157 ),c( 357.5,329 ))
# )
# 
# svgR(wh=c(100,200), 
#      
#      polygon(points=ptDefs$x, fill="blue",opacity=.5),
#      rect( class="draggable", opacity=.5,
#            xy=c(0,30), wh=c(80,80), fill="blue", 
#            transform="matrix(1 0 0 1 193 211)"
#      ),
#      circle( class="draggable", 
#              cxy=c(100,230), r=50, fill="red", opacity=.5,
#              transform=matrix(c(1, 0, 0, 1, 303, -55),2,)
#      )
# )'
# 
# 
# 
# #used for testing
# ancestors<-function(id, df, count=4){ 
#   rid<-id
#   for(i in 0:count){
#     cid<-rid[length(rid)]
#     s<-subset(df, id==cid)
#     if(nrow(s)>0){
#       rid<-c(rid,s$parent)
#     }
#   }
#   rid
# }



# used by server.R:output$svghtml
# detects transforms with draggable and 
# returns a modified copy of txt with
# tid for the Draggable
usingDraggable<-function(txt, transformType){
  if(is.null(transformType)){
    return(txt)
  }
  if(transformType=='Scale'){
    onMouseDownTxt = "', onmousedown='ptRPlotter_ptR_SVG_TRANSFORM_SCALE.selectElement(evt)'"
  } else if(transformType=='Rotate'){
    onMouseDownTxt = "', onmousedown='ptRPlotter_ptR_SVG_TRANSFORM_ROTATE.selectElement(evt)'"
  }else{
    onMouseDownTxt = "', onmousedown='ptRPlotter_ptR_SVG_TRANSFORM_TRANSLATE.selectElement(evt)'"
  }
  
  ep<-parse(text=txt)
  df<-getParseData(ep)
  
  drag<-subset(df, text %in% c('"draggable"', "'draggable'"))
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


# the tid (transform id) 
# ASSUMES the transform is on a single line (not a good idea)
# row is the line, start, end are the character positions
# This is grouped into a single string.
#
# tr2src finds the location in src and replaces everthing
# between the 2 boundries with trDefDelta
# tr2src<-function( src, tid, trDefDelta ){
#   strsplit(tid,'-')[[1]]->coords
#   as.numeric(coords[2])->row
#   as.numeric(coords[3])->start
#   as.numeric(coords[4])->end
#   lines<-strsplit(src,"\n")[[1]]
#   line<-lines[[row]]
#   pre<-substr(line,1,start-1)
#   post<-substr(line, end+1, nchar(line))
#   line<-paste0(pre, trDefDelta, post)
#   lines[row]<-line
#   paste(lines, collapse="\n")
# }

tid2replacementCoord<-function(tid, trDefDelta){
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

# getDefPos2<-function(lines, df, defTag){  
#   df.def<-subset(df, text==defTag)
#   parent(parent(df.def))[1,]->info
#   cnt<-cumsum(1+nchar(lines))
#   c(cnt[max(1,info$line1-1)]+info$col1, cnt[max(1,info$line2-1)]+info$col2)
# }

# txt2def<-function(txt, df, defTag){
#   lines<-strsplit(txt,"\n")[[1]]
#   pos<-getDefPos2(lines, df, defTag)
#   str<-substr(txt, pos[1], pos[2])
#   str
# }

#used by extractWH
rng2txt<-function(lines, ssdf){ 
  l2<-lines[[ssdf$line2]]
  lines[[ssdf$line2]]<-substr(l2, 1, ssdf$col2)
  l1<-lines[[ssdf$line1]]
  lines[[ssdf$line1]]<-substr(l1, ssdf$col1, nchar(l1))
  lines<-lines[ssdf$line1:ssdf$line2]
  paste0(lines, collapse="\n")  
}

# intended to be used to find WH for graphPaper
# but currently not used
extractWH<-function(src){
  ep<-parse(text=src)
  df<-getParseData(ep)
  # 1 find svgR
  subset(df, text=='svgR' & token=='SYMBOL_FUNCTION_CALL')->svgR.df
  subset(df, id==svgR.df$parent)$parent->gp.svgR
  #2 find wh whose parent is the svgR
  subset(df, text=='wh' & parent==gp.svgR)->WH.df
  #3 target id <-wh id +2
  #targ.id<-WH.df$id
  target.df<-subset(df,id==WH.df$id+2)
  #4 if target is SYMBOL_fUNCTION_call
  if(target$token=='SYMBOL_FUNCTION_CALL'){
    gip.id<-subset(df, id==target$parent)$parent
    target<-subset(df, id=gip.id)
  }
  txt<-rng2txt(target)
  lines<-strsplit(src,"\n")[[1]]
  rng2txt(lines, target)  
}

#used in showPts::serverSVGHTML.R
subSVGX2<-function(txt, insert.beg, insert.end){
  
  # cat('insert.beg=',format(insert.beg),"\n")
  # cat('insert.end=',format(insert.end),"\n")
  ep<-parse(text=txt, keep.source=TRUE)
  
  df<-getParseData(ep)
  svgR.df<-df[df$text=="svgR" & df$token=='SYMBOL_FUNCTION_CALL',] #svgr
  if( length(svgR.df)==0 || nrow(svgR.df)==0){
    return(NULL)
  }
  stop.unless(length(svgR.df)>0 && nrow(svgR.df)==1, "Trouble finding the svgR")
  svgR2.df<-df[df$id==svgR.df$parent,] #
  stop.unless(nrow(svgR2.df)==1, "Trouble finding the svgR")
  gpid<-svgR2.df$parent
  stop.unless(gpid!=0, "Trouble finding the svgR call")
  
  df2<-subset(df,parent==gpid)
  n<-nrow(df2)
  stop.unless(n>2, "Ill formed svgR call")
  stop.unless(df2$token[2]=="'('" , "failed to find left paren in call to svgR")  
  stop.unless(df2$token[n]=="')'" , "failed to find right paren in call to svgR")  
  l1<-df2$line1[2]; c1<-df2$col1[2] #pos of left paren
  l2<-df2$line1[n]; c2<-df2$col1[n] #pos of right paren
  
  lines<-strsplit(txt,"\n")[[1]]
  
  lines.pre<-c(
    if(l1>1) lines[1:(l1-1)] else NULL, 
    substr(lines[l1],1,c1)
  )
  lines.post<-c(
    substr(lines[l2],c2,nchar(lines[l2])), 
    if(l2<length(lines)) lines[(l2+1):length(lines)] else  NULL
  )
  if(l1==l2){
    lines.mid<- substr(lines[l1],(c1+1),c(c2-1))
  } else {
    lines.mid<-c(
      substr(lines[l1],(c1+1),nchar( lines[l1] )), 
      if(l2>l1+1) lines[(l1+1):(l2-1)] else NULL,
      substr(lines[l2],1, c2-1)
    )
  }
  lines.out<-na.omit(c(lines.pre, insert.beg, lines.mid,  insert.end, lines.post))
  txt.out<-paste(lines.out, collapse="\n")
  txt.out
}

