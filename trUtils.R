
# txt<-'
#   WH<-c(600,620)
# 
# #points defined by mouse clicks, edit with care!
# ptDefs<-list(
#  x=c(c( 123.5,392 ),c( 198.5,100 ),c( 288.5,210 ))
# )
# 
# trDefs<-list(
#   rect="matrix(1 0 0 1 304 264)",
#   circle="matrix(1 0 0 1 344 -45)"
# )
# 
# svgR(wh=WH, 
# 
#   polygon(points=ptDefs$x, fill="blue",opacity=.5),
#   rect( class="draggable", opacity=.5,
#     xy=c(0,30), wh=c(80,80), fill="blue", 
#     transform="matrix(1 0 0 1 304 264)"
#   ),
#   circle( class="draggable", 
#    cxy=c(100,230), r=50, fill="red", opacity=.5,
#    transform="matrix(1 0 0 1 344 -45)"
#   )
# )
# '
# 
# ep<-parse(text=txt)
# df<-getParseData(ep)



txt<-'#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src getNode script
WH<-c(600,620)

#points defined by mouse clicks, edit with care!
ptDefs<-list(
  x=c(c( 123.5,392 ),c( 198.5,100 ),c( 288.5,210 ),c( 357.5,329 ))
)


svgR(wh=WH, 
     
     polygon(points=ptDefs$x, fill="blue",opacity=.5),
     rect( class="draggable", opacity=.5,
           xy=c(0,30), wh=c(80,80), fill="blue", 
           transform="matrix(1 0 0 1 304 264)"
     ),
     circle( class="draggable", 
             cxy=c(100,230), r=50, fill="red", opacity=.5,
             transform=matrix(c(1, 0, 0, 1, 34, -45)2,)
     )
)
'
txt<-'#svgR elements: font-face glyph missing-glyph hkern vkern font font-face-name font-face-format font-face-uri animate animateColor animateMotion animateTransform set mpath feFuncA feFuncB feFuncG feFuncR feDistantLight feTurbulence feConvolveMatrix feDiffuseLighting feOffset filter feBlend feColorMatrix feComponentTransfer feComposite feDisplacementMap feFlood feGaussianBlur feImage feMerge feMorphology feSpecularLighting feTile feSpotLight fePointLight svg a altGlyph circle clipPath cursor defs ellipse foreignObject g image line linearGradient marker mask path pattern polygon polyline radialGradient rect script switch symbol text textPath tref tspan use view altGlyphDef altGlyphItem color-profile desc feMergeNode font-face-src glyphRef metadata stop style title font.face missing.glyph font.face.name font.face.format font.face.uri color.profile font.face.src getNode script
WH<-c(600,620)

#points defined by mouse clicks, edit with care!
ptDefs<-list(
   x=c(c( 123.5,392 ),c( 110.5,180 ),c( 329.5,157 ),c( 357.5,329 ))
)

svgR(wh=WH, 
     
     polygon(points=ptDefs$x, fill="blue",opacity=.5),
     rect( class="draggable", opacity=.5,
           xy=c(0,30), wh=c(80,80), fill="blue", 
           transform="matrix(1 0 0 1 193 211)"
     ),
     circle( class="draggable", 
             cxy=c(100,230), r=50, fill="red", opacity=.5,
             transform=matrix(c(1, 0, 0, 1, 303, -55),2,)
     )
)'


siblings<-function(sdf){
  if(is.numeric(sdf)){
    sdf<-subset(df, id==sdf)
  }
  par<-parent(sdf)
  subset(df, parent %in% par) 
}

ancestors<-function(id, df, count=4){ 
  rid<-id
  for(i in 0:count){
    cid<-rid[length(rid)]
    s<-subset(df, id==cid)
    if(nrow(s)>0){
      rid<-c(rid,s$parent)
    }
  }
  rid
}


rng2txt<-function(lines, ssdf){ 
  l2<-lines[[ssdf$line2]]
  lines[[ssdf$line2]]<-substr(l2, 1, ssdf$col2)
  l1<-lines[[ssdf$line1]]
  lines[[ssdf$line1]]<-substr(l1, ssdf$col1, nchar(l1))
  lines<-lines[ssdf$line1:ssdf$line2]
  paste0(lines, collapse="\n")  
}

insertAt<-function(lines, replacements){
  N<-length(replacements)
  if(N>0){
    
    for(i in N:1){
      repl<-replacements[[i]]
      row<-repl$row
      line<-lines[ row ]
      start<-repl$start
      end<-repl$end
      pre<-substr(line, 1, start)
      post<-substr(line, end, nchar(line))
      line<-paste0(pre, repl$replace, post)
      lines[[row]]<-line
    }
  }
  paste(lines, collapse="\n")
}
  
usingDraggable<-function(txt){

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
  #lapply(split(sibNodes, 1:nrow(sibNodes)), function(x)paste("tid",x$line1,x$col1,x$col2,sep="-"))->tr.id
  names(tr.id)<-tr$id
  
  #get end pos of parent Nodes of tr ( or sib nodes) for insertion of tid and mousedown
  # form list in increasing order of names of sib nodes and endPos of parent Nodes
  insertions<-lapply(tr$id, function(i){ 
    #x<-parent(i) 
    parent.id<-subset(df, id==i)$parent
    x<-subset(df, id==parent.id)
    list(id=i, 
         row=as.numeric(x$line2), 
         pos=as.numeric(x$col2), 
         txt=paste0(", tid='",tr.id[[as.character(i)]], "', onmousedown='selectElement(evt)'")) 
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



tr2src<-function( src, tid, trDefDelta ){
  strsplit(tid,'-')[[1]]->coords
  as.numeric(coords[2])->row
  as.numeric(coords[3])->start
  as.numeric(coords[4])->end
  lines<-strsplit(src,"\n")[[1]]
  line<-lines[[row]]
  pre<-substr(line,1,start-1)
  post<-substr(line, end+1, nchar(line))
  line<-paste0(pre, trDefDelta, post)
  lines[row]<-line
  paste(lines, collapse="\n")
}


getDefPos2<-function(lines, df, defTag){  
  df.def<-subset(df, text==defTag)
  parent(parent(df.def))[1,]->info
  #print(info)
  #assert info$parent==0 as check
  cnt<-cumsum(1+nchar(lines))
  c(cnt[max(1,info$line1-1)]+info$col1, cnt[max(1,info$line2-1)]+info$col2)
}

txt2def<-function(txt, df, defTag="ptDefs"){
  lines<-strsplit(txt,"\n")[[1]]
  pos<-getDefPos2(lines, df, defTag)
  str<-substr(txt, pos[1], pos[2])
  #eval(parse(text=txt))
  str
}




def2txt<-function(defVal, txt, df, defTag="ptDefs"){
  lines<-strsplit(txt,"\n")[[1]]
  rpl<-paste0(defTag,"=", formatValue(defTag, defVal))
  pos<-getDefPos2(lines, df, defTag)
  substr(txt,pos[1],pos[2])<-rpl
} 


#testcode
# 
#   src<-txt
# ep<-parse(text=src)
# df<-getParseData(ep)
#  src<-usingDraggable(src)

