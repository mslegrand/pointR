
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
             transform="matrix(1 0 0 1 344 -45)"
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
             transform="matrix(1 0 0 1 303 -55)"
     )
)'

# parent<-function(sdf){
#   if(is.numeric(sdf)){
#     sdf<-subset(df, df$id==sdf)
#   }
#   subset(df, df$id %in% sdf$parent) 
# }

siblings<-function(sdf){
  if(is.numeric(sdf)){
    sdf<-subset(df, df$id==sdf)
  }
  par<-parent(sdf)
  subset(df, df$parent %in% par) 
}

insertAt<-function(lines, replacements){
  N<-length(replacements)
  if(N>0){
    #cat("N=",N,"\n")
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
  
  # get dragglable transforms
  #fn.tr<-function(){
    #browser()
    drag<-subset(df, df$text %in% c('"draggable"', "'draggable'"))
    pDrag<-subset(df, df$id %in% drag$parent) 
    gpDrag<-pDrag$parent
    tr<-subset(df, df$text=="transform" & df$terminal==TRUE & df$parent %in% gpDrag)
  #}
  
  #fn.tr()->tr
  #cat("   nrow(tr)=",nrow(tr),"\n")
  #get sib nodes (values expressions) for pos of replacements
  sibNodes<-subset(df, df$id %in% (tr$id+2))
  
  #save sib node table, and original source
  
  #form tid from sib node values
  lapply(split(sibNodes, 1:nrow(sibNodes)), function(x)paste("tid",x$line1,x$col1,x$col2,sep="-"))->tr.id
  names(tr.id)<-tr$id
  
  #get end pos of parent Nodes of tr ( or sib nodes) for insertion of tid and mousedown
  # form list in increasing order of names of sib nodes and endPos of parent Nodes
  replacements<-lapply(tr$id, function(i){ 
    #x<-parent(i) 
    parent.id<-subset(df, id==i)$parent
    x<-subset(df, id==parent.id)
    list(id=i, 
         row=as.numeric(x$line2), 
         start=as.numeric(x$col2-1), 
         end=as.numeric(x$col2),
         replace=paste0(", tid='",tr.id[[as.character(i)]], "', onmousedown='selectElement(evt)'")) 
  })
  
  #cat("length(replacements)=",length(replacements),"\n")
  # sub into source to produce sourceX 
  lines<-strsplit(txt,"\n")[[1]]
  #cat("length(replacements)=",length(replacements),"\n")
 
  N<-length(replacements)
  if(N>0){
    #cat("N=",N,"\n")
    for(i in N:1){
      repl<-replacements[[i]]
      row<-repl$row
      line<-lines[ row ]
      start<-repl$start
      end<-repl$end
      pre<-substr(line, 1, start)
      post<-substr(line, end, nchar(line))
      line<-paste0(pre, repl$replace, post)
      lines[row]<-line
    }
  }
  src<-paste(lines, collapse="\n")
  #src<-insertAt(lines, replacements)
  #cat("src:\n:",src,"\n")
  src
}



tr2src<-function( src, tid, trDefDelta ){
  strsplit(tid,'-')[[1]]->coords
  as.numeric(coords[2])->row
  as.numeric(coords[3])->start
  as.numeric(coords[4])->end
  lines<-strsplit(src,"\n")[[1]]
  replacement<-list(
      list(
        id=13, 
        row=as.numeric(as.numeric(coords[2])), 
        start=as.numeric(as.numeric(coords[3])), 
        end=as.numeric(as.numeric(coords[4])),
        replace=paste0("matrix(",paste0(trDefDelta,collapse=" "),")")
      )
  )
  src2<-insertAt(lines, replacement)
  #cat(src2)
  src2
}


getDefPos2<-function(lines, df, defTag){  
  df.def<-subset(df, df$text==defTag)
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


# formatVal<-function(defTag, defVal ){
#   switch(defTag)(
#     ptDefs= if(is.null(pts)){
#               "c()"
#             } else{
#               tmp<-unlist(pts)
#               tmp<-matrix(tmp,2,)
#               tmp<-apply(tmp, 2, function(x)paste(x,collapse=","))
#               tmp<-paste("c(",tmp,")")
#               tmp<-paste(tmp, collapse=",")
#               paste0("c(", tmp, ")")
#             },
#     trDefs=paste0('"', defVals ,'"')
#   ) 
# }


def2txt<-function(defVal, txt, df, defTag="ptDefs"){
  lines<-strsplit(txt,"\n")[[1]]
  rpl<-paste0(defTag,"=", formatValue(defTag, defVal))
  pos<-getDefPos2(lines, df, defTag)
  substr(txt,pos[1],pos[2])<-rpl
} 


#testcode

#  src<-txt
# # ep<-parse(text=src)
# # df<-getParseData(ep)
#  src<-usingDraggable(src)

