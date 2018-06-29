

imageBlockIndices<-function(temp){
  unlist(str_split(temp, '\n'))->lines
  pose<-grep('^```', lines)
  posb<-1+grep('^SVGR Image:', lines)
  indx<-which(pose %in% posb)
  posx<-pose[indx]
  posy<-pose[indx+1]
  rtv<-rbind(posx,posy)
  rtv
}

dripplets2Rmd<-function( drps ){
  drps<-str_replace_all(drps, "svgR\\(", 'temp<-svgR(')
  m<-imageBlockIndices(drps)
  print(m)
  unlist(str_split(drps, '\n'))->drps
  indx<-m[2,]
  print(indx)
  drps[indx]<-paste(
    "temp$root$setAttr('width',480)",
    "temp$root$setAttr('height',320)",
    "temp$root$setAttr('viewBox','0 0  48 32')",
    "temp$root$prependChildren(
      svgR:::use(filter=svgR:::filter( filterUnits='userSpaceOnUse', svgR:::feFlood(flood.color='black') ) )
    )",
    "temp",
    "```",
    sep="\n"
  )
  drps<-paste(drps,collapse="\n")
  drps<-str_replace(drps, "\noutput: dnd_snippet", "\noutput: html_document")
  drps<-str_replace_all(drps, "Hint:\\s*\n```\n(.+)\n```", "### \\1")
  drps<-str_replace_all(drps, "\nSnippet\\s+Insert:\\s*","\n")
  drps<-str_replace_all(drps, "\nSVGR Image:\\s*\n```\n", '\n\n```\\{r, echo=FALSE, results=\'asis\'\\}\n')
  drps
}

extractVal<-function(x,pattern){
  cat('extractVal\n')
  rtv<-str_match_all(x,pattern)
  print(rtv)
  rtv<-rtv[[1]]
  if(length(rtv)>0){
    rtv<-rtv[1,2]
  } else {
    rtv<-NULL
  }
  rtv
}

dripplets2List<-function(drps){
  drps<-str_split(drps,pattern = '\\*{3,}')
  drps<-drps[[1]]
  
  i<-1
  drps<-lapply(drps, function(dr){
    rtv<-list()
    cat(i, 'hint\n')
    rtv$hint<-    extractVal(dr, "(?:\nHint: \n```\n)(.+)(?:\n```\n)")
    cat(i, 'snippet\n')
    rtv$snippet<- extractVal(dr, "(?:\nSnippet\\s+Insert:\\s*\n```\\s*\n)(.+)(?:\n```\\s*\n)")
    rtv
  })
}


extractDripplet<-function(dr, tmpdir=tempdir() ){
  temp<-str_trim(unlist(str_split(dr,'\n```')))
  if(length(temp)>=6){
    temp<-temp[nchar(temp)>0]
  }
  if(length(temp)!=6){
    return(NULL)
  }
  
  tt<-setNames(temp[c(2,4,6)], str_remove(temp[c(1,3,5)], ":\\s*"))
  m<-match(names(tt),c("Hint", "Snippet Insert", "SVGR Image"),0)
  if(all(m>0)){
    rtv<-tt
    rtv<-tryCatch({
       svg<-as.character(eval(parse(text=tt['SVGR Image'])))
       rtv["SVGR Image"]<-svg
       names(rtv)<-c('hint','snip','logo')[m]
       rtv
    }, error=function(e){NULL} )
  } else {
    rtv<-NULL
  }
  rtv
}

dripplets2List2<-function(drps){
  drps<-unlist(str_split(drps,pattern = '\\*{3,}'))
  tmpdir=tempdir()
  tmpdir='drippets'
  drps<-lapply(drps, function(dr){
    rtv<-extractDripplet(dr, tmpdir)
    rtv
  })
  drps
}

row2DrippletBlock<-function(lineNo){
  unlist(str_split(temp, '\n'))->lines
  pose<-grep('^```', lines)
  posb<-1+grep('^SVGR Image:', lines)
  indx<-which(pose %in% posb)
  posx<-pose[indx]
  posy<-pose[indx+1]
  
  indxx<-which((posx <=row) & (row<=posy))
  if(length(indxx)>0){
    rtv<-paste(lines[(posx[indxx]+1):(posy[indxx]-1)], collapse="\n")
  } else {
    rtv<-NULL
  }
  rtv
}

row2DrippletBlockIndices<-function(lineNo){
  unlist(str_split(temp, '\n'))->lines
  pose<-grep('^```', lines)
  posb<-1+grep('^SVGR Image:', lines)
  indx<-which(pose %in% posb)
  posx<-pose[indx]
  posy<-pose[indx+1]
  
  indxx<-which((posx <=row) & (row<=posy))
  if(length(indxx)>0){
    rtv<-c((posx[indxx]+1),(posy[indxx]-1))
  } else {
    rtv<-NULL
  }
  rtv
}




