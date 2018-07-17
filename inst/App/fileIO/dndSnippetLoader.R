

imageBlockIndices<-function(temp){
  unlist(str_split(temp, '\n'))->lines
  pose<-grep('^```', lines)
  posb<-1+grep('^SVGR', lines)
  indx<-which(pose %in% posb)
  posx<-pose[indx]
  posy<-pose[indx+1]
  rtv<-rbind(posx,posy)
  rtv
}

dripplets2Rmd<-function( drps ){
  drps<-str_replace_all(drps, "svgR\\(", 'temp<-svgR(')
  m<-imageBlockIndices(drps)
  # print(m)
  unlist(str_split(drps, '\n'))->drps
  indx<-m[2,]
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
  drps<-str_replace_all(drps, "POPUP\\s*\n```\n(.+)\n```", "### \\1")
  drps<-str_replace_all(drps, "\nSNIPPET\\s*","\n")
  drps<-str_replace_all(drps, "\nSVGR\\s*\n```\n", '\n\n```\\{r, echo=FALSE, results=\'asis\'\\}\n')
  drps
}

extractVal<-function(x,pattern){
  # cat('extractVal\n')
  rtv<-str_match_all(x,pattern)
  # print(rtv)
  rtv<-rtv[[1]]
  if(length(rtv)>0){
    rtv<-rtv[1,2]
  } else {
    rtv<-NULL
  }
  rtv
}

# NOT USED!!!
dripplets2List<-function(drps){
  drps<-str_split(drps,pattern = '\\*{3,}')
  drps<-drps[[1]]
  
  i<-1
  drps<-lapply(drps, function(dr){
    rtv<-list()
    # cat(i, 'hint\n')
    rtv$hint<-    extractVal(dr, "(?:\nPOPUP\n```\n)(.+)(?:\n```\n)")
    # cat(i, 'snippet\n')
    rtv$snippet<- extractVal(dr, "(?:\nSNIPPET\\s*\n```\\s*\n)(.+)(?:\n```\\s*\n)")
    rtv
  })
}

#' @param dr, a section of a drippet text file representing a single drippet
#' @return a named character vector, with each component representation a piece of the drippet
#' 
#' \enumerate{
#'  \item hint represents the popup
#'  \item snip represents the snippet
#'  \item logo is the result of processing the svgr
#' }
#extractDripplet<-function(dr, tmpdir=tempdir() ){
extractDripplet<-function(dr ){
    temp<-str_trim(unlist(str_split(dr,'\n```')))
  if(length(temp)>=6){
    temp<-temp[nchar(temp)>0]
  }
  if(length(temp)!=6){
    return(NULL)
  }
  
  tt<-setNames(temp[c(2,4,6)], str_remove(temp[c(1,3,5)], ":\\s*"))
  m<-match(names(tt),c("POPUP", "SNIPPET", "SVGR"),0)
  if(all(m>0)){
    rtv<-tt
    rtv<-tryCatch({
       svg<-as.character(eval(parse(text=tt['SVGR'])))
       rtv["SVGR"]<-svg
       names(rtv)<-c('hint','snip','logo')[m]
       rtv
    }, error=function(e){NULL} )
  } else {
    rtv<-NULL
  }
  rtv
}

#' @param drps text from drippet file
#' @return list of drippet tripples (named character vectors)
#' 
#' Returns list of character vectors named with names'hint', 'snip', 'logo' representing a drippet
dripplets2List2<-function(drps){
  drps<-unlist(str_split(drps,pattern = '\\*{3,}'))
  # tmpdir=tempdir()
  # tmpdir='drippets'
  drps<-lapply(drps, function(dr){
    # rtv<-extractDripplet(dr, tmpdir)
    rtv<-extractDripplet(dr)
    rtv
  })
  drps
}

row2DrippletBlock<-function(lineNo){
  unlist(str_split(temp, '\n'))->lines
  pose<-grep('^```', lines)
  posb<-1+grep('^SVGR', lines)
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
  posb<-1+grep('^SVGR', lines)
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


#test



