



# used by processKnit, processDnip, plotRmd
dripplets2Rmd<-function( drps ){
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
  
  drps<-str_replace_all(drps, "svgR\\(", 'temp<-svgR(')
  m<-imageBlockIndices(drps)
  # print(m)
  unlist(str_split(drps, '\n'))->drps
  indx<-m[2,]
  drps[indx]<-paste(
     "temp$root$setAttr('width',48)",
     "temp$root$setAttr('height',32)",
    # "temp$root$setAttr('viewBox','0 0  48 32')",
    "temp$root$setAttr('stroke','#00FFFF')",
    "temp$root$setAttr('fill','#00FFFF')",
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
  drps<-str_replace_all(drps, "\nSVGR\\s*\n```\n", '\n\n```\\{r, echo=FALSE, results=\'asis\'\\}\nWH=c(48,32)\n')
  drps
}



# NOT USED!!!
# dripplets2List<-function(drps){
#   
#   extractVal<-function(x,pattern){
#     # cat('extractVal\n')
#     rtv<-str_match_all(x,pattern)
#     # print(rtv)
#     rtv<-rtv[[1]]
#     if(length(rtv)>0){
#       rtv<-rtv[1,2]
#     } else {
#       rtv<-NULL
#     }
#     rtv
#   }
#   
#   drps<-str_split(drps,pattern = '\\*{3,}')
#   drps<-drps[[1]]
#   
#   i<-1
#   drps<-lapply(drps, function(dr){
#     rtv<-list()
#     # cat(i, 'hint\n')
#     rtv$hint<-    extractVal(dr, "(?:\nPOPUP\n```\n)(.+)(?:\n```\n)")
#     # cat(i, 'snippet\n')
#     rtv$snippet<- extractVal(dr, "(?:\nSNIPPET\\s*\n```\\s*\n)(.+)(?:\n```\\s*\n)")
#     rtv
#   })
# }

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
       # svg<-as.character(eval(parse(text=tt['SVGR']), new.env()))
       # svg<-eval(parse(text=tt['SVGR']), new.env())
       svg<-eval(parse(text=tt['SVGR']), list(WH=c(48,32)))
       svg$root$setAttr('stroke','#00FFFF')
       svg$root$setAttr('fill','#00FFFF')
       svg$root$setAttr('width',48)
       svg$root$setAttr('height',32)
       #set width, height???
       svg<-as.character(svg)
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
#' uses in cmdFileDnippet.R
dripplets2List2<-function(drps){

  drps<-unlist(str_split(drps,pattern = '\\*{3,}'))
  drps<-lapply(drps, function(dr){
    rtv<-extractDripplet(dr)
    rtv
  })
  s.upper<-function(s){
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }
  drps<-lapply(drps, function(dr){
    dr['snip']<-gsub("\\```","```" , dr['snip'])
    dr
  })
  if(!is.null(editOption$currentProjectName)){
    cname<-sub('\\.pprj$','', editOption$currentProjectName)
    Cname<-s.upper(cname)
    drps<-lapply(drps, function(dr){
      dr['snip']<-gsub('@projectName@', cname , dr['snip'])
      dr['snip']<-gsub('@ProjectName@', Cname , dr['snip'])
      dr
    })
  }
  drps
}

# row2DrippletBlock<-function(lineNo){
#   unlist(str_split(temp, '\n'))->lines
#   pose<-grep('^```', lines)
#   posb<-1+grep('^SVGR', lines)
#   indx<-which(pose %in% posb)
#   posx<-pose[indx]
#   posy<-pose[indx+1]
#   
#   indxx<-which((posx <=row) & (row<=posy))
#   if(length(indxx)>0){
#     rtv<-paste(lines[(posx[indxx]+1):(posy[indxx]-1)], collapse="\n")
#   } else {
#     rtv<-NULL
#   }
#   rtv
# }

# row2DrippletBlockIndices<-function(lineNo){
#   unlist(str_split(temp, '\n'))->lines
#   pose<-grep('^```', lines)
#   posb<-1+grep('^SVGR', lines)
#   indx<-which(pose %in% posb)
#   posx<-pose[indx]
#   posy<-pose[indx+1]
#   
#   indxx<-which((posx <=row) & (row<=posy))
#   if(length(indxx)>0){
#     rtv<-c((posx[indxx]+1),(posy[indxx]-1))
#   } else {
#     rtv<-NULL
#   }
#   rtv
# }


#test
# 
# src<-'
# ---
# title: "Dnd Snippet"
# author: "Anonymous"
# date:   "TODAY"
# output: dnd_snippet
# ---
# 
# - Individual drippets are seperate by lines consisting of three or more stars (*)
# - Each drippet consists of 3 entries, with each entry having a title and a value (block)
#     - The title consists of a single line followed by a colon (:)
#         - titles are *POPUP*, *SNIPPET*, *SVGR*
#         - The values are blocks defined by 3 backtics *````*
# - Two drippets are shown below to help you get started
# 
# 
# *********************
# 
# 
# POPUP
# ```
# Ellipse
# ```
# SNIPPET
# ```
# ellipse(
#   cxy=${1:WH/2},
#   rxy=${2:WH/4},
#   fill=${3:\'orange\'},
#   stroke=${4:\'blue\'},
#   stroke.width=${0:3}
# )
# ```
# SVGR
# ```
# library(svgR)
# WH<-c(48,32)
# ptR<-list(
#   x=matrix(0,2,0)
# )
# svgR(wh=WH,
#   ellipse(
#     cxy=WH/2,
#     rxy=WH/4,
#     fill=\'none\',
#     stroke=\'#00FFFF\',
#     stroke.width=2
#   )
# )
# ```
# *********************
# 
# POPUP
# ```
# Assign Point Matrix
# ```
# SNIPPET
# ```
# =matrix(${1:0},2,${0:0})
# ```
# SVGR
# ```
# library(svgR)
# WH<-c(48,32)
# ptR<-list(
#   x=matrix(0,2,0)
# )
# X=c(.2,.4,.6,.8)
# svgR(wh=WH, stroke.width=2, stroke="#00FFFF",  fille="none",
# polyline(points=c(WH)*c(c(.3,.1),c(.1,.1), c(.1,.9), c(.3,.9))),
# polyline(points=c(WH)*c(c(.7,.1),c(.9,.1), c(.9,.9), c(.7,.9))),
# lapply( X, function(x){
#   list(
#     line(xy1=WH*c(x,.3),xy2=WH*c(x,.4)),
#     line(xy1=WH*c(x,.6),xy2=WH*c(x,.7)))
# })
# 
# )
# ```
# ******************
# '
# 
# 
# 
# src<-dripplets2Rmd(src)
# knit2html(text = src, fragment.only = TRUE, quiet = TRUE)