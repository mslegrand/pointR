library(stringr)
getPtDefPos<-function(txt){
  str_locate(txt,"ptDefs")->beg
  str_locate_all(txt,"\\(")->ilft
  str_locate_all(txt,"\\)")->irt
  
  apply(ilft[[1]],1,function(x)x[1])->tmp1
  apply(irt[[1]],1,function(x)x[1])->tmp2
  
  v<-rep(0,nchar(txt))
  lft2<-rep(0,nchar(txt))
  lft2[tmp1]<-1
  rt2<-rep(0,nchar(txt))
  rt2[tmp2]<-1
  
  cumsum(lft2-rt2)->tmp
  tmp[1:beg[[2]]]<-5
  tmp
  
  ti<-2:length(tmp)
  which(tmp[ti]==0 & tmp[ti-1]==1)->nn
  nn<-nn+1
  return( c(start=beg[1], end=nn) )
}


replacePtDef<-function(txt, replacement="ptsdefs<-c()"){
  pos<-getPtDefPos(txt)
  paste(
    substr(txt, 1, pos[1]-1),
    replacement,
    substr(txt,pos[2]+1,nchar(txt)),
    sep=""
  )
}


getPtDef<-function(txt){
 pos<-getPtDefPos(txt)
 substr(txt, pos[1], pos[2])
}

formatPts<-function(pts){
    tmp<-unlist(pts)
    tmp<-matrix(tmp,2,)
    tmp<-apply(tmp, 2, function(x)paste(x,collapse=","))
    tmp<-paste("c(",tmp,")")
    tmp<-paste(tmp, collapse=",")
    tmp<-paste0("c(", tmp, ")")
}


