pts2boxes %<c-% function(
    pts,  txtArry=c(), wh=c(100,40), rxy=c(0,0), 
    fill='none', stroke='black', ...){
  if(length(pts)<2){
    return(NULL)
  }
  tLen<-length(txtArry)
  if(tLen< ncol(pts)){
    txtArry<-c(txtArry, paste0((tLen+1):ncol(pts)))
  }
  lapply(1:ncol(pts), function(i){
    pt<-pts[,i]+c(0,.5)*wh
    txt<-txtArry[i]
    g(
      rect(cxy=pt, wh=wh, rxy=rxy, fill=fill, stroke=stroke,  ...),
      text(cxy=pt, txt, fill=fill, stroke=stroke, ...)
    )
  })
}

