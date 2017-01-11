
pts2text %<c-% function(pts, txtArry=c(), ...){
    if(length(pts)<2){
        return(NULL)
    }
    tLen<-length(txtArry)
    if(tLen< ncol(pts)){
        txtArry<-c(txtArry, paste0((tLen+1):ncol(pts)))
    }
    lapply(1:ncol(pts), function(i){
        text(xy=c(10,0) + pts[,i], txtArry[i], ...)
    })
}