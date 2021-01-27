
SHOW_DEBUG_LOG=TRUE

log.fin<-function(x){ 
  if(SHOW_DEBUG_LOG){
    cat(
      '>>>',
      '------',
      deparse(substitute(x)),
      '------\n',
      sep=""
    )
  }
}
log.fout<-function(x){ 
  if(SHOW_DEBUG_LOG){
    cat(
      '<<<',
      '------',
      deparse(substitute(x)),
      '------\n',
      sep=""
    )
  }
}
log.val<-function(x){ 
  if(SHOW_DEBUG_LOG){
    tryCatch(
      cat(
        '   ',
        deparse(substitute(x)),
        '=',
        format(x),
        '\n',
        sep=""
      ),
      error=function(e){
        print(e)
      }
      
    )
  }
}

log.counter<-function(x=0){
  x<-x+1
  cat(paste0('counter-',x),'\n')
  x
}