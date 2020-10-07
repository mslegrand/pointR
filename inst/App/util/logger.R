
SHOW_DEBUG_LOG=FALSE

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