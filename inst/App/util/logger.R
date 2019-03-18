
log.fin<-function(x){ 
  cat(
    '>>>',
    '------',
    deparse(substitute(x)),
    '------\n',
    sep=""
  )
}
log.fout<-function(x){ 
  cat(
    '<<<',
    '------',
    deparse(substitute(x)),
    '------\n',
    sep=""
  )
}
log.val<-function(x){ 
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