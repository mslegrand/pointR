sendPtRManagerMessage<-function(sender, ...){ 
  data<- c( list(sender=sender), list(...), list(fk=runif(1)))
  lapply(data, function(dd){
    if(any(sapply(dd,is.na))){
      print(data)
      stop("encounterd an NA")
    }
  })
  session$sendCustomMessage( type = "ptRManager", data)
}

