sendPtRManagerMessage<-function(sender, ...){ 
  # cat('entering ---------------sendPtRManagerMessage---------------------\n')
  data<- c( list(sender=sender), list(...), list(fk=runif(1)))
  # if(identical(sender,'tibNrow')){
  #   cat("Enter==============tibNRow data ======================\n")
  #   print(data)
  #   cat("Exit==============tibNRow data =======================\n")
  # }
  lapply(data, function(dd){
    if(any(sapply(dd,is.na))){
      print(data)
      stop("encounterd an NA")
    }
  })
  session$sendCustomMessage( type = "ptRManager", data)
  # cat('exiting ---------------sendPtRManagerMessage---------------------\n')
}

