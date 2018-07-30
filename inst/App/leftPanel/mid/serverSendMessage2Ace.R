
updateAceExtDef<-function(newPtDef, sender, selector=list() ){
  if(!is.null(getCode())){
    newPtDef$tib<-pts2Integers(newPtDef$tib )
    replacementList<-ptDef2ReplacementList(name, newPtDef, getCode() ) #name not used!!!
    if( length(replacementList)>0 ){
      data<-list(id= getAceEditorId(), replacement=replacementList, selector=selector, sender=sender, ok=1)
      lapply(data, function(x){
        if(any(unlist(lapply(x, is.na )))){
          print(data)
          stop("encounterd an NA")
        }
      })
      session$sendCustomMessage( type = "shinyAceExt", data )
    }
  }

}


updateAceExt<-function(id, sender, ... ){
  data<-list(...)
  if(is.null(sender)){stop('null sender')}
  if(length(data)>0){
    if(length(id)==0){
      id<-'bogus'
    }
    data<-c(list(id= id, sender=sender), data )
    if(length(id)>0 && nchar(id)>0){
      lapply(data, function(d){
        if(length(d)==0){
          cat("-----------\n")
          print(data)
          stop('d has length 0')
        }
        if(length(d)==1 && is.na(d)){
          print(data)
          stop("encounterd an NA")
        }
      })
      session$sendCustomMessage(
        type = "shinyAceExt",
        data
      )
    }
    
  }
}

