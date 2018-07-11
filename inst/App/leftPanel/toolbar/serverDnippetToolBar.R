getDnippets4ToolBar<-function(dnippetList){
  dnippetList<-dnippetList[!sapply(dnippetList, is.null)]
  
  temp<-lapply(dnippetList, function(dr){
    
     hint<-dr[['hint']]
    if(is.null(hint) || nchar(hint)==0){
      hint="no hint"
    }
    image<-as.character(dr["logo"])
    if(nchar(image)==0){
      image<-'NA'
    }
    id=paste0(sample(letters,5), collapse="")
    snip<-dr[["snip"]]
    
    list(id=id, hint=hint, snip=snip, logo=image)
  })
  temp
}


