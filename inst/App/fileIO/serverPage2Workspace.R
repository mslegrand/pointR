#workspace save


# presumably to be saved whenever there is a commit
savePage<-function(pageId, path=getWorkSpaceDir()){
  if(!is.null(pageId) && nchar(pageId)>0){
    cat('>---> savePage\n')
    fileName=paste0(path,"/",pageId,".rda")
    asel<-reactiveValuesToList(selectedAsset)
    fileDescriptor=getFileDescriptor(pageId)
    backdrop=getPageBackDrop(pageId)
    grid=getPageSvgGrid(pageId)
    trib<-getPageUseTribble(pageId)
    dnip<-getPageDnippetsDB(pageId)
    cat('saving dnip')
    print(dnip)
    rtv<-c(
      fileDescriptor=getFileDescriptor(pageId),
      code=getCode(),
      assetSelection=asel,
      backdrop=backdrop,
      grid=grid,
      trib=trib,
      dnip=dnip
    )
    
    ppE<-getPreProcPtEntries(pageId)
    if(length(ppE)!=0 && nrow(ppE)>0){
      rtv<-c(rtv, preprocScripts=ppE)
    }
    saveRDS(object=rtv, file = fileName)
    cat('<---< savePage\n')
  }
}

