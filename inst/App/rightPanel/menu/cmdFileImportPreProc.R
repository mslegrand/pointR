
cmdPreProcPtsImport<-function(){
  click('buttonPreProcPtImport')
}

loadPrePoints<-function(datapath){
   #preprocText<-paste(readLines(datapath), collapse = "\n")
  
  tryCatch({
    preProcList<-unlist(source(datapath, local=T)$value)
      insertPreProcPtEntry(
        tab_Id= getTibTabId(),  
        tib_Name=getAssetName(),
        pt_Column_Name=getTibColumnName(),
        newScript=preProcList
      )
      #selection<-names(preProcList)[1]
      txt=preProcList[1]
      updateAceEditor(session, editorId='ptPreProcAceEditor', value=txt)
  }, 
  error=function(e){
        e<-c(e,traceback())
        err<-paste(unlist(e), collapse="\n", sep="\n")
        # maybe should do an alert here.
        setErrorMssg(err)
  })
}

observeEvent(input$buttonPreProcPtImport,{
  fp.dt<-parseFilePaths(c(wd='~'), input$buttonPreProcPtImport)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    loadPrePoints(datapath)
  }
})