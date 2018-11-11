
cmdPreProcPtsImport<-function(){
  click('buttonPreProcPtImport')
}

loadPrePoints<-function(datapath){

  tryCatch({
    preProcList<-unlist(source(datapath, local=T)$value)
    #check preProcList
    if(is.null(preProcList) ||  
       length(preProcList)!=3 ||
       any(match(names(preProcList), preprocChoices   , 0 )==0)
       # TODO: check for return of eval(parse(text=preProcList[i])) for each i
    ){
      stop('bad preproc')
    }
    insertPreProcPtEntry(
      tab_Id= getTibTabId(),  
      tib_Name=getAssetName(),
      pt_Column_Name=getTibColumnName(),
      newScript=preProcList
    )
    selection<-names(preProcList)[1]
    updateRadioGroupButtons(session, "ptPreProcCmdChoice", selected=selection )
    txt=preProcList[1]
    updateAceEditor(session, editorId='ptPreProcAceEditor', value=txt)
  }, 
  error=function(e){
        e<-c(e,traceback())
        err<-paste(unlist(e), collapse="\n", sep="\n")
        alert(err)
  })
}

observeEvent(input$buttonPreProcPtImport,{
  fp.dt<-parseFilePaths(c(home='~'), input$buttonPreProcPtImport)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    datapath<-gsub(pattern = '^NA/', "~/", datapath)
    # cat('loading ', datapath,"\n")
    loadPrePoints(datapath)
  }
})