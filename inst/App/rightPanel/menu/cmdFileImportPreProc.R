
cmdPreProcPtsImport<-function(){
  click('buttonPreProcPtImport')
}

loadPrePoints<-function(datapath){
  # extractBodyWithComments<-function(fn){
  #   tt<-capture.output(print(fn))
  #   tt<-tt[-1] # TODO: refactor for better reliability
  #   tt<-tt[-length(tt)]  
  #   tt<-tt[-length(tt)]
  #   paste(tt, collapse="\n")
  # }
  
  extractBodyWithComments<-function(fn){
    tt<-capture.output(print(fn))
    tt<-paste(tt, collapse="\n")
    pos1<-str_locate_all(tt,'\\{')[[1]][1]
    if(length(pos1)==0) {stop('ill formed preproc')}
    pos2<-str_locate_all(tt,'\\}')[[1]]
    if(length(pos2)==0) {stop('ill formed preproc')}
    pos1<-pos1[1]+1
    pos2<-pos2[length(pos2)]-1
    substr(tt,pos1,pos2)
    
  }
  
  tryCatch({
    #preProcList<-unlist(source(datapath, local=T)$value)
    
    preProcList<-source(datapath, local=T)$value
    # browser()
    #check preProcList
    if(is.null(preProcList) ||  
       length(preProcList)!=3 ||
       any(match(names(preProcList), preprocChoices   , 0 )==0)
      
    ){
      stop('ill-formed  preprocessor')
      # todo better message
    }
    
    preProcList<-sapply(preProcList, extractBodyWithComments)
    # browser()
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
    loadPrePoints(datapath)
  }
})