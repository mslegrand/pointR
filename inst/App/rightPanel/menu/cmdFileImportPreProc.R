
cmdPreProcPtsImport<-function(){
  click('buttonPreProcPtImport')
}
cmdPreProcAtsImport<-function(){
  click('buttonPreProcAtImport')
}


loadPreProc<-function(datapath, type){
  extractBodyWithComments<-function(fn){
    tt<-capture.output(print(fn))
    blanks1<-grepl('^ *$',tt)
    blanks2<-c(blanks1[-1], FALSE)
    bad<-blanks1 & blanks2
    tt<-tt[!bad]
    tt<-sub('^( )( )*','',tt) #eat all indents :()
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
    preProcList<-source(datapath, local=T)$value
    #check preProcList
  
    if(is.null(preProcList) ||  
       !(setequal(names(preProcList), preprocChoices[[type]]))
    ){
      stop('ill-formed  preprocessor')
      # todo better message
    }
    
    preProcList<-sapply(preProcList, extractBodyWithComments)
    
    
    # remove 1st 2 spaces of each line
    if(type=='points'){
      auxPath<-getPreProcPPAuxPath()
    } else {
      auxPath<-getPreProcPAAuxPath()
    }
    
    #if this works, copy datapath to aux and reload
      file.copy(datapath, auxPath, overwrite = TRUE)
      readAuxPreProcs()
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
    loadPreProc(datapath, 'points')
  }
})

observeEvent(input$buttonPreProcAtImport,{
  fp.dt<-parseFilePaths(c(home='~'), input$buttonPreProcAtImport)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    datapath<-gsub(pattern = '^NA/', "~/", datapath)
    loadPreProc(datapath, 'attrs')
  }
})