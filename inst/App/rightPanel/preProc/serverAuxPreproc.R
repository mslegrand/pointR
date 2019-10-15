
loadAuxPreProc<-function(datapath, type){
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
    preProcList<-source(datapath, local=T)$value
    #check preProcList
    if(is.null(preProcList) ||  
       #length(preProcList)!=3 ||
       any(match(names(preProcList), unlist(preprocChoices)   , 0 )==0)
       
    ){
      stop('ill-formed  preprocessor')
      # todo better message
    }
    tb<-tibble(scriptName=basename(datapath), cmd=names(preProcList), script=preProcList)
    # may need to use cmds instead
    if( "preprocPts"==basename(dirname(filePath))){
      rbind(preProcScriptDB$points, tb)
    } else if( "preprocAts"==basename(dirname(filePath))){
      rbind(preProcScriptDB$attrs, tb)
    }
  }, 
  error=function(e){
    e<-c(e,traceback())
    err<-paste(unlist(e), collapse="\n", sep="\n")
    alert(err)
  })
}

reloadPreProcScriptDB<-function(dirPath, scriptType='points'){
  ppfiles<-dir(dirPath, full.names=TRUE)
  # refresh/add any dnd whose file has just appeared
  for(datapath in ppfiles ){
    # loadPreProc
    loadAuxPreProc(datapath)
  }
}
