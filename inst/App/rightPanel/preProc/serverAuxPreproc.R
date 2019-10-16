preProcScriptDB<-reactiveValues(
  points=tibble( scriptName='bogus', cmd="bogus", script='bogus')[0,],
  attrs=tibble( scriptName='bogus', cmd="bogus", script='bogus')[0,]
)

loadAuxPreProc<-function(fullName){
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
    preProcList<-source(fullName, local=T)$value
    #check preProcList
    if(is.null(preProcList) ||  
       #length(preProcList)!=3 ||
       any(match(names(preProcList), unlist(preprocChoices)   , 0 )==0)
       
    ){
      stop('ill-formed  preprocessor')
      # todo better message
    }
    tb<-tibble(scriptName=basename(fullName), cmd=names(preProcList), script=preProcList)
    # may need to use cmds instead
    # browser()
    if( "preprocPts"==basename(dirname(fullName))){
      preProcScriptDB$points<-rbind(preProcScriptDB$points, tb)
    } else if( "preprocAts"==basename(dirname(fullName))){
      preProcScriptDB$attrs<-rbind(preProcScriptDB$attrs, tb)
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
  for(fullName in ppfiles ){
    # loadPreProc
    readAuxPreProc(fullName)
  }
}


getPreProcPPAuxPath<-reactive({file.path(getDirPath(),'aux','preprocPts')})   
getPreProcPAAuxPath<-reactive({file.path(getDirPath(),'aux','preprocAts')})   

readAuxPreProcs<-function( startup=TRUE){
  
  preProcFilePaths<-c(
    list.files(getPreProcPPAuxPath(), full.names=TRUE),
    list.files(getPreProcPAAuxPath(), full.names=TRUE)
  ) 
  for(fp in preProcFilePaths){
    loadAuxPreProc(fp)
  }
}



getPreProcChoices<-reactive({
  rtv<-c()
  if( getTibEditState()==TRUE ){
    browser()
    
    if(getColumnType()=='point'){
      rtv<-unique(preProcScriptDB$points$scriptName)
    } else {
      rtv<-unique(preProcScriptDB$attrs$scriptName)
    }
  }
  rtv
})

hasPreProcChoices<-reactive({ length(getPreProcChoices)>0})
