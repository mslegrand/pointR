
theCode<-reactiveVal("")
theBlocks<-reactiveVal(NULL)
theEnvList<-reactiveVal(list()) #or NULL?

request<-reactiveValues(
  cmd=NULL,
  tabs=NULL,
  trigger=0,
  predoc=""
)

getWDCmd<-reactive({
  log.fin(getWDCmd)
  dpath<-getDirPath()
  log.val(dpath)
  if(identical(dpath, '~/.ptR')){
    dpath<-'~'
  }
  dd<-paste0('\nsetwd("',dpath,'")\n\n')
  log.fout(getWDCmd)
  dd
})


getEnvList<-reactive({
  wd<-getWDCmd()
  pcode<-theBlocks()
  initialEnv=new.env()
  if(!is.null(pcode) && pcode!=""){
    pcode=paste(wd,pcode,sep="\n")
  } else {
    pcode<-wd
  }
  eval(parse(text=pcode),initialEnv)
  initialEnv
})

trigger<-reactiveValues(
  redraw=0,
  startup=0
)

setTrigger<-function(what="redraw"){
  trigger[[what]]<-sample(10^9,1)
}

getCode4Rendering<-eventReactive( trigger$redraw, {
  getCode()
})

getCode4RenderingTransform<-eventReactive( trigger$redraw, {
  src<-getCode()
  src<-usingDraggable(src, getTransformType())
  src
})


setTabRequest<-function(cmd, tabs){
  # if(length(cmd)==1 && length(tabs)>1){
  #   cmd<-rep_len(cmd,length(tabs) )
  # }
  request$cmd<-cmd
  request$tabs<-tabs
  request$trigger<-sample(10^6,1)
}

popTabRequest<-function(){
  tab   <- request$tabs[1]
  request$tabs  <-request$tabs[-1]
  tab
}

peekTabRequest<-function(){
  request$tabs[1]
}

peekTabCmd<-function(){
  request$cmd
}

requestStartUp<-function(){ 
  setTrigger('startup')
}

peekTab<-reactive( {request$tabs[1]} )
popTab<-reactive({
  tab<-request$tabs[1]
  request$tabs<-request$tabs[-1]
  # ?????   if length(tabs is 0, remove cmd?)
  tab
})

setBlocks<-function(blocks){ # not used???
  theBlocks(blocks)
}

getBlocks<-reactive({ # not used???
  theBlocks()
})

setCode<-function(code){
  theCode(code)
}
getCode<-reactive({
  theCode()
})
