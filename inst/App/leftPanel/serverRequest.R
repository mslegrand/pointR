
theCode<-reactiveVal("")

request<-reactiveValues(
  sender=NULL,
  tabs=NULL,
  trigger=0
)



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


setTabRequest<-function(sender, tabs){
  # if(length(sender)==1 && length(tabs)>1){
  #   sender<-rep_len(sender,length(tabs) )
  # }
  request$sender<-sender
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
  request$sender
}


clearRequest<-function(  ){
  request$sender<-NULL
  request$tags<-list()
}

requestStartUp<-function(){ 
  setTrigger('startup')
}

peekTab<-reactive( {request$tabs[1]} )
popTab<-reactive({
  tab<-request$tabs[1]
  request$tabs<-request$tabs[-1]
  # ?????   if length(tabs is 0, remove sender?)
  tab
})

setCode<-function(code){
  theCode(code)
}
getCode<-reactive({
  theCode()
})
