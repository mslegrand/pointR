
theCode<-reactiveVal("")

request<-reactiveValues(
  #code=NULL, 
  sender='startup',
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
  request$sender<-sender
  request$tabs<-tabs
  request$trigger<-sample(10^6,1)
}

setRequests<-function( requestList ){
  
}

popRequest<-function(  ){
  tab<-request$tabs[1]
  request$tabs<-request$tabs[-1]
  # ?????   if length(tabs is 0, remove sender?)
  c(request$sender, tab)
}

peekRequest<-reactive( {
  c(request$sender,request$tabs[1])
} )


clearRequest<-function(  ){
  request$sender<-NULL
  request$tags<-list()
}



getRequestSender<-reactive({request$sender}) 
#setRequestSender<-function(sender){request$sender<-sender} 

requestStartUp<-function(){ 
  setTrigger('startup')
  #request$sender<-'startup'
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
  #request$code
})
