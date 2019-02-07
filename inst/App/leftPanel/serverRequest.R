request<-reactiveValues(
  code=NULL, 
  sender='startup',
  tabs=NULL,
  trigger=0
)

trigger<-reactiveValues(
  redraw=0
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

getSender<-reactive({request$sender})
peekTab<-reactive( {request$tabs[1]} )
popTab<-reactive({
  tab<-request$tabs[1]
  request$tabs<-request$tabs[-1]
  tab
})


getCode<-reactive({
  request$code
})
