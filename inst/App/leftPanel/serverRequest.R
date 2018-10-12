request<-reactiveValues(
  code=NULL, 
  sender='startup',
  tabs=NULL
)


setTabRequest<-function(sender, tabs){
  request$sender<-sender
  request$tabs<-tabs
}

getSender<-reactive({request$sender})
peekTab<-reactive( {request$tabs[1]} )
popTab<-reactive({
  tab<-request$tabs[1]
  request$tabs<-request$tabs[-1]
  tab
})

# 
# getMode<-reactive({
#   request$mode
# })

getCode<-reactive({
  request$code
})
