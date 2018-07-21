request<-reactiveValues(
  code=NULL, 
  mode='ptr',
  sender='startup',
  tabs=NULL
)

observeEvent(request$sender,{
    if(request$sender=='startup'){
      cmdFileNewPtR()
      sampleDnippets<-paste(system.file('App', package='pointR'), 'templates', 'sampleShapes.dnippets', sep='/')
      cat(sampleDnippets)
      loadDndSnippets(sampleDnippets)
    }
}, priority=100)

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


getMode<-reactive({
  request$mode
})

getCode<-reactive({
  request$code
})
