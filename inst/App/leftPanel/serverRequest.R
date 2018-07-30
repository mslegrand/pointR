request<-reactiveValues(
  code=NULL, 
  #mode='ptr',
  sender='startup',
  tabs=NULL
)

observeEvent(request$sender,{
    if(request$sender=='startup'){
      #Todo: make this workspace dependent
      sampleDnippets<-paste(system.file('App', package='pointR'), 'templates', 'sampleShapes.dnippets', sep='/')
      loadDndSnippets(sampleDnippets)
      for( datapath in editOption$dnippetsFiles){
        loadDndSnippets(datapath)
      }
      
      
      #cmdFileNewPtR()
      #add the dndSnipppets previously stored
      success<-restoreWorkSpace()
      if(!success){
        cmdFileNewPtR()
      }
      #
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

# 
# getMode<-reactive({
#   request$mode
# })

getCode<-reactive({
  request$code
})
