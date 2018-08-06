request<-reactiveValues(
  code=NULL, 
  sender='startup',
  tabs=NULL
)

observeEvent(request$sender,{
    if(request$sender=='startup'){
      #Todo: make this workspace dependent
      # cat(">---> startup")
      # cat('readDnippetsFileNams')
      readDnippetsFileNames()
      # cat('now to saveDnippetsFileNames\n')
      saveDnippetsFileNames()
    
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
