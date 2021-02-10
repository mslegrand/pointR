
observeEvent( input$tbNewFile ,{
  cmdFileNewPtR()
}, ignoreInit = TRUE)

observeEvent( input$tbSaveFile ,{
  tabId<-input$pages
  docFilePath<-getFileDescriptor(tabId )$filePath
  if(!is.null(docFilePath) && !docFilePath=='?'){
    cmdFileSave()
  } else {
    cmdFileSaveAs()
  }
  
}, ignoreInit = TRUE)

observeEvent( input$tbCloseFile ,{
  cmdFileClose()
}, ignoreInit = TRUE)

# tbObserverList<-function(){
#   genTBObserver<-function(tbId, cmd){
#     return(observeEvent(
#       input[[tbId]],{
#         updateAceExt( id= getAceEditorId(), sender='fileCmd.toolbar', tbMssg=cmd )
#       }, ignoreInit=TRUE)
#     )
#   }
#   c(
#     lapply(names(bar1), function(tbId){
#       genTBObserver(tbId,bar1[[tbId]]$cmd)
#     }),
#     lapply(names(bar2), function(tbId){
#       genTBObserver(tbId,bar2[[tbId]]$cmd)
#     })
#   )
# }

# hBaRR<-reactiveValues(
#   observers=tbObserverList() #list()
# )

hBaRR<-reactiveValues(
  observers=list()
)

#observeEvent( request$cmd, {
observeEvent( getCode(), {
    genTBObserver<-function(tbId, cmd){
      return(observeEvent(
        input[[tbId]],{
          updateAceExt( id= getAceEditorId(), sender='fileCmd.toolbar', tbMssg=cmd )
        }, ignoreInit=TRUE)
      )
    }
    #tbID<-setdiff(names(bar1), c('tbNewFile', 'tbSaveFile'))
    hBaRR$observers<-
      lapply(names(bar1), function(tbId){
      genTBObserver(tbId,bar1[[tbId]]$cmd)
    })
    #tbID<-names(bar2)
    hBaRR$observers<-
      lapply(names(bar2), function(tbId){
      genTBObserver(tbId,bar2[[tbId]]$cmd)
    })
}, once = TRUE)


