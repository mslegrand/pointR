
observeEvent( input$tbNewFile ,{
  cmdFileNewPtR()
}, ignoreInit = TRUE)

observeEvent( input$tbSaveFile ,{
  cmdFileSave()
}, ignoreInit = TRUE)

observeEvent( input$tbCloseFile ,{
  cmdFileClose()
}, ignoreInit = TRUE)

hBaRR<-reactiveValues(
  observers=list()
)

observeEvent( request$sender, {
    genTBObserver<-function(tbId, cmd){
      return(observeEvent(
        input[[tbId]],{
          # session$sendCustomMessage(
          #   type = "shinyAceExt",    
          #   list(id= getAceEditorId(), sender='fileCmd.toolbar', tbMssg=cmd)
          # )
          updateAceExt( id= getAceEditorId(), sender='fileCmd.toolbar', tbMssg=cmd )
        }, ignoreInit=TRUE)
      )
    }
    tbID<-setdiff(names(bar1), c('tbNewFile', 'tbSaveFile'))
    hBaRR$observers<-lapply(names(bar1), function(tbId){
      genTBObserver(tbId,bar1[[tbId]]$cmd)
    })
    tbID<-names(bar2)
    hBaRR$observers<-lapply(names(bar2), function(tbId){
      genTBObserver(tbId,bar2[[tbId]]$cmd)
    })
}, once = TRUE)

