
observeEvent( input$tbNewFile ,{
  cmdFileNew()
}, ignoreInit = TRUE)

observeEvent( input$tbSaveFile ,{
  cmdFileSave()
}, ignoreInit = TRUE)


hBaRR<-reactiveValues(
  observers=list()
)

observeEvent( request$sender, {
    genTBObserver<-function(tbId, cmd){
      return(observeEvent(
        input[[tbId]],{
          session$sendCustomMessage(
            type = "shinyAceExt",    
            list(id= getAceEditorId(), tbMssg=cmd)
          )
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

