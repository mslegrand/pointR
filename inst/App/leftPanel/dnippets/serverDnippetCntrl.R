dnippetSelection<-reactiveValues(
  all=list() # contains all dnippets 
)


add2DnippetsSelectionAll<-function(dnName, dnippets ){
  dnippetSelection$all[[dnName]]=dnippets
}

removeFromDnippetsSelectionAll<-function(dnName ){
  dnippetSelection$all<-dnippetSelection$all[[-dnName]]
}


observeEvent(c( getDnippetsAll(), dnippetsDB$usage, input$pages),{
  if(!is.null(input$pages)){
    log.fin(c( getDnippetsAll(), getDnippetsSelected(), input$pages))
    mode<-getMode()
    all<-getDnippetsAll()
    updateAwesomeCheckboxGroup(session, 
       inputId="selectedDDDnippets", 
       choices  = getDnippetsAll(),
       selected = getDnippetsSelected(), 
       inline = FALSE, status = "primary"
    )
    
    selected<-getDnippetsSelected()
    dnippets<-dnippetSelection$all[selected]
    dnippets<-unlist(dnippets,recursive=F)
    names(dnippets)<-NULL
    if(length(dnippets)==0){ 
      sendPtRManagerMessage(sender='cmd.dnippet.file.load', removeDrippets=runif(1));
    } else{
      sendPtRManagerMessage(sender='cmd.dnippet.file.load', insertDrippets=dnippets)
    }
  }
  
  if(length(input$pages) && length(getDnippetsAll())>0 && 
    any(sapply(c('ptr','javascript'), function(x)identical( getModeX(),x )))
  ){
    showElement('selectedDnippetButtonBoxContainer')
  } else {
    hideElement('selectedDnippetButtonBoxContainer')
  }
  log.fout(c( getDnippetsAll(), getDnippetsSelected(), input$pages))
}, label='getDnippetsAll+usage+pages')

observeEvent(input$selectedDDDnippets,{
  if(!is.null(input$pages)){
    log.fin(input$selectedDDDnippets)
    selected=input$selectedDDDnippets
    setDnippetsSelected(input$pages , selected)
    log.fout(input$selectedDDDnippets)
  }
  
}, ignoreInit = FALSE, ignoreNULL = FALSE, label='selectedDDDnippets')


