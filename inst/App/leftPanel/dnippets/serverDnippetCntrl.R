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
  log.fout(c( getDnippetsAll(), getDnippetsSelected(), input$pages))
  if(!is.null(input$pages)){
    selected<-getDnippetsSelected()
    mode<-getMode()
    all<-getDnippetsAll()
    updateAwesomeCheckboxGroup(session, 
       inputId="selectedDDDnippets", 
       choices  = all,
       selected = selected, 
       inline = FALSE, status = "primary"
    )
    
    dnippets<-dnippetSelection$all[selected]
    dnippets<-unlist(dnippets,recursive=FALSE)
    names(dnippets)<-NULL
    if(length(dnippets)==0){ 
      sendPtRManagerMessage(sender='cmd.dnippet.file.load', removeDrippets=runif(1));
    } else{
      sendPtRManagerMessage(sender='cmd.dnippet.file.load', insertDrippets=dnippets)
    }
  }
  
  if(length(input$pages)>0 && length(getDnippetsAll())>0 && 
    any(sapply(c('ptr','javascript'), function(x)identical( getModeX(),x )))
  ){
    showElement('selectedDnippetButtonBoxContainer')
  } else {
    hideElement('selectedDnippetButtonBoxContainer')
  }
  log.fout(c( getDnippetsAll(), getDnippetsSelected(), input$pages))
}, label='getDnippetsAll+usage+pages', ignoreNULL = FALSE,)

observeEvent(input$selectedDDDnippets,{
  if(!is.null(input$pages)){
    selected=input$selectedDDDnippets
    setDnippetsSelected(input$pages , selected)
  }
  
}, ignoreInit = TRUE, ignoreNULL = FALSE, label='selectedDDDnippets')


