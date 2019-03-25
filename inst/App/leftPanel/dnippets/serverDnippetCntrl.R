dnippetSelection<-reactiveValues(
  all=list() # contains all dnippets 
)


add2DnippetsSelectionAll<-function(dnName, dnippets ){
  dnippetSelection$all[[dnName]]=dnippets
}



observeEvent(c( getDnippetsAll(), dnippetsDB$usage, input$pages),{
  if(!is.null(input$pages)){
    mode<-getMode()
    # cat("\n>-----> observeEvent(c( getDnippetsAll(), getDnippetsSelected(), input$pages):\n"  )
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
  
  if(length(input$pages) && length(getDnippetsAll())>0 && identical( getModeX(),'ptr') ){
    showElement('selectedDnippetButtonBoxContainer')
  } else {
    hideElement('selectedDnippetButtonBoxContainer')
  }
  # cat("<-----< observeEvent(c( getDnippetsAll(), getDnippetsSelected(), input$pages):\n\n"  )
}, label='getDnippetsAll+usage+pages')

observeEvent(input$selectedDDDnippets,{
  if(!is.null(input$pages)){
    # cat("\n>---> observeEvent(input$selectedDDDnippets\n")
    selected=input$selectedDDDnippets
    setDnippetsSelected(input$pages , selected)
    # cat("<---<  observeEvent(input$selectedDDDnippets\n\n")
  }
  
}, ignoreInit = FALSE, ignoreNULL = FALSE, label='selectedDDDnippets')


