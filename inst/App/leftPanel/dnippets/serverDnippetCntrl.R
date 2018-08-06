dnippetSelection<-reactiveValues(
  #current=NULL, # contains names of selected dnippets
  all=list() # contains all dnippets 
)

# addDrippets<-function(dnName, dnippets, select=dnName ){
#   drippetSelection$all[[dnName]]=dnippets
#   #drippetSelection$current<-c(drippetSelection$current, select)
# }

add2DnippetsSelectionAll<-function(dnName, dnippets ){
  dnippetSelection$all[[dnName]]=dnippets
  #drippetSelection$current<-c(drippetSelection$current, select)
}



observeEvent(c( getDnippetsAll(), dnippetsDB$usage, input$pages),{
  if(!is.null(input$pages)){
    mode<-getMode()
    cat("\n>-----> observeEvent(c( getDnippetsAll(), getDnippetsSelected(), input$pages):\n"  )
    all<-getDnippetsAll()
    cat("getDnippetsAll()=",format(all),"\n")
    cat("getDnippetsSelected()=",format(getDnippetsSelected()),"\n")
    cat("input$pages=",format(input$pages),"\n")
    cat("Now updateAwesomeCheckboxGroup with inputId= selectedDDDnippets\n")
    # browser()
    updateAwesomeCheckboxGroup(session, 
       inputId="selectedDDDnippets", 
       choices  = getDnippetsAll(),
       selected = getDnippetsSelected(), 
       inline = FALSE, status = "primary"
    )
    
    selected<-getDnippetsSelected()
    dnippets<-dnippetSelection$all[selected]
    dnippets<-unlist(dnippets,recursive=F)
    cat("sendPtRManagerMessage cmd.dnippet.file.load\n")
    names(dnippets)<-NULL
    if(length(dnippets)==0){
      sendPtRManagerMessage(sender='cmd.dnippet.file.load', removeDrippets=runif(1));
    } else{
      sendPtRManagerMessage(sender='cmd.dnippet.file.load', insertDrippets=dnippets)
    }
  }
  
  if(length(input$pages) && length(getDnippetsAll())>0 && identical(getMode(),'ptr') ){
    showElement('selectedDnippetButtonBoxContainer')
  } else {
    hideElement('selectedDnippetButtonBoxContainer')
  }
  cat("<-----< observeEvent(c( getDnippetsAll(), getDnippetsSelected(), input$pages):\n\n"  )
})

observeEvent(input$selectedDDDnippets,{
  if(!is.null(input$pages)){
    
    cat("\n>---> observeEvent(input$selectedDDDnippets\n")
    cat('input$pages=',format(input$pages),"\n")
    selected=input$selectedDDDnippets
    cat("selected=",format(selected),"\n")
    cat("about to setDnippetsSelected(",format(input$pages), ",", selected,")")
    # browser()
    setDnippetsSelected(input$pages , selected)
    #drippetSelection$selected=selected
    cat("<---<  observeEvent(input$selectedDDDnippets\n\n")
  }
  
}, ignoreInit = FALSE, ignoreNULL = FALSE)

# observeEvent( input$drippetSelectionDropdown, {
#   #tabId=input$pages
#   updateAwesomeCheckboxGroup(session,
#                              inputId="drippetSelectionDropdown",
#                              choices  = getDnippetsAll(),
#                              selected = getDnippetsSelected(),
#                              inline = FALSE, status = "primary")
# })
