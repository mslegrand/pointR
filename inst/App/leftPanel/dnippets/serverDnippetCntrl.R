dnippetSelection<-reactiveValues(
  #current=NULL, # contains names of selected dnippets
  all=list() # contains all dnippets 
)

# addDrippets<-function(dnName, dnippets, select=dnName ){
#   drippetSelection$all[[dnName]]=dnippets
#   #drippetSelection$current<-c(drippetSelection$current, select)
# }

addDnippets2AllDnippets<-function(dnName, dnippets ){
  dnippetSelection$all[[dnName]]=dnippets
  #drippetSelection$current<-c(drippetSelection$current, select)
}

observeEvent(c( getDnippetsAll(), getDnippetsSelected(), input$pages),{
  mode<-getMode()
  
  updateAwesomeCheckboxGroup(session, 
     inputId="selectedDDDnippets", 
     choices  = getDnippetsAll(),
     selected = getDnippetsSelected(), 
     inline = FALSE, status = "primary")
  if(length(input$pages) && length(getDnippetsAll())>0 && identical(getMode(),'ptr') ){
    showElement('selectedDnippetButtonBoxContainer')
  } else {
    hideElement('selectedDnippetButtonBoxContainer')
  }
})

observeEvent(input$selectedDDDnippets,{
  selected=input$selectedDDDnippets
  setDnippetsSelected(input$pages , selected)
  #drippetSelection$selected=selected
  dnippets<-dnippetSelection$all[selected]
  dnippets<-unlist(dnippets,recursive=F)
  
  names(dnippets)<-NULL
  if(length(dnippets)==0){
    sendPtRManagerMessage(sender='cmd.dnippet.file.load', removeDrippets=runif(1));
  } else{
    sendPtRManagerMessage(sender='cmd.dnippet.file.load', insertDrippets=dnippets)
  }
  
}, ignoreInit = TRUE, ignoreNULL = FALSE)

