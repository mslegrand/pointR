drippetSelection<-reactiveValues(
  current=NULL,
  all=list()
)

addDrippets<-function(dnName, dnippets, select=dnName ){
  drippetSelection$all[[dnName]]=dnippets
  drippetSelection$current<-c(drippetSelection$current, select)
}

observeEvent(c( drippetSelection$all, request$mode, input$pages),{
  updateAwesomeCheckboxGroup(session, inputId="selectedDDDnippets", choices = names(drippetSelection$all),
                             selected = drippetSelection$current, inline = FALSE, status = "primary")
  if(length(input$pages) && length(drippetSelection$all)>0 && identical(request$mode,'ptr') ){
    showElement('selectedDnippetButtonBoxContainer')
  } else {
    hideElement('selectedDnippetButtonBoxContainer')
  }
})

observeEvent(input$selectedDDDnippets,{
  selected=input$selectedDDDnippets
  drippetSelection$selected=selected
  dnippets<-drippetSelection$all[selected]
  dnippets<-unlist(dnippets,recursive=F)
  
  names(dnippets)<-NULL
  if(length(dnippets)==0){
    sendPtRManagerMessage(sender='cmd.dnippet.file.load', removeDrippets=runif(1));
  } else{
    sendPtRManagerMessage(sender='cmd.dnippet.file.load', insertDrippets=dnippets)
  }
  
}, ignoreInit = TRUE, ignoreNULL = FALSE)

