output$htmlHelpSvg_Out<-renderText(
  htmlHelpSvgOut()
)

htmlHelpSvgOut<-reactive({
  HTML(htmlHelpR$elements)
})

htmlHelpR<-reactiveValues(
  elements="hi there"
)

observeEvent(input$helpSvgR,{
  query<-input$helpSvgR
  htmlHelpR$elements<-getSvgRHelpTopic(query)
})

observeEvent(input$helpSvgRMssg,{
  query<-input$helpSvgRMssg
  htmlHelpR$elements<-getSvgRHelpTopic(query)
})

#---- help popup  
modalHelp <- function(..., size = "m" ) {
  
  modalDialog(
    div( width="100%",
      htmlOutput(outputId = "htmlHelpSvg_Out", width="100%")
    ),
    title="Help",
    easyClose = TRUE,
    ...
  ) 
}

getSvgRHelpTopic<-function(query){
  query<-trimws(query)
  pkg<-"svgR"
  pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg), 'help', pkg))
  #links = tools::findHTMLlinks()
  topics<-names(pkgRdDB)
  if(!(query %in% topics)){
    query<-'svgR'
  }
  txtConnection<-textConnection("html","w")
  #tools::Rd2HTML(pkgRdDB[[query]],Links=links, out=txtConnection)
  tools::Rd2HTML(pkgRdDB[[query]],Links=NULL, out=txtConnection)
  close(txtConnection)
  html<-paste(html,collapse="\n") 
  pattern='(<dt><code>([^<]+)</code></dt>)'
  replace='<dt><code><a onclick="helpSvgRTopic(\'\\2\')" >(\\2)</a></code></dt>'
  html<-gsub(pattern=pattern, replacement=replace, x=html, perl=FALSE)
  html
}

#----observer that triggers help popup  
observeEvent(input$helpMssg, {
  query<-input$helpMssg
  if(length(query)>0 && nchar(query)>0){
    htmlHelpR$elements<-getSvgRHelpTopic(query)
    showModal( modalHelp() )
  }    
}) 

observeEvent( input$keyBoardHelp,{
  kb<-input$keyBoardHelp
  print(kb)
}) 




