
#---commit  button----- 
observe({
  c(input$commit,input$commitMssg )
  isolate(
    {
    sender='cmd.commit'
    triggerRefresh(sender, rollBack=FALSE)
  })
})



#---commit  button----- 
observeEvent(input$writeNOpen ,{
  setTabRequest(sender='buttonCmd.rmdViewer', tabs=input$pages)
}, label= "writeNOpen")



