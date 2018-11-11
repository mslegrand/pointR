
#---commit  button----- 
observeEvent(input$commitButton, {
    cat('>---> input$commitButton\n')
    triggerRefresh(sender='cmd.commit', rollBack=FALSE)
    cat('<---< input$commitButton\n')
})

# commitMssg is triggered by ace upon ctl+shift+enter
observeEvent(input$commitMssg, {
  cat('>---> input$input$commitMssg\n')
  triggerRefresh(sender='cmd.commit', rollBack=FALSE)
  cat('<---< input$input$commitMssg\n')
})

# observe({
#   c(input$commitButton,input$commitMssg )
#  
#   # commitButton is button
#   isolate({
#     cat('>---> input$commitButton, input$commitMssg\n')
#     sender='cmd.commit'
#     triggerRefresh(sender, rollBack=FALSE)
#     cat('<---< input$commitButton, input$commitMssg\n')
#   })
# })

#---commit rmdView button----- 
observeEvent(input$writeNOpen ,{
  setTabRequest(sender='buttonCmd.rmdViewer', tabs=input$pages)
}, label= "writeNOpen")



