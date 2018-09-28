
#--tibble box
# observeEvent(input$useTribble,{
#   useTribble<-ifelse(input$useTribble=='Tribble',TRUE,FALSE)
#   if(editOption$useTribbleFormat!=useTribble){
#     editOption$useTribbleFormat=useTribble
#     newPtDefs<-getPtDefs()
#     sender='useTibble'
#     updateAceExtDef(newPtDefs, sender=sender)
#   }
# })



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



# #!!!  unused?
# checkPtrSyntax<-function(src){
#   lines<-strsplit(src,"\n") 
#   lines<-lines[[1]]
#   ptRPos<-grep("^\\s*ptR<-",lines)
#   svgRPos<-grep("^\\s*svgR\\(",lines)
#   if(length(ptRPos)>1 )
#     base::stop("Bad File: Multiple  ptR lists")
#   if(length(svgRPos)>1) 
#     base::stop("Bad File: Multiple  ptR lists or svgR calls")
# }

# observeEvent(input$rmdBrowserButton, {
#   
# })
