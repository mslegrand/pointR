
#--tibble box
observeEvent(input$useTribble,{
  useTribble<-ifelse(input$useTribble=='Tribble',TRUE,FALSE)
  if(editOption$useTribbleFormat!=useTribble){
    editOption$useTribbleFormat=useTribble
    newPtDefs<-getPtDefs()
    sender='useTibble'
    updateAceExtDef(newPtDefs, sender=sender)
  }
})

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
  cat('writeNOpen::\n')
  # 1. is saved? 
  #   if not save (as Rmd) (sender= "rmdView" )
  # 2. get is docFilePath (from where?)
  # 3. rmarkdown::render(input=docFilePath, all)
  # 4. html_file <- readLines(file.path(outputdir, outfile))
  # In javascript: 
  # var win = window.open();
  # win.document.write('<iframe width="560" height="315" src="//www.youtube.com/embed/mTWfqi3-3qU" frameborder="0" allowfullscreen></iframe>')
  #
  # or
  #
  # var iframe = document.createElement('iframe');
  # iframe.src = 'http://example.com';
  # document.body.appendChild(iframe);
  
  setTabRequest(sender='buttonCmd.rmdViewer', tabs=input$pages)
  # updateAceExt(
  #   id=getAceEditorId(),
  #   sender='buttonCmd.rmdViewer',
  #   getDoc=TRUE
  # )
  
  
  # sender='cmd.commit'
      # triggerRefresh(sender, rollBack=FALSE)
})



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
