
output$TopLeftTabPanel<-renderUI({
  tmp<-getCurrentFile()
  cat("getCurrentFile()=",format(getCurrentFile()),"\n")
  if(nchar(tmp)==0){
    tmp<-'Unnamed'
    cat("setting to Unnamed\n")
  }
  tabsetPanel( tabPanel(tmp))
})

