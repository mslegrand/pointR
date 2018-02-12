
output$TopLeftTabPanel<-renderUI({
  tmp<-getCurrentFile()
  if(nchar(tmp)==0){
    tmp<-'Unnamed'
  }
  tabsetPanel( tabPanel(tmp))
})

