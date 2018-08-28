

cmdFileOpenProject<-function(){
  click('buttonFileOpenProject')
}

observeEvent(input$buttonFileOpenProject,{
  cat("observe input$buttonFileOpen:: enter\n")
  fp.dt<-parseFilePaths(c(wd='~'), input$buttonFileOpenProject)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    datapath<-gsub(pattern = '^NA/', "~/", datapath)
    pathToProj<-dirname(datapath)
    projName<-basename(datapath)
    openProj(projName, pathToProj)
  }
})


