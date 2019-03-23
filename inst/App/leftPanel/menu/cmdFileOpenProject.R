

cmdFileOpenProject<-function(){
  click('buttonFileOpenProject')
}

observeEvent(input$buttonFileOpenProject,{
 
  fp.dt<-parseFilePaths(c(home='~'), input$buttonFileOpenProject)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    datapath<-gsub(pattern = '^NA/', "~/", datapath)
    pathToProj<-dirname(datapath) # converts the ~/ to /home/user/
    projName<-basename(datapath)
    openProj(projName, pathToProj)
  }
})


