cmdChoiceSetImport<-function(){
  click('buttonChoiceSetImport')
}

observeEvent(input$buttonChoiceSetImport,{
  fp.dt<-parseFilePaths(c(home='~'), input$buttonChoiceSetImport)
  if(length(fp.dt)>0 && nrow(fp.dt)){
    datapath<-as.character(fp.dt$datapath[1])
    datapath<-gsub(pattern = '^NA/', "~/", datapath)
    # copy to aux
    # get targetDir
    targetDir<-getAuxChoicesPath()
    if(!file.exists(targetDir)){ # we should probably place this elsewhere !!!
      dir.create(targetDir, recursive=TRUE)
    }
    
    targetName<-basename(datapath)
    
    target<-file.path(targetDir,targetName )
    
    # file copy
    file.copy(datapath, target, TRUE)
    # relaod
    readAuxChoices(targetDir)
  }
})

