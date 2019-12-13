
UIProjectSampleMenu<-function(){
  projTemplateNames<-dir(filePath(ptRPath, "App","sampleProjects"))
  
  tmp<-lapply(projTemplateNames, function(prjname){
    shinyDMDMenu::menuItem(prjname, value=paste0('projectSample-',prjname))
  })
  do.call(tagList, tmp)
}