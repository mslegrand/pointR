
UIProjectTemplateMenu<-function(){
  projTemplateNames<-dir(filePath(ptRPath, "App","projectTemplates"))
  
  tmp<-lapply(projTemplateNames, function(prjname){
     shinyDMDMenu::menuItem(prjname, value=paste0('projectTemplate-',prjname))
  })
   do.call(tagList, tmp)
}