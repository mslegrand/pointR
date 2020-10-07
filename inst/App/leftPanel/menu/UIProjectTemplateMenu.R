
UIProjectTemplateMenu<-function(){
  projTemplatePaths<-list.dirs(
    file.path(ptRPath, "App","projectTemplates"), full.names = TRUE, recursive = FALSE
  )
  userTemplatePaths<-list.dirs(
    file.path(homeDir, '.ptR','.templates'), full.names = TRUE, recursive = FALSE
  )
  templatePaths<-c(projTemplatePaths,userTemplatePaths)
  templateNames<-basename(templatePaths)
  templatePaths<-paste0('projectTemplate-',templatePaths)
  tmp<-mapply(shinyDMDMenu::menuItem,   templateNames, value=templatePaths,SIMPLIFY=FALSE)
   do.call(tagList, tmp)
  
}



UIRemoveUserTemplate<-function(){
  userTemplatePaths<-list.dirs(
    file.path(homeDir, '.ptR','.templates'), full.names = TRUE, recursive = FALSE
  )
  if(length(userTemplatePaths)==0){
    NULL
  } else {
    userTemplateNames<-basename(userTemplatePaths)
    userTemplatePaths<-paste0('removeTemplate-',userTemplatePaths)
    tmp<-mapply(shinyDMDMenu::menuItem,   userTemplateNames, value=userTemplatePaths, SIMPLIFY=FALSE)
    do.call(tagList, tmp)
  }
}


updateNewProjectMenu<-function(session){
  # 1 remove menuDropdown
  removeDMDM(session=session, menuBarId="editNavBar", entry='New Project')
  #recreate dropdow
  submenu<-menuDropdown('New Project',
               shinyDMDMenu::menuItem('New Basic Project', value='newBasicProject'),
               shinyDMDMenu::menuItem('Clone of Existing Project', value='newCloneProject'),
               # shinyDMDMenu::menuItem('svgR-based ShinyInput Control', value='newSimpleInputWidget'),
               UIProjectTemplateMenu()
  ) 
  # 4 insertsubmenu
  if(!is.null(submenu)){
    insertAfterDMDM(
      session, menuBarId = "editNavBar", 
      entry="New File", submenu= submenu
    )
  }
}
updateRemoveTemplateMenu<-function(session){
  # 1 remove menuDropdown
  removeDMDM(session=session, menuBarId="editNavBar", entry="Remove from Menu")
  #recreate dropdown
  templateMenuList<-UIRemoveUserTemplate()
  submenu<-menuDropdown("Remove from Menu",
                        templateMenuList
  ) 
  # 4 insertsubmenu
  #if(!is.null(UIRemoveUserTemplate())){
    insertAfterDMDM(
      session, menuBarId = "editNavBar", 
      entry="addTemplate", submenu= submenu
    )
  #}
  if(is.null(templateMenuList)){
    disableDMDM(session, menuBarId="editNavBar", entry="Remove from Menu")
  }
}
