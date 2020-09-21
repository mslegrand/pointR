preProcScriptDB<-reactiveValues(
  points=tibble( scriptName='bogus', cmd="bogus", script='bogus')[0,],
  attrs=tibble(  scriptName='bogus', cmd="bogus", script='bogus')[0,]
)

observeEvent(nrow(preProcScriptDB$points),{
  if(nrow(preProcScriptDB$points)>0){
       enableDMDM(session, 'plotNavBar','Edit preproc points')
  } else{
      disableDMDM(session, 'plotNavBar','Edit preproc points')
  }
})

observeEvent(nrow(preProcScriptDB$attrs),{
  if(nrow(preProcScriptDB$attrs)>0){
    enableDMDM(session, 'plotNavBar','Edit preproc attrs')
  } else{
    disableDMDM(session, 'plotNavBar','Edit preproc attrs')
  }
})

loadAuxPreProc<-function(fullName){
  extractBodyWithComments<-function(fn){
    tt<-capture.output(print(fn))
    #remove blank lines
    blanks1<-grepl('^ *$',tt)
    blanks2<-c(blanks1[-1], FALSE)
    bad<-blanks1 #& blanks2
    tt<-tt[!bad]
    #drop function beginning
    begPos<-min(grep('\\{',tt)) #todo handle case where { not found
    if(grepl( "\\{\\s*$", tt[begPos])){
      begPos<-begPos+1
    } else {
      bPos<-1+min(unlist(gregexpr('\\{',tt[begPos])))
      tt[begPos]<-substring(tt[begPos],nn)
    }
    # begPos
    #drop function ending
    endPos<-max(grep('\\}',tt)) #todo handle case where } not found
    if(grepl( "\\S+\\s*\\}\\s*$", tt[endPos])){
      ePos<- -1+max(unlist(gregexpr('\\}',tt[endPos])))
      tt[endPos]<-substring(tt[endPos],ePos)
    } else {
      endPos<-endPos-1
    }
    # endPos
    tt<-tt[begPos:endPos]
    
    #remove indents
    nn<-min(unlist(gregexpr('\\S+',tt)))
    
    tt<-substring(tt,nn)
    
    tt<-paste(tt, collapse="\n")
    
    tt
  }
  
  tryCatch({
    preProcList<-source(fullName, local=T)$value
    #check preProcList
    if(is.null(preProcList) ||  
       any(match(names(preProcList), unlist(preprocChoices)   , 0 )==0)
       
    ){
      stop('ill-formed  preprocessor')
      # todo better message
    }
    
    ppscripts<-lapply(preProcList, extractBodyWithComments)
    
    script.Name=sub('\\.R$','',basename(fullName))
    tb<-tibble(scriptName=script.Name, cmd=names(preProcList), script=ppscripts)
   
    if( "preprocPts"== basename(dirname(fullName))){
      preProcScriptDB$points<-filter(preProcScriptDB$points, scriptName!=script.Name)
      preProcScriptDB$points<-rbind(preProcScriptDB$points, tb)
    } else if( "preprocAts"==basename(dirname(fullName))){
      preProcScriptDB$attrs<-filter(preProcScriptDB$attrs, scriptName!=script.Name)
      preProcScriptDB$attrs<-rbind(preProcScriptDB$attrs, tb)
    }
  }, 
  error=function(e){
    e<-c(e,traceback())
    err<-paste(unlist(e), collapse="\n", sep="\n")
    alert(err)
  })
}

reloadPreProcScriptDB<-function(dirPath, scriptType='points'){
  ppfiles<-dir(dirPath, full.names=TRUE)
  for(fullName in ppfiles ){
    readAuxPreProc(fullName)
  }
}


getPreProcPPAuxPath<-reactive({file.path(getDirPath(),'aux','preprocPts')})   
getPreProcPAAuxPath<-reactive({file.path(getDirPath(),'aux','preprocAts')})   

clearPreProcEditMenu<-function(type='points'){
  entry=paste0('Edit preproc ',type)
  removeDMDM(session=session, menuBarId="plotNavBar", entry=entry, type="dropdown")
}

populatePreProcEditMenu<-function(type=points){
  sn<-trimws(unique(preProcScriptDB[[type]]$scriptName))
  kids<-lapply(sn, function(nn){
    shinyDMDMenu::menuItem(nn, value=paste0('editPP-',type,'-',nn))
  })
  idd=trimws(paste0('dropDown-editPreProc-',type))
  afterEntry=ifelse(type=='points', 'cmdNewPP', 'cmdNewAP')
  label=paste0('Edit preproc ',type)
  shinyDMDMenu::insertAfterDMDM(
    session, 
    menuBarId  ="plotNavBar",  
    entry=afterEntry,
    submenu=
      do.call(
        function(...){ menuDropdown( label,...) },
        kids
      )
  )
}

readAuxPreProcs<-function( startup=TRUE){
  preProcFilePaths<-c(
    list.files(getPreProcPPAuxPath(), full.names=TRUE),
    list.files(getPreProcPAAuxPath(), full.names=TRUE)
  ) 
  # clear the menus
  clearPreProcEditMenu('points')
  clearPreProcEditMenu('attrs')
  
  for(fp in preProcFilePaths){
    loadAuxPreProc(fp)
  }
  
  populatePreProcEditMenu('points')
  populatePreProcEditMenu('attrs')
  # repopulate the menu
}



getPreProcChoices<-reactive({
  rtv<-c() 
  if( getTibEditState()==TRUE && !is.null(getColumnType())){
    if(getColumnType()=='point'){
      rtv<-unique(preProcScriptDB$points$scriptName)
    } else {
      rtv<-unique(preProcScriptDB$attrs$scriptName)
    }
  }
  rtv
})

hasPreProcChoices<-reactive({ length(getPreProcChoices())>0})

observeEvent(input$preProcDropDown, {
  choices=getPreProcChoices()
  if(length(choices)>0){
    choices<-c('none', getPreProcChoices())
    tab_Id=selectedAsset$tabId
    tib_Name=selectedAsset$name
    column_Name=selectedAsset$columnName
    selected<-getPreProcScriptName(tab_Id, tib_Name, column_Name)
  } else {
    choices<-'none'
    selected<-'none'  
  }
  updateRadioButtons(session, "preProcChooser", choices=choices, selected=selected, )
})

writeAuxPreprocList<-function(filePath, scripts){
  txt0<-paste0(names(scripts),'= function( pt, context, WH, keys){\n',scripts,"\n}", collapse=",\n")
  unlist(str_split(txt0, '\n'))->lines
  paste0("  ", lines,collapse="\n")->txt1
  txt<-paste0('preprocList<-list(\n', txt1, "\n)")
  writeLines(txt, filePath)
}
