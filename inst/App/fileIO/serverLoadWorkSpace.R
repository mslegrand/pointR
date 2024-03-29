
syncDndDB<-function(usageDB){
  dndNames<-getDnippetsAll()
  usageNames<-names(usageDB)
  usageNames<-usageNames[usageNames!='tabId']
  for(sname in usageNames) {
    if(!sname %in% dndNames){ #remove col if usagename not in current dnds dir
      usageDB<-select(usageDB, -sname)
    }
  }
  for(dndName in dndNames) {#
    if(!dndName %in% names(usageDB)){ #not in  usageDB
      usageDB<-add_column(usageDB, !!dndName:=FALSE)
    }
  }
  usageDB
}


restoreWorkSpace<-reactive({
  log.fin(restoreWorkSpace)
  workSpaceDir=getWorkSpaceDir()
  prjPath=getProjectFullPath()
  fileWSPaths<-dir(workSpaceDir, pattern='PTR-TABID', full.names = T)
  if(length(fileWSPaths)==0){
    return(NULL)
  }
  wsPages<-list()
  
  ptRproj<-pprj()
  
  selectedTab<-readCurrentTab()
  tabs=c()
  
  # 1. load all pages into a list.
  for(filePath in fileWSPaths){
    
    page<-readRDS(filePath)
    # A. assign tabIds to each page
    id=basename(filePath)
    if(!is.null(ptRproj$pathToProj)){
      docFilePath=page$fileDescriptor.filePath
      page$fileDescriptor.filePath<-sub( ptRproj$pathToProj, editOption$currentProjectDirectory, docFilePath)
      if(!file.exists(page$fileDescriptor.filePath)){
        next # omit this, else saving will cause an error
      }
      saveRDS(page,filePath)
    }
    wsPages[[id]]<-page
  }
  
 
   extractDBFromPages<-function(wsPages, pattern, initTib){
    rtv<-lapply(wsPages, function(page){
      tibAs<-page[ grep(pattern, names(page)) ]
      if(length(tibAs)>0){
        names(tibAs)<-gsub(pattern, '', names(tibAs))
        tibAs[sapply(tibAs,is.null)]<-NA
        setdiff(names(initTib),names(tibAs))->tmp
        if(length(tmp)>0){
          tmp<-sapply(tmp, function(x){NA}, USE.NAMES = T)
          tibAs<-c(tibAs,tmp)
        }
      }
      tibAs
    })
    rtv<-bind_rows( rtv)
    if(ncol(rtv)==0){
      rtv<-initTib
    }
    rtv
  }  
  

  tabId<-'bogus'
  mode<-'ptr'
  txt<-NULL
  aceId<-'bogus'
  
  
  for(page in wsPages){
    # extract the serverAsset portion and add
    
    
    tabId=page$fileDescriptor.tabId
    tabs<-c(tabs,tabId)
    # if(identical(page$isSelected, TRUE)){
    #   selectedTab=tabId
    # }
    # cat('selectTab=',format(selectedTab),"\n")
    mode=page$fileDescriptor.mode
    docFilePath=page$fileDescriptor.filePath
    
    
    fileSaveStatus=page$fileDescriptor.isSaved
    
    txt=page$code
    
    if(identical(fileSaveStatus,TRUE) && file.exists(docFilePath)){ 
      tryCatch(
        {txt<-paste(readLines(docFilePath), collapse="\n")},
        error=function(e){
          e<-e$message
          cat("Unable to read file:", paste(e, collapse="\n"))
          return(NULL) #bail
        }
      )
    } 
     
    if(!identical(docFilePath, "?")){
      title=basename(docFilePath)
    } else {
      title=paste('Anonymous', page$fileDescriptor.anonNo)
    }
    
    aceId<-newPage(
      tabId=tabId, title=title, txt=txt,
      docFilePath=docFilePath, mode=mode,
      fileSaveStatus=fileSaveStatus
    )
  }
  
  serverAssetDB$tib<-extractDBFromPages(wsPages, "^assetSelection.", initTib=initialServerAssetDB() )

  tib<-extractDBFromPages(wsPages, "^backdrop.", initTib=initialBackDropDB() )
  backDropDB(tib)

  tib<-extractDBFromPages(wsPages, "^grid.", initTib=initialSvgGridDB()  )
  svgGridDB(tib)

  tib<-extractDBFromPages(wsPages, "^trib.", initTib=initialTribbleDB())
  useTribbleFormatDB(tib)
  
  tib<-extractDBFromPages(wsPages, "^widg.", initTib=initialWidgetDB() )
  widgetDB(tib)
  
  

  usageDB<-extractDBFromPages(wsPages, "^dnip.", initTib=tibble(tabId='bogus')[0,] )
  usageDB[is.na(usageDB)]<-FALSE
  usageDB<-syncDndDB(usageDB)
  dnippetsDB$usage<-usageDB
  
  tib<-extractDBFromPages(wsPages, "^preprocPage.", initTib=initialPreProcPageDB())
  preProcPageDB(tib)
  tib<-extractDBFromPages(wsPages, "^fileDescriptor.", initTib=initialFileDescDB() )
  fileDescDB(tib)  
  
  if(!is.null(ptRproj)){
    ptRproj$pathToProj<-editOption$currentProjectDirectory
    ptRproj$projName<-editOption$currentProjectName
    fullpathProjName=file.path(ptRproj$pathToProj, ptRproj$projName)
    write_json(ptRproj, fullpathProjName, pretty=4) 
  } 
  if(!is.null(selectedTab)){
    if(selectedTab %in% tabs ){
      updateTabsetPanel(session, "pages", selected = selectedTab)
      # aceId<-selectedTab
    }
  }
  
  
  log.fout(restoreWorkSpace)
  return(aceId)
})
  
# need add observer for aceId in pages??

