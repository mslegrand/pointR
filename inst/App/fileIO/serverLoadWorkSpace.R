
#restoreWorkSpace<-function( workSpaceDir=getWorkSpaceDir(), pprjPath=getProjectFullPath(), session=getSession() ){
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
  
  # 1. load all pages into a list.
  for(filePath in fileWSPaths){
    page<-readRDS(filePath)
    # A. assign tabIds to each page
    id=basename(filePath)
    if(!is.null(ptRproj$pathToProj)){
      docFilePath=page$fileDescriptor.filePath
      page$fileDescriptor.filePath<-sub( ptRproj$pathToProj, editOption$currentProjectDirectory, docFilePath)
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
      }
      tibAs
    })
    rtv<-bind_rows( rtv)
    rtv
  }  
  

  tabId<-'bogus'
  mode<-'ptr'
  txt<-NULL
  aceId<-'bogus'
  for(page in wsPages){
    # extract the serverAsset portion and add
    
    tabId=page$fileDescriptor.tabId
    
    mode=page$fileDescriptor.mode
    docFilePath=page$fileDescriptor.filePath
    
    
    fileSaveStatus=page$fileDescriptor.isSaved
    txt=page$code
    if(fileSaveStatus==TRUE && file.exists(docFilePath)){ 
      tryCatch(
        {txt<-paste(readLines(docFilePath), collapse="\n")},
        error=function(e){
          cat("Unable to read file:", paste(e, collapse="\n"))
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

  dnippetsDB$usage<-extractDBFromPages(wsPages, "^dnip.", initTib=tibble(tabId='bogus')[0,] )

  preProcDB$points<-extractDBFromPages(wsPages, "^preprocScripts.", initTib=initialPreprocDB())

  tib<-extractDBFromPages(wsPages, "^fileDescriptor.", initTib=initialFileDescDB() )
  print(tib)
  fileDescDB(tib)  
  
  #ptRproj<-pprj()
  if(!is.null(ptRproj)){
    ptRproj$pathToProj<-editOption$currentProjectDirectory
    ptRproj$projName<-editOption$currentProjectName
    fullpathProjName=file.path(ptRproj$pathToProj, ptRproj$projName)
    write_json(ptRproj, fullpathProjName, pretty=4) 
  } 
  log.fout(restoreWorkSpace)
  return(aceId)
})
  
  # need add observer for aceId in pages??

