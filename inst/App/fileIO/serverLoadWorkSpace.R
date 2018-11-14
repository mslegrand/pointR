
#restoreWorkSpace<-function( workSpaceDir=getWorkSpaceDir(), pprjPath=getProjectFullPath(), session=getSession() ){
restoreWorkSpace<-reactive({
  cat('>---> restoreWorkSpace\n')
  # browser()
  workSpaceDir=getWorkSpaceDir()
  prjPath=getProjectFullPath()
  fileWSPaths<-dir(workSpaceDir, pattern='PTR-TABID', full.names = T)
  if(length(fileWSPaths)==0){
    cat("workSpaceDir = ",format(workSpaceDir), "\n")
    cat(" length(fileWSPaths)==0 \n")
    cat('<---< restoreWorkSpace\n')
    return(NULL)
  }
  wsPages<-list()
  

  
  # 1. load all pages into a list.
  for(filePath in fileWSPaths){
    page<-readRDS(filePath)
    # A. assign tabIds to each page
    id=basename(filePath)
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
  for(page in wsPages){
    # extract the serverAsset portion and add
    tabId=page$fileDescriptor.tabId
    cat('page$fileDescriptor.tabId=',format(page$fileDescriptor.tabId),"\n")
    aceId<-tabID2aceID(tabId)
    mode=page$fileDescriptor.mode
    docFilePath=page$fileDescriptor.filePath
    fileSaveStatus=page$fileDescriptor.isSaved 
    txt=page$code
    
    if(!identical(docFilePath, "?")){
      title=basename(docFilePath)
    } else {
      title=paste('Anonymous', page$fileDescriptor.anonNo)
    }
    
    if(mode=='ptr'){
      divClass="cAceContainer"
    } else {
      divClass="cAceRmdContainer"
    }
    
    appendTab(
      inputId = "pages", select=TRUE,
      tabPanel( 
        title=tabTitleRfn(title, tabId, docFilePath), # maybe we should save title in fileDescriptor?
        div(
          class=divClass,
          overflow= "hidden",inline=FALSE,
          shinyAce4Ptr(
            outputId = aceId,
            value    = txt,
            mode     = mode,
            theme    = defaultOpts["theme"],
            fontSize = defaultOpts["fontSize"], autoComplete="enabled",
            if(mode=='ptr'){
              autoCompleteList =list(names(svgR:::eleDefs))
            } else {
              NULL
            },
            docFilePath =docFilePath,
            initSaved   =fileSaveStatus
          )
        ),
        value=tabId
      )
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
  fileDescDB(tib)  
  
  cat("tabId=",tabId,"\n")
  cat('input$pages=',format(input$pages),"\n")
  

  cat('<---< restoreWorkSpace\n')
  return(aceId)
})
  
  # need add observer for aceId in pages??

