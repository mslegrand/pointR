
restoreWorkSpace<-function( workSpaceDir=getWorkSpaceDir(), pprjPath=getProjectFullPath() ){
  # cat('>---> restoreWorkSpace\n')
  
  fileWSPaths<-dir(workSpaceDir, pattern='PTR-TABID', full.names = T)
  if(length(fileWSPaths)==0){
    # cat("workSpaceDir = ",format(workSpaceDir), "\n")
    return(FALSE)
  }
  wsPages<-list()
  

  
  # 1. load all pages into a list.
  for(filePath in fileWSPaths){
    page<-readRDS(filePath)
    # A. assign tabIds to each page
    # id=getNextTabId()
    id=basename(filePath)
    # # substitute value ending in tabId with id
    # tbid<-grep("tabId$", names(page))
    # page[tbid]<-id
    wsPages[[id]]<-page
  } 
  # load project.pprj
 
  # if(length(pprjPath)>0){
  #   pprj<-read_json(pprjPath)
  #   if(!identical(pprj$pathToProj, getWorkSpaceDir())){
  #     for(page in wsPages){
  #       fp<-page$fileDescriptor.filePath
  #       page$fileDescriptor.filePath<-gsub(getWorkSpaceDir(), pprj$pathToProj, fp)
  #     }
  #   }
  # }
  # # 2 remove filePath file
  # for(filePath in fileWSPaths){
  #   file.remove(filePath)
  # }
  # 
  # # 3 write pages
  # for(id in names(wsPages)){
  #   newFilePath<-paste0(workSpaceDir,"/",id,".rda")
  #   page<-wsPages[[id]]
  #   saveRDS(object=page, file = newFilePath)
  # }
  
  #4. iterate through pages and add to serverAssetDB$tib
  
  # # --- serverAssetDB
  # tib<-NULL #serverAssetDB$tib
  # pattern<-"^assetSelection."
  # for(page in wsPages){
  #   tibAs<-page[ grep(pattern, names(page)) ]
  #   names(tibAs)<-gsub(pattern, '', names(tibAs))
  #   tibAs[sapply(tibAs,is.null)]<-NA
  #   tryCatch({tib<-bind_rows(tib, tibAs)}, error=function(e){
  #     # if(inherits( tib, 'tbl_df')){ 
  #     #   cat('glimpse\n')
  #     #   glimpse(tib )
  #     #   cat("ncol(tib)=",ncol(tib),"\n")
  #     #   #cat(names('names(tib)=', paste(names(tib),collapse=","), "\n"))
  #     #   cat("nrow(tib)=",nrow(tib),"\n")
  #     # } else { print(format(tib))}
  #     # cat("class(tibAs) =" , class(tibAs), "\n")
  #     # print('tibAs=')
  #     # print(format(tibAs))
  #     # cat("names(tibAs)=",names(tibAs),"\n")
  #     e<-c(e,"\n", traceback())
  #     stop(e)
  #   })
  # }
  # serverAssetDB$tib<-tib
  # 
  # # --- backDropDB
  # tib<-backDropDB()
  # pattern<-"^backdrop."
  # for(page in wsPages){
  #   tibAs<-page[ grep(pattern, names(page)) ]
  #   names(tibAs)<-gsub(pattern, '', names(tibAs))
  #   tryCatch({tib<-bind_rows(tib, tibAs)}, error=function(e){
  #     print(pattern)
  #     print(format(tib))
  #     print(format(tibAs))
  #     e<-c(e,traceback())
  #     stop(e)
  #   })
  # }
  # backDropDB(tib)
  # 
  # # ---grid---
  # tib<-svgGridDB()
  # pattern<-"^grid."
  # for(page in wsPages){
  #   tibAs<-page[ grep(pattern, names(page)) ]
  #   names(tibAs)<-gsub(pattern, '', names(tibAs))
  #   tryCatch({tib<-bind_rows(tib, tibAs)}, error=function(e){
  #     print(pattern)
  #     print(format(tib))
  #     print(format(tibAs))
  #     e<-c(e,traceback())
  #     stop(e)
  #   })
  # }
  # svgGridDB(tib)
  # 
  # #--- tribbleDB
  # tib<-useTribbleFormatDB()
  # pattern<-"^trib."
  # for(page in wsPages){
  #   tibAs<-page[ grep(pattern, names(page)) ]
  #   names(tibAs)<-gsub(pattern, '', names(tibAs))
  #   tryCatch({tib<-bind_rows(tib, tibAs)}, error=function(e){
  #     print(pattern)
  #     print(format(tib))
  #     print(format(tibAs))
  #     e<-c(e,traceback())
  #     stop(e)
  #   })
  # }
  # useTribbleFormatDB(tib)
  # 
  # #--- dnippetsDB
  # tib<-dnippetsDB$usage
  # pattern<-"^dnip."
  # for(page in wsPages){
  #   tibAs<-page[ grep(pattern, names(page)) ]
  #   names(tibAs)<-gsub(pattern, '', names(tibAs))
  #   tryCatch({tib<-bind_rows(tib, tibAs)}, error=function(e){
  #     print(pattern)
  #     print(format(tib))
  #     print(format(tibAs))
  #     e<-c(e,traceback())
  #     stop(e)
  #   })
  # }
  # dnippetsDB$usage<-tib
  # 
  # # --- preProcDB
  # tib<-preProcDB$points
  # pattern<-"^preprocScripts."
  # for(page in wsPages){
  #   # extract the serverAsset portion and add
  #   asi<-grep(pattern, names(page))
  #   if(length(asi)>0){
  #     tibAs<-page[asi]
  #     tn<-gsub(pattern, '', names(tibAs))
  #     names(tibAs)<-tn
  #     tryCatch({tib<-bind_rows(tib, tibAs)}, error=function(e){
  #       print(pattern)
  #       print(format(tib))
  #       print(format(tibAs))
  #       e<-c(e,traceback())
  #       stop(e)
  #     })
  #   }
  # }
  # preProcDB$points<-tib
  # 
  # # ------- fileDescDB
  # tib<-fileDescDB()
  # pattern<-"^fileDescriptor."
  # for(page in wsPages){
  #   # extract the serverAsset portion and add
  #   asi<-grep(pattern, names(page))
  #   if(length(asi)>0){
  #     tibAs<-page[asi]
  #     tn<-gsub(pattern, '', names(tibAs))
  #     names(tibAs)<-tn
  #     tryCatch({tib<-bind_rows(tib, tibAs)}, error=function(e){
  #       print(pattern)
  #       print(format(tib))
  #       print(format(tibAs))
  #       e<-c(e,"\n",traceback())
  #       stop(e)
  #     })
  #   }
  # }
  # fileDescDB(tib)
  
  
  extractDBFromPages<-function(wsPages, pattern, initTib){
    for(page in wsPages){
      tibAs<-page[ grep(pattern, names(page)) ]
      if(length(tibAs)>0){
        names(tibAs)<-gsub(pattern, '', names(tibAs))
        tibAs[sapply(tibAs,is.null)]<-NA
        tryCatch({initTib<-bind_rows(initTib, tibAs)}, error=function(e){
          stop('failure with pattern=',pattern, "and page=\n")
        })
      }
    }
    return(initTib)
  }  
 
  serverAssetDB$tib<-extractDBFromPages(wsPages, "^assetSelection.", initTib=initialServerAsset() )
  
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
  
  
  
  
  for(page in wsPages){
    # extract the serverAsset portion and add
    tabId=page$fileDescriptor.tabId
    
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
      inputId = "pages",
      tabPanel( #tabId,
        title<-tabTitleRfn(title, tabId, docFilePath), # maybe we should save title in fileDescriptor?
        div(
          class=divClass,
          overflow= "hidden",inline=FALSE,
          shinyAce4Ptr(
            outputId = aceId,
            value=txt,
            mode=mode,
            theme=defaultOpts["theme"],
            fontSize=defaultOpts["fontSize"], autoComplete="enabled",
            if(mode=='ptR')
              autoCompleteList =list(names(svgR:::eleDefs))
            else
              NULL
            ,
            docFilePath=docFilePath,
            initSaved=fileSaveStatus
          )
        ),
        value=tabId
      )
    )
    if(!(tabId %in% serverAssetDB$tib$tabId) ){
      browser()
      stop('tabId=', tabId, 'not found in serverAssetDB$tib$tabId')
    }
    restoreAssetState(tabId)
    updateTabsetPanel(session, inputId='pages', selected=tabId)
  }
  delay(500,{
    for(page in wsPages){
      
      tabId=page$fileDescriptor.tabId
      fileSaveStatus=page$fileDescriptor.isSaved 
      savedStatus<-ifelse(fileSaveStatus, 'saved', 'notSaved')
      
      if(is.null(tabId)){ browser() }
      addNewPage2dnippetsDB(tabId)
      sendFileTabsMessage(resize=runif(1), tabId=tabId, savedStatus= savedStatus)
    }
  })
  # cat('<---< restoreWorkSpace\n')
  return(TRUE)
  
}

