

observeEvent(input$messageFromAce, {
  cat('\n====serverAce:...observe input$messageFromAce:: entering\n')
  # cat('\n initial value of getTibRow()=', format(getTibRow()), "\n")
    if(
      length(input$messageFromAce$code)>0 &&
      length(input$messageFromAce$sender)>0
    ){
      request$code<-input$messageFromAce$code
      sender<-input$messageFromAce$sender
      request$sender<-sender
      clearErrorMssg()
      
      cat("input$messageFromAce$id=" , format(input$messageFromAce$id), "\n")
      if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code) ){
        reqSelector<-input$messageFromAce$selector
        updateSelected4Ace(reqSelector)
      }
      cat('request$sender=',format(request$sender),"\n")
      if(length(input$messageFromAce$isSaved)>0){ 
        aceId<-input$messageFromAce$id
        editOption$.saved <- input$messageFromAce$isSaved
        cat('\n--setting editOption$.saved --\n')
        cat("set editOption$.saved=",editOption$.saved,"\n")
      }
      # cat('22 ace request$sender=',format(request$sender),"\n")
      if(sender %in% c('cmd.tabChange', 'cmd.file.new', 'cmd.openFileNow')){
        #browser()
        request$mode<-input$messageFromAce$mode
        cat('Ace:: request$mode=', request$mode, '\n')
        if(identical(request$mode, 'ptrrmd')){
          panels$sourceType<-rmdPanelTag
          processCommit()
          return(NULL) #skip the rest of the processing (for ptR mode)
        } 
        if(identical(request$mode, 'markdown')){
          panels$sourceType<-rmdPanelTag
          processCommit()
          return(NULL) #skip the rest of the processing (for ptR mode)
        } 
        if(identical(request$mode, 'text')){
          panels$sourceType<-textPanelTag
          processCommit()
          return(NULL) #skip the rest of the processing (for ptR mode)
        } 
        if(identical(request$mode, 'snippets')){
          panels$sourceType<-snippetPanelTag
          processCommit()
          return(NULL) #skip the rest of the processing (for ptR mode)
        } 
      }
      # From this point on, all processing assumes ptR mode
      if(sender %in% c('cmd.commit', 'cmd.add.column', 'cmd.add.asset', 'cmd.openFileNow', 'cmd.saveFileNow', 'cmd.file.new', 'cmd.tabChange')){
        # cat('33 request$sender=',format(request$sender),"\n")
        #cat('Ace: invoking processCommit\n')
        processCommit() # this sets the sourceType
        # cat('returning from processCommit\n')
        # cat('getTibName()=',format(getTibName()),"\n")
        if(sender %in% c('cmd.commit', 'cmd.add.column', 'cmd.add.asset', 'cmd.saveFileNow') && !is.null(getTibName())){ 
          if(sender=='cmd.add.asset'){
            name=input$messageFromAce$selector$assetName
          } else {
            name=getTibName() # 'cmd.commit', 'cmd.add.column'
          }
          tibs<-getPtDefs()$tib
          # cat('name=',format(name),"\n")
          # cat("ace invoking resetSelectedTibbleName\n")
          resetSelectedTibbleName(tibs=tibs, name=name)
        } else { 
          #name=NULL #  'cmd.openFileNow', 'cmd.tabChange' , 'cmd.file.new'
          cat('else: ', 'cmd.openFileNow', 'cmd.tabChange' , 'cmd.file.new',"\n")
          cat('sender=',format(sender),"\n")
          cat("length(input$pages)=",length(input$pages),"\n")
          tttid<-input$pages;
          cat("input$pages=",format(tttid),"\n")
          if(length(input$pages) >0 && 
             nchar(input$pages)>0 && 
             selectedTibble$tabId != input$pages ){
            
             cat('next tabId=',input$pages,"\n")
             cat('selectedTibble$tabId=',selectedTibble$tabId,"\n")
             cat("input$messageFromAce$id=" , format(input$messageFromAce$id), "\n")
             storeAssetState()
             restoreAssetState(input$pages)
            # choices<-getRightPanelChoices()
            # cat("getRightPanelChoices()=",format(getRightPanelChoices()),"\n" )
            # if(length(choices)>0 && length(selectedTibble$tabId)>0  && selectedTibble$tabId!='whatthefuck'){
            #   # cat( "store( tabId=",selectedTibble$tabId,")\n")
            #   tmp2<-isolate(reactiveValuesToList(selectedTibble, all.names=TRUE))
            #   #browser()
            #   tmp2[sapply(tmp2,is.null)]<-NA
            #   #plotSelect.tib<-rbind(plotSelect.tib, tmp )
            #   tmp1<-filter(plot$selections.tib, tabId!=selectedTibble$tabId)
            #    cat("serverAce::  plot$selections.tib<-bind_rows(tmp1, tmp2)\n")
            #    
            #   cat("tmp2=",format(tmp2),"\n")
            #   plot$selections.tib<-bind_rows(tmp1, as.tibble(tmp2))
            #    cat("========   ",paste(plot$selections.tib$tabId,collapse=", "),"\n")
            # }
            # row.tib<-filter(plot$selections.tib, tabId==input$pages)
            # cat("nrow(row.tib)=",nrow(row.tib),"\n")
            # print(row.tib)
            # if(nrow(row.tib)==0){
            #   cat('creating new tib for tabId=', input$pages,"\n")
            #   cat('choices=',format(choices),"\n")
            #   cat('columns=',format(names(getPtDefs()$tib )), "\n")
            #   row.tib<-newPlotSel(tabId=input$pages, choices=choices, tibs=getPtDefs()$tib)
            # }
            # 
            # # cat( "copy *row.tib* to *selectedTibble.*\n"  )
            # if(!is.null(row.tib)){
            #   lapply(names(row.tib), function(n){
            #     v<-row.tib[[n]][1]
            #     cat("row.tib$", n, "=", format(v),"\n")
            #     selectedTibble[[n]]<-ifelse(is.na(v), NULL, v)
            #   } )
            # }
            
          }
          # end selectTibUpdate:
          
        }
        
      } 
      
      if(sender %in% c( 'fileCmd.save', 'fileCmd.close', 'fileCmd.saveAs', 'fileCmd.quit' , 'fileCmd.saveNow')){
        
        id<-input$messageFromAce$id
        saved<-input$messageFromAce$isSaved
        # if(TRUE){
        #   cat("\n------",sender,"-----------------------------------\n")
        #   cat('input$messageFromAce$saved=',format(saved),"\n")
        #   cat('class(saved)=',class(saved),"\n")
        #   cat('saved=',saved,"\n")
        #   cat("and editOption$.saved=",editOption$.saved,"\n")
        #   cat("for id=",id,"\n")
        # }
        
        if( !saved || sender %in% c('fileCmd.saveAs','fileCmd.saveNow') ) { #need to save
          docFilePath<-unlist(input$messageFromAce$docFilePath)
          # if(TRUE){
          #   cat("docFilePath=",docFilePath,"\n")
          #   cat("sender=",sender,"\n")
          # }
          if(docFilePath=='?' || sender=='fileCmd.saveAs'){ # file unnamed : fileSaveAs
             tabId<-aceID2TabID(id)
             # cat('11: tabId=',format(tabId),"\n")
             # cat("executing sendPtRManagerMessage( id=id,  sender=sender, saveFile=TRUE,  type='R', tabId=tabId)\n")
             
             if(identical(request$mode, 'ptr')){
               ext=list(R='R')
             } else if( identical(request$mode, 'ptrrmd') ){
               ext=list(Rmd='Rmd')
             } else if( identical(request$mode, 'snippets') ){
               ext=list(snippet='snippets')
             } else if( identical(request$mode, 'markdown') ){
                 ext=list(dnippets='dnippets')
             } else {
               cat('request$mode=',request$mode,"\n")
               stop('unknowMode')
               ext=list(text='txt')
             }
             ext<-shinyFiles:::formatFiletype(ext)

             target<-saveButtonFileNames[[request$mode]]
             # cat('sendPtRManagerMessage( sender=',sender,', saveFile=TRUE,',  'target=',target, ', tabId=',tabId,')\n'  )
             sendPtRManagerMessage( sender=sender, saveFile=TRUE,  target=target, tabId=tabId )
          } else { 
            # write file
            # cat('serverAce:: writeLines\n')
            # cat('serverAce::docFilePath=',format(docFilePath),"\n")
            code<-input$messageFromAce$code
            # !!!TODO!!! if write fails revert.
            writeLines(code, docFilePath)
            
            updateAceExt(id, sender,  setDocFileSaved=TRUE)
            editOption$.saved<-TRUE
            tabId<-popTab()
            if(request$sender %in% c('fileCmd.close', 'fileCmd.quit')){
              addToRecentFiles(input$messageFromAce$docFilePath)
              closeTabNow(tabId)
            } else { 
              addToRecentFiles(input$messageFromAce$priorFilePath)
              title=as.character(tabTitleRfn( tabName=basename(docFilePath ), tabId=tabId, docFilePath=docFilePath ))
              sendFileTabsMessage(title=title, tabId=tabId)
            }
          }
        } else { #already saved
          tabId<-popTab()
          if(request$sender%in% c('fileCmd.close', 'fileCmd.quit') ){
            addToRecentFiles(input$messageFromAce$docFilePath)
            closeTabNow(tabId)
          }
        }
      }
      # cat('\n final value of getTibRow()=', format(getTibRow()), "\n")
    }
}, priority = 90, ignoreNULL = TRUE, ignoreInit = TRUE)

updateAceExtDef<-function(newPtDef, sender, selector=list() ){
  if(!is.null(getCode())){
    # cat("newPtDef:\n")
    # print(newPtDef)
    # cat("getCode()\n")
    # print(getCode())
    newPtDef$tib<-pts2Integers(newPtDef$tib )
    
    replacementList<-ptDef2ReplacementList(name, newPtDef, getCode() ) #name not used!!!
    if( length(replacementList)>0 ){
      data<-list(id= getAceEditorId(), replacement=replacementList, selector=selector, sender=sender, ok=1)
      lapply(data, function(x){
        if(any(unlist(lapply(x, is.na )))){
          print(data)
          stop("encounterd an NA")
        }
      })
      session$sendCustomMessage(
        type = "shinyAceExt",
        #list(id= getAceEditorId(), replacement=replacementList, selector=selector, sender=sender, ok=1)
        data
      )
    }
  }

}

updateAceExt<-function(id, sender, ... ){
  data<-list(...)
  if(is.null(sender)){stop('null sender')}
  if(length(data)>0){
    if(length(id)==0){
      id<-'bogus'
    }
    data<-c(list(id= id, sender=sender), data )
    if(TRUE){
        cat("Entering updateAceExt::\n")
        cat("updateAceExt::id=",format(id), ", length(id)=",length(id),",class(id)=", class(id),"\n")
        cat("updateAceExt::sender=",format(sender),"\n")
        cat("updateAceExt::names(data)=",format(names(data)),"\n")
        cat("updateAceExt::data=",format(data),"\n")
        #print(data)
        cat("updateAceExt::sendCustomMessage NOW\n")
    }


    
    if(length(id)>0 && nchar(id)>0){
      lapply(data, function(d){
        if(length(d)==0){
          cat("-----------\n")
          print(data)
          stop('d has length 0')
        }
        if(length(d)==1 && is.na(d)){
          print(data)
          stop("encounterd an NA")
        }
      })
      session$sendCustomMessage(
        type = "shinyAceExt",
        data
      )
    }
    
  }
}

observeEvent(request$sender,{
    if(request$sender=='startup'){
      #cat('startup\n')
      cmdFileNewPtR()
    }
  # if(request$sender=='startup'){
  #   cat('startup\n')
  #   tabId=getNextAnonymousFileName()
  #   # txt=''
  #   src<-codeTemplate
  #   addFileTab(tabId,codeTemplate)
  #   # addFileTab(tabId, txt)
  #   
  #   cat('fin addFileTab\n')
  #   cat('sendCustomMessage sender=  cmd.file.new\n')
  #   # session$sendCustomMessage(
  #   #   type = "shinyAceExt",
  #   #   list(id= tabId, sender='cmd.file.new', setValue= src, ok=TRUE)
  #   # )
  #   triggerRefresh('cmd.commit', rollBack=FALSE)
  # 
  #   mssg$error<-""
  #}
  
}, priority=100)

# TODO!!!: rewrite
updateSelected4Ace<-function( reqSelector){
  
  if(!is.null(reqSelector[['name']])){
    #cat("reqSelector$name=", format(reqSelector$name ),"\n")
    selectedTibble$name=reqSelector[['name']]
  }
  if(!is.null(reqSelector[['ptColName']])){
    #cat("reqSelector$ptColName=", format(reqSelector$ptColName ),"\n")
    selectedTibble$ptColName=reqSelector[['ptColName']]
  }
  if(!is.null(reqSelector[['rowIndex']])){ # !!! may want to provide a check here
    #cat("reqSelector$ptColName=", format(reqSelector$rowIndex ),"\n")
    selectedTibble$rowIndex=reqSelector[['rowIndex']]
  }
  if(!is.null(reqSelector[['matCol']])){
    #cat("reqSelector$matCol=", format(reqSelector$matCol ),"\n")
    selectedTibble$matCol=reqSelector[['matCol']]

  }
  if(!is.null(reqSelector[['columnName']])){
    #cat("reqSelector$columnName=", format(reqSelector$columnName ),"\n")
    selectedTibble$columnName=reqSelector[['columnName']]
  }
} 

