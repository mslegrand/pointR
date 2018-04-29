

observeEvent(input$messageFromAce, {
  cat('\n====serverAce:...observe input$messageFromAce:: entering\n')
  cat('\n initial value of getTibRow()=', format(getTibRow()), "\n")
    if(
      length(input$messageFromAce$code)>0 &&
      length(input$messageFromAce$sender)>0
    ){
      request$code<-input$messageFromAce$code
      sender<-input$messageFromAce$sender
      request$sender<-sender
      clearErrorMssg()
      
      # cat("input$messageFromAce$id=" , format(input$messageFromAce$id), "\n")
      
      if(!is.null(input$messageFromAce$selector) && !is.null(input$messageFromAce$code) ){
        reqSelector<-input$messageFromAce$selector
        updateSelected4Ace(reqSelector)
      }
      # cat('request$sender=',format(request$sender),"\n")
      if(length(input$messageFromAce$isSaved)>0){ 
        aceId<-input$messageFromAce$id
        editOption$.saved <- input$messageFromAce$isSaved
        # cat('\n--setting editOption$.saved --\n')
        # cat("set editOption$.saved=",editOption$.saved,"\n")
      }
      cat('22 ace request$sender=',format(request$sender),"\n")
      if(sender %in% c('cmd.commit', 'cmd.add.column', 'cmd.add.asset', 'cmd.openFileNow', 'cmd.saveFileNow', 'cmd.file.new', 'cmd.tabChange')){
        # cat('33 request$sender=',format(request$sender),"\n")
        cat('Ace: invoking processCommit\n')
        processCommit()
        cat('returning from processCommit\n')
        cat('getTibName()=',format(getTibName()),"\n")
        if(sender %in% c('cmd.commit', 'cmd.add.column', 'cmd.add.asset', 'cmd.saveFileNow') && !is.null(getTibName())){ 
          if(sender=='cmd.add.asset'){
            name=input$messageFromAce$selector$assetName
          } else {
            name=getTibName() # 'cmd.commit', 'cmd.add.column'
          }
          tibs<-getPtDefs()$tib
          cat('name=',format(name),"\n")
          cat("ace invoking resetSelectedTibbleName\n")
          resetSelectedTibbleName(tibs=tibs, name=name)
        } else { 
          #name=NULL #  'cmd.openFileNow', 'cmd.tabChange' , 'cmd.file.new'
          
          # begin selectTibUpdate: 
          # browser()
          # cat('else: ', 'cmd.openFileNow', 'cmd.tabChange' , 'cmd.file.new',"\n")
          if(length(input$pages) >0 && nchar(input$pages)>0 && selectedTibble$tabId != input$pages ){
            # cat('next tabId=',input$pages,"\n")
            # cat('selectedTibble$tabId=',selectedTibble$tabId,"\n")
            # cat("input$messageFromAce$id=" , format(input$messageFromAce$id), "\n")
            choices<-getRightPanelChoices()
            if(length(choices)>0 && length(selectedTibble$tabId)>0  && selectedTibble$tabId!='whatthefuck'){
              # cat( "store( tabId=",selectedTibble$tabId,")\n")
              tmp2<-isolate(reactiveValuesToList(selectedTibble, all.names=TRUE))
              tmp2[is.null(tmp2)]<-NA
              #plotSelect.tib<-rbind(plotSelect.tib, tmp )
              tmp1<-filter(plot$selections.tib, tabId!=selectedTibble$tabId)
              plot$selections.tib<-bind_rows(tmp1, tmp2)
              # cat("========   ",paste(plot$selections.tib$tabId,collapse=", "),"\n")
            }
            row.tib<-filter(plot$selections.tib, tabId==input$pages)
            if(nrow(row.tib)==0){
              #cat('creating new tib for tabId=\n', input$pages,"\n")
              row.tib<-newPlotSel(tabId=input$pages, choices=choices, tibs=getPtDefs()$tib)
            }
            
            cat( "copy *row.tib* to *selectedTibble.*\n"  )
            lapply(names(row.tib), function(n){
              v<-row.tib[[n]][1]
              cat("row.tib$", n, "=", format(v),"\n")
              selectedTibble[[n]]<-ifelse(is.na(v), NULL, v)
            } )
          }
          # end selectTibUpdate:
          
        }
        
      } 
      
      if(sender %in% c( 'fileCmd.save', 'fileCmd.close', 'fileCmd.saveAs', 'fileCmd.quit' )){
        # cat("\n------",sender,"-----------------------------------\n")
        id<-input$messageFromAce$id
        saved<-input$messageFromAce$isSaved
        # cat('input$messageFromAce$saved=',format(saved),"\n")
        # cat('class(saved)=',class(saved),"\n")
        # cat('saved=',saved,"\n")
        # cat("and editOption$.saved=",editOption$.saved,"\n")
        # cat("for id=",id,"\n")
        if( !saved || sender=='fileCmd.saveAs' ) { #need to save
          docFilePath<-unlist(input$messageFromAce$docFilePath)
          # cat("docFilePath=",docFilePath,"\n")
          # cat("sender=",sender,"\n")
          if(docFilePath=='?' || sender=='fileCmd.saveAs'){ # file unnamed : fileSaveAs
             tabId<-aceID2TabID(id)
             # cat('11: tabId=',format(tabId),"\n")
             # cat("executing sendPtRManagerMessage( id=id,  sender=sender, saveFile=TRUE,  type='R', tabId=tabId)\n")
             sendPtRManagerMessage( id=id,  sender=sender, saveFile=TRUE,  type='R', tabId=tabId)
          } else { 
            # write file
            # cat('serverAce:: writeLines\n')
            code<-input$messageFromAce$code
            # !!!TODO!!! if write fails revert.
            writeLines(code, docFilePath)
            
            updateAceExt(id, sender,  setDocFileSaved=TRUE)
            editOption$.saved<-TRUE
            tabId<-popTab()
            if(request$sender %in% c('fileCmd.close', 'fileCmd.quit')){
              addToRecentFiles(input$messageFromAce$docFilePath)
              removeTab(inputId = "pages", tabId)
            } else { 
              addToRecentFiles(input$messageFromAce$priorFilePath)
              # !!!TODO add docFilePath to recentfiles
              #title=as.character(span(basename(docFilePath ), span( " " , class='icon-cancel')  ))
              title=as.character(tabTitleRfn( title=basename(docFilePath ), tabId=tabId, docFilePath=docFilePath ))
              # cat("title=",title,"\n")
              session$sendCustomMessage(
                type = "scrollManager", 
                list( title=title, value=tabId ) 
              )
            }
          }
        } else { #already saved
          tabId<-popTab()
          if(request$sender%in% c('fileCmd.close', 'fileCmd.quit') ){
            addToRecentFiles(input$messageFromAce$docFilePath)
            removeTab(inputId = "pages", tabId)
          }
        }
      }
      cat('\n final value of getTibRow()=', format(getTibRow()), "\n")
    }
}, priority = 90, ignoreNULL = TRUE, ignoreInit = TRUE)

updateAceExtDef<-function(newPtDef, sender, selector=list() ){

  newPtDef$tib<-pts2Integers(newPtDef$tib )
  
  replacementList<-ptDef2ReplacementList(name, newPtDef, getCode() ) #name not used!!!
  if( length(replacementList)>0 ){
    session$sendCustomMessage(
      type = "shinyAceExt",
      list(id= getAceEditorId(), replacement=replacementList, selector=selector, sender=sender, ok=1)
    )
  }
}

updateAceExt<-function(id, sender, ... ){
  data<-list(...)
  if(length(data)>0){
    data<-c(list(id= id, sender=sender), data )
    cat("data=",format(data),"\n")
    print(data)
    session$sendCustomMessage(
      type = "shinyAceExt",
      data
    )
  }
}

observeEvent(request$sender,{
    if(request$sender=='startup'){
      #cat('startup\n')
      cmdFileNew()
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

