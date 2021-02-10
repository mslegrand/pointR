
sendFileTabsMessage<-function(...){ 
  data<- list(...) 
  if(length(data)>0){
    session$sendCustomMessage( type = "scrollManager",  data )
  }
}


getNextTabId<-function(){
  tabId<-basename(tempfile('PTR-TABID'))
  tabId
}

aceID2TabID<-function(aceId){
  sub("ACE","TAB",aceId)
}
tabID2aceID<-function(tabId){
  sub("TAB","ACE",tabId)
}
tabID2prePtProc<-function(tabId){
  sub("TAB","PTPP",tabId)
}


closeRfn<-function(tabId){
  paste0("event.stopPropagation();Shiny.onInputChange('closeTab',  {id:'",tabId,"', type: 'tabId'} ); return false")
}

tabTitleRfn<-function(tabName, tabId, docFilePath, fileSaveStatus){
  if(fileSaveStatus==FALSE){
    tabNameClass<-"tabTitle star"
  } else {
    tabNameClass<-"tabTitle"
  }
  span(
    bs_embed_tooltip(span(tabName, "class"=tabNameClass), title=docFilePath),
    span( " " , class='icon-cancel', onclick=closeRfn(tabId))  
  )
}

closeTabNow<-function(tabId2X){
  stopifnot('tabId' %in% names(serverAssetDB$tib))
  serverAssetDB$tib<-filter(serverAssetDB$tib, tabId!=tabId2X)
  removePageWidgetDB(tabId2X)
  removeFileDesc(tabId2X)
  removeTab(inputId = "pages", tabId2X)
}

closeTabsNow<-function(tabIds2Close){
  if(length(tabIds2Close)>0){
    serverAssetDB$tib<-filter(serverAssetDB$tib, !(tabId %in% tabIds2Close))
    db<-widgetDB()
    db<-filter(db, !(tabId %in% tabIds2Close))
    widgetDB(db)
    fdDB<-fileDescDB()
    fdDB<-filter(fdDB, !(tabId %in% tabIds2Close))
    fileDescDB(fdDB)
    path=getWorkSpaceDir()
    for(id in tabIds2Close){
      pth<-paste0(path,"/",id,".rda")
      file.remove(pth)
      removeTab(inputId = "pages", id)
    }
  }
  
  
}





# TODO!!!! , add input parameters for:   autocomplete
# fontsize should be read from options 
addFileTab<-function(title, txt,  docFilePath='?', mode='ptr', fileSaveStatus=FALSE, link=NULL){
  log.fin(addFileTab)
  tabId<-getNextTabId()
  
  if(is.null(tabId)){ cat("tabId is null\n"); browser() } #should never happen
  parId=NULL
  if(!is.null(link)){
    parId<-unlist(strsplit(link,'\\.'))[[1]]
  }
  addFileDesc(pageId=tabId, docFilePath=docFilePath, fileSaveStatus, fileMode=mode, parId)
  setUseTribble( pageId=tabId, value=TRUE)
  addNewPage2dnippetsDB(tabId)
  
  aceId<-newPage(tabId=tabId, title=title, txt=txt,
                 docFilePath=docFilePath, mode=mode,
                 fileSaveStatus=fileSaveStatus,
                 link=link)
  
  #sendFileTabsMessage(tabId=pageId, sender='savedStatus', saveStatus=fileSaveStatus,resize=runif(1))
  sendFileTabsMessage(resize=runif(1))
  log.fout(addFileTab)
  return(tabId)
}

getAceEditorId<-reactive({
  tabId<-input$pages
  tabID2aceID(tabId)
})

#  triggers doc has been changed.
observeEvent(input$pages,{
  tabId<-input$pages
  if(!is.null(tabId)){
    aceId<-tabID2aceID(tabId)
    updateAceExt(id=aceId, sender='cmd.tabChange', roleBack=FALSE, setfocus=TRUE, getValue=TRUE)
  } else {
    reOrgPanels(id=NULL, mode=NULL)
  }
},  ignoreNULL = FALSE, ignoreInit = FALSE, label='pages2')


# updated by scrollManager and relays sender with tabs to request
observeEvent(input$tabManager,{
  tabs=unlist(input$tabManager$tabs)
  cmd=input$tabManager$sender
  setTabRequest(cmd=cmd, tabs=tabs)
}, label='tabManager')

# request$tabs is updated by either
#  1. input$tabManager or 
#  2. ace 
# if non-empty, first request with 1st tab forwarded to ace (save/close/saveAs) 
observeEvent(c(request$trigger,request$tabs), {
  if(length(request$tabs)>0){
    sender<-peekTabCmd()
    tabId<-peekTabRequest()
    docFilePath<-getFileDescriptor(tabId)$filePath
    if((docFilePath=='?' && 
        sender %in% c( 'buttonCmd.rmdViewer', "buttonCmd.runApp")
       ) || (
         sender=='fileCmd.saveAs'
       )
    ){ #need user to assign Path
      
      ext<-mode2pathExt( getMode() )
      ext<-shinyFiles:::formatFiletype(ext)
      target<-saveButtonFileNames[[getMode()]]
      sendPtRManagerMessage( # triggers shinyFiles
        sender=sender, saveFile=TRUE,  
        target=target, tabId=tabId 
      ) 
    } else {
      aceId<-tabID2aceID(tabId)
      updateAceExt( id=aceId, sender='fileCmd.save', 
                    getDoc=TRUE, 
                    getSaved=TRUE, 
                    getFilePath=TRUE
      )
    }
  }
}, label='request-tabs-trigger')

observeEvent(input$messageContextMenu, {
  cmd=input$messageContextMenu$cmd
  if(cmd=="newTab"){
    start_row=input$messageContextMenu$start_row
    end_row=input$messageContextMenu$end_row
    src<-input$messageContextMenu$code
    rid<-input$messageContextMenu$id
    tabName<-input$messageContextMenu$label
    rmdAceId<-tabID2aceID(input$pages)
    link<-paste(rmdAceId,rid, sep=".")
    # we update Ace with code
    # + all prior code as a hidden portion
    # and keep a hidden copy of full text for later reinsertion.
    # BUT widget handler then has a problem, would need to know which ptR we are refering to.
    if(tabName==""){
      tabName<-getNextAnonymousFileName()
    }
    tabId<-addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptr', fileSaveStatus=FALSE, link=link)
    # 
    aceId<-tabID2aceID(tabId)
    # alternatively set ace content of to code, and save full txt somewhere
    
    mssg$error<-""
  } else if(cmd=="openTab"){
    id<-input$messageContextMenu$id
    tabId=aceID2TabID(id)
    
    updateTabsetPanel(session, "pages", selected = tabId)
    # change to tabId
    #sendFileTabsMessage(selected=tabId, resize=runif(1))
    
    #setTabRequest(cmd="tabChange", tabs=tabId)
    
  }
  
  # log.fout(cmdFileNewPtR)
})