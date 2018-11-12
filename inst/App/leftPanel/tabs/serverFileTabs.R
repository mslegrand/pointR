
sendFileTabsMessage<-function(...){ 
  data<- list(...) 
  if(length(data)>0){
    # cat("sendFileTabsMessage::", paste(data, collapse = ', '),"\n")
    session$sendCustomMessage( type = "scrollManager",  data )
  }
}

# tracks the naming for unnamed pages
pages<- reactiveValues(
  fileName='',
  fileNameCount=1
)


# getNextAnonymousFileName<-function(){
#   newFileName<-paste0("Anonymous ", pages$fileNameCount)
#   pages$fileNameCount<-pages$fileNameCount+1
#   newFileName
# }

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

tabTitleRfn<-function(tabName, tabId, docFilePath){
  span(
    bs_embed_tooltip(span(tabName, "class"="tabTitle"), title=docFilePath),
    span( " " , class='icon-cancel', onclick=closeRfn(tabId))  
  )
}

closeTabNow<-function(tabId2X){
  stopifnot('tabId' %in% names(serverAssetDB$tib))
  serverAssetDB$tib<-filter(serverAssetDB$tib, tabId!=tabId2X)
  stopifnot('tabId' %in% names(handler$choices))
  handler$choices<-filter(handler$choices, tabId!=tabId2X)
  removeFileDesc(tabId2X)
  removeTab(inputId = "pages", tabId2X)
}






# TODO!!!! , add input parameters for:  mode, autocomplete
# fontsize should be read from options 
addFileTab<-function(title, txt,  docFilePath='?', mode='ptr', fileSaveStatus=FALSE){
  # cat("addFileTab:: mode=",mode,"\n")
  tabId<-getNextTabId()
  cat("addFileTab:: tabId",format(tabId),"\n")
  aceId<-tabID2aceID(tabId)
  # cat("addFileTab:: aceId",aceId,"\n")
  # cat("addFileTab:: docFilePath",format(docFilePath),"\n")
  # !!!TODO add docFilePath to recentFiles (if !='?')
  
  addFileDesc(pageId=tabId, docFilePath=docFilePath, fileSaveStatus, fileMode=mode)
  setUseTribble( pageId=tabId, value=TRUE)
  if(mode=='ptr'){
    divClass="cAceContainer"
  } else {
    divClass="cAceRmdContainer"
  }
  if(is.null(tabId)){ browser() }
  addNewPage2dnippetsDB(tabId)
  # ptpreprocId=tabID2prePtProc(tabId)
  appendTab(
    inputId = "pages",
    tabPanel( #tabId,
      title=tabTitleRfn(title, tabId, docFilePath),
      #span(title, span( " " , class='icon-cancel', onclick=closeRfn(tabId))  ), 
      #title=span(title, span( " " , class='icon-cancel', onclick=closeRfn(tabId))  ), 
      #span(tabId,  actionButton(inputId=paste0("but",tabId), label="", class='icon-cancel') ), 
      #checkboxInput(tabId, tabId, FALSE),
      div(
        class=divClass,
        overflow= "hidden",inline=FALSE,
        shinyAce4Ptr(
            outputId = aceId,  
            value=txt,
            mode=mode, 
            theme=editOption$theme, 
            fontSize=editOption$fontSize,
            autoComplete="enabled",
            if(mode=='ptr')
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
  # cat('tabId=',format(tabId),"\n")
  # xyz<-input$pages
  # cat('serverFileTabs.R:: input$pages=',format(xyz),"\n")
  
  updateTabsetPanel(session,inputId = 'pages', selected = tabId)
  
  sendFileTabsMessage(resize=runif(1))
  # updateAceExt(id=aceId, sender='cmd.file.new', getValue= TRUE,  ok=TRUE )

  # insertEDinPP(ptpreprocId)
  
  # selector='#cXX' #paste0( '#',NS("footerRight")('cXX'))
  # cat('selector=',selector,'\n')
  # ui=newPointPreprocessor(id=ptpreprocId)
  # #ui=textInput("txttxt", "Insert some text")
  # insertUI(
  #   selector=selector,
  #   where='beforeEnd',
  #   ui=ui,
  #   #immediate=FALSE,
  #   session=session
  # )
  return(tabId)
}

getAceEditorId<-reactive({
  tabId<-input$pages
  tabID2aceID(tabId)
})

#  triggers doc has been changed.
observeEvent(input$pages,{
  cat(">---> input$pages 2\n")
  tabId<-input$pages
  cat('tabId=',format(tabId),'\n')
  # if(!allGood(tabId)){ browser() }
  if(!is.null(tabId)){
    sendFileTabsMessage(selected=tabId)
  }
  aceId<-tabID2aceID(tabId)
  updateAceExt(id=aceId, sender='cmd.tabChange', roleBack=FALSE, setfocus=TRUE, getValue=TRUE)
  #triggerRefresh('cmd.commit', rollBack=FALSE) # seems to trigger the redraw of the screen (uses getValue=TRUE)
  cat("<---< input$pages 2\n")
},  ignoreNULL = FALSE, label='pages2')


# updated by scrollManager and relays sender with tabs to request
observeEvent(input$tabManager,{
  tabs=unlist(input$tabManager$tabs)
  sender=input$tabManager$sender
  setTabRequest(sender=sender, tabs=tabs)
}, label='tabManager')

# request$tabs is updated by either
#  1. input$tabManager or 
#  2. ace 
# if non-empty, first request with 1st tab forwarded to ace (save/close/saveAs) 
observeEvent(request$tabs, {
  if(length(request$tabs)>0){
    tabId<-peekTab()
    cat('tabId22=',format(tabId),'\n')
    sender<-getSender()
    aceId<-tabID2aceID(tabId)
    updateAceExt( id=aceId, sender=sender, getDoc=TRUE)
  } else {
    if(getSender()=='fileCmd.quit'){
      cmdQuitNow()
    }
  }
}, label='request$tabs')


