


closeRfn<-function(tabId){paste0("event.stopPropagation();Shiny.onInputChange('closeTab',  {id:'",tabId,"', type: 'tabId'} ); return false")}
tabTitleRfn<-function(tabName, tabId, docFilePath){
  span(
    bs_embed_tooltip(span(tabName, "class"="tabTitle"), title=docFilePath),
    span( " " , class='icon-cancel', onclick=closeRfn(tabId))  
  )
}

closeTabNow<-function(tabId2X){
  plot$selections.tib<-filter(plot$selections.tib, tabId!=tabId2X)
  handler$choices<-filter(handler$choices, tabId!=tabId2X)
  removeTab(inputId = "pages", tabId2X)
}

# TODO!!!! , add input parameters for:  mode, autocomplete
# fontsize should be read from options 
addFileTab<-function(title, txt,  docFilePath='?', mode='ptr'){
  cat("addFileTab:: mode=",mode,"\n")
  tabId<-getNextTabId()
  # cat("addFileTab:: tabId",tabId,"\n")
  aceId<-tabID2aceID(tabId)
  # cat("addFileTab:: aceId",aceId,"\n")
  # cat("addFileTab:: docFilePath",format(docFilePath),"\n")
  # !!!TODO add docFilePath to recentFiles (if !='?')
  if(mode=='ptr'){
    divClass="cAceContainer"
  } else {
    divClass="cAceRmdContainer"
  }
  appendTab(
    inputId = "pages",
    tabPanel( #tabId,
      title<-tabTitleRfn(title, tabId, docFilePath),
      #span(title, span( " " , class='icon-cancel', onclick=closeRfn(tabId))  ), 
      #title=span(title, span( " " , class='icon-cancel', onclick=closeRfn(tabId))  ), 
      #span(tabId,  actionButton(inputId=paste0("but",tabId), label="", class='icon-cancel') ), 
      #checkboxInput(tabId, tabId, FALSE),
      div(
        class=divClass,
        overflow= "hidden",
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
            docFilePath=docFilePath
          ),
           inline=FALSE
        ),
      value=tabId
    )
  )
  
  
  updateTabsetPanel(session,inputId = 'pages', selected = tabId)
  sendFileTabsMessage(resize=runif(1))                          
}

getAceEditorId<-reactive({
  tabId<-input$pages
  tabID2aceID(tabId)
})

#  triggers doc has been changed.
observeEvent(input$pages,{
  tabId<-input$pages 
  sendFileTabsMessage(selected=tabId)
  
  aceId<-tabID2aceID(tabId)
  updateAceExt(id=aceId, sender='cmd.tabChange', roleBack=FALSE, setfocus=TRUE, getValue=TRUE)
  #triggerRefresh('cmd.commit', rollBack=FALSE) # seems to trigger the redraw of the screen (uses getValue=TRUE)
}, ignoreNULL = TRUE)


# updated by scrollManager and relays sender with tabs to request
observeEvent(input$tabManager,{
  # cat("observer input$tabManager\n")
  tabs=unlist(input$tabManager$tabs)
  # cat('tabs=',format(tabs),"\n")
  sender=input$tabManager$sender
  # cat('sender=',format(sender),"\n")
  setTabRequest(sender=sender, tabs=tabs)
  # cat("leaving tabManager observer------------------\n")
})

# request$tabs is updated by either
#  1. input$tabManager or 
#  2. ace 
# if non-empty, first request with 1st tab forwarded to ace (save/close/saveAs) 
observeEvent(request$tabs, {
  if(length(request$tabs)>0){
    tabId<-peekTab()
    sender<-getSender()
    aceId<-tabID2aceID(tabId)
    updateAceExt( id=aceId, sender=sender, getDoc=TRUE)
    # saveFile(tabId)
  } else {
    if(getSender()=='fileCmd.quit'){
      cmdQuitNow()
    }
  }
})


