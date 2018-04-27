
jqui_sortable('#pages')

closeRfn<-function(tabId){paste0("event.stopPropagation();Shiny.onInputChange('closeTab',  {id:'",tabId,"', type: 'tabId'} ); return false")}
tabTitleRfn<-function(title, tabId, docFilePath){
  span(
    bs_embed_tooltip(span(title, "class"="tabTitle"), title=docFilePath),
    #span(title, "class"="tabTitle"),
    span( " " , class='icon-cancel', onclick=closeRfn(tabId))  
  )
}

addFileTab<-function(title, txt,  docFilePath='?'){
  tabId<-getNextTabId()
  aceId<-tabID2aceID(tabId)
  cat("addFileTab:: docFilePath",docFilePath,"\n")
  # !!!TODO add docFilePath to recentFiles (if !='?')
  
  appendTab(
    inputId = "pages",
    tabPanel( #tabId,
      title=tabTitleRfn(title, tabId, docFilePath),
      #span(title, span( " " , class='icon-cancel', onclick=closeRfn(tabId))  ), 
      #title=span(title, span( " " , class='icon-cancel', onclick=closeRfn(tabId))  ), 
      #span(tabId,  actionButton(inputId=paste0("but",tabId), label="", class='icon-cancel') ), 
      #checkboxInput(tabId, tabId, FALSE),
      div(
        class="cAceContainer",
        overflow= "hidden",
        shinyAce4Ptr(
            outputId = aceId,  value=txt,
            mode="ptr", theme=defaultOpts["theme"],
            fontSize=16, autoComplete="live",
            autoCompleteList =list(svgR=names(svgR:::eleDefs)),
            docFilePath=docFilePath
          ),
           inline=FALSE
        ),
      value=tabId
    )
  )
  updateTabsetPanel(session,inputId = 'pages', selected = tabId)
      session$sendCustomMessage( type = "scrollManager", list( resize=TRUE )
  )
}

getAceEditorId<-reactive({
  tabId<-input$pages
  tabID2aceID(tabId)
})

#  triggers doc has been changed.
observeEvent(input$pages,{
  cat("input$pages=",format(input$pages),"\n")
  tabId<-input$pages 
  session$sendCustomMessage(
    type = "scrollManager", 
    list( selected=tabId ) #  Requests to scroll into view
  )
  aceId<-tabID2aceID(tabId)
  cat("input$pages:: aceId=",aceId,"\n")
  updateAceExt(id=aceId, sender='cmd.tabChange', roleBack=FALSE, setfocus=TRUE, getValue=TRUE)
  #triggerRefresh('cmd.commit', rollBack=FALSE) # seems to trigger the redraw of the screen (uses getValue=TRUE)
}, ignoreNULL = TRUE)


# updated by scrollManager and relays sender with tabs to request
observeEvent(input$tabManager,{
  cat("observer input$tabManager\n")
  tabs=unlist(input$tabManager$tabs)
  cat('tabs=',format(tabs),"\n")
  sender=input$tabManager$sender
  cat('sender=',format(sender),"\n")
  setTabRequest(sender=sender, tabs=tabs)
  cat("leaving tabManager observer------------------\n")
})

# request$tabs is updated by either
#  1. input$tabManager or 
#  2. ace 
# if non-empty, first request with 1st tab forwared to ace (save/close/saveAs) 
observeEvent(request$tabs, {
  if(length(request$tabs)>0){
    #tabId<-head(request$tabs,1)
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

