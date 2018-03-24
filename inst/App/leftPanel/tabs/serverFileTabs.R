
jqui_sortable('#pages')

addFileTab<-function(title, txt,  docFilePath='?'){
  tabId<-tabName2TabId(title)
  aceId<-tabName2AceId(title)
  cat("addFileTab:: docFilePath",docFilePath,"\n")
  appendTab(
    inputId = "pages",
    tabPanel( #tabId,
      title=span(title, span( " " , class='icon-cancel')  ), 
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
  session$sendCustomMessage(
    type = "scrollManager", list( resize=TRUE )
  )
}

getAceEditorId<-reactive({
  tabId<-input$pages
  tabID2aceID(tabId)
})

#  triggers doc has been changed.
observeEvent(input$pages,{
  cat("input$pages=",format(input$pages),"\n")
  pages$fileName<-input$pages
  session$sendCustomMessage(
    type = "scrollManager", 
    list( selected=pages$fileName )
  )
  isolate(
    {
      sender='cmd.commit'
      triggerRefresh(sender, rollBack=FALSE)
    })
}, ignoreNULL = TRUE)

