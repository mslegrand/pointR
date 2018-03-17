
# probably this uioutput is obsolete when using tabs
# 
# output$TopLeftTabPanel<-renderUI({
#   tmp<-getCurrentFile()
#   if(nchar(tmp)==0){
#     tmp<-'Unnamed'
#   }
#   tabsetPanel( tabPanel(tmp))
# })

jqui_sortable('#pages')

pages<- reactiveValues(
  fileName='',
  fileNumber=1
)


addFileTab<-function(){
  pages$fileNumber
  newTabId<-paste0("Dynamic-", pages$fileNumber)
  newAceId<-'source'
  if(pages$fileNumber>1)
    newAceId<-paste0("Ace-", pages$fileNumber)
  txt=paste(rep(newTabId,50), collapse="\n")
  appendTab(inputId = "pages",
            tabPanel(newTabId)
            #tabPanel(newTabId, aceEditor(newAceId, value=txt))
            # tabPanel(newTabId, div(
            #   #id='aceContainer',
            #   "class"="cSvgOut cSvgOutRightIndent", #class"="cAceContainer",
            #   #style="overflow-y:hidden;",
            #   #overflow= "hidden",
            #   shinyAce4Ptr(
            #     outputId = newAceId,  value="",
            #     mode="ptr", theme=defaultOpts["theme"],
            #     fontSize=16, autoComplete="live",
            #     autoCompleteList =list(svgR=names(svgR:::eleDefs))
            #   ),
            #   inline=FALSE
            # ))
  )
  updateTabsetPanel(session,inputId = 'pages',selected = newTabId)
  #jqui_draggable('#pages li')
  session$sendCustomMessage(
    type = "scrollManager", 
    list( resize=TRUE )
  )
}

observeEvent( request$sender,{
  # if(request$sender=='startup'){
  #   for(i in 1:3){
  #     addFileTab()
  #   }
  # }
  
}) 


getAceEditorId<-reactive({
  return('source')
})

observeEvent(input$pages,{
  #cat(input$pages,"\n")
  pages$fileName<-input$pages
  session$sendCustomMessage(
    type = "scrollManager", 
    list( selected=pages$fileName )
  )
}, ignoreNULL = TRUE)