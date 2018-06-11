#trigger help from menu
cmdSVGHelp<-function(query){
  helpsvgR$html<-svgQueryTopic2Help(query)
  showModal( modalHelp() )
}

#----trigger help popup from F1
observeEvent(input$helpMssg, {
  query<-input$helpMssg$query
  editorId<-input$editorId # !!! NOT USED
  if(length(query)>0 && nchar(query)>0){
    helpsvgR$html<-svgQueryTopic2Help(query)
    showModal( modalHelp() )
  }    
}) 

#----------------------

output$htmlHelpSvg_Out<-renderText(
  htmlHelpSvgOut()
)

htmlHelpSvgOut<-reactive({
  HTML(helpsvgR$html)
})

helpsvgR<-reactiveValues(
  html="",
  history=NULL
)

observeEvent(input$helpSvgR,{
  query<-input$helpSvgR
  helpsvgR$html<-svgQueryTopic2Help(query)
})


observeEvent(input$helpSvgRMssg,{
  mssg<-input$helpSvgRMssg
  if(length(mssg$queryTopic)>0){
    helpsvgR$html<-svgQueryTopic2Help(query)
  } else if(length(mssg$queryAddress)>0 ){
    helpsvgR$html<-svgQueryAddr2Help(mssg$queryAddress)
  }
})

#---- help popup  
modalHelp <- function(..., size = "m" ) {

  modalDialog(
    div( width="100%",
      htmlOutput(outputId = "htmlHelpSvg_Out", width="100%")
    ),
    title="Help",
    footer=tagList(
      actionButton("backHelp", "Back"),
      actionButton("dismiss", "Dismiss")
    ),
    easyClose = TRUE,
    ...
  ) 
}

observeEvent(input$dismiss,{
  removeModal()
  session = getDefaultReactiveDomain()
  session$sendCustomMessage(
    type = "shinyAceExt",
    list(id=getAceEditorId(), sender='fileCmd.dismiss', setfocus='focus')
  )
  #updateAceExt(id=getAceEditorId(), sender='fileCmd.dismiss', setfocus='focus' )
})

svgQueryAddr2Help<-function(queryAddr){
  # 1. trim off the front
  addr<-basename(queryAddr)
  
  if(addr=="00Index.html"){
    queryTopic<-"00Index.html"
  } else {
    # 2. look up in links
    pkg<-"svgR"
    path<-find.package(pkg)
    tools::findHTMLlinks(pkgDir=path,level=0)->links
    linksBase<-basename(links)
    pos<-which(addr==linksBase)
    # if(length(pos)>1){
    #   browser()
    # }
    if(length(pos)>0){
      queryTopic<-names(links)[pos[length(pos)]]
      
    } else {
      queryTopic<-"00Index.html"
    }    
  }
  svgQueryTopic2Help(queryTopic)
}

 
svgQueryTopic2Help<-function(query){
  query<-trimws(query)
  if(query=="[[.svgDoc"){
    query<-"sub-sub-.svgDoc"
  } else if (query=="%<c-%"){
    query<-"grapes-less-than-c-grapes"
  }
  #record this query so we can backup
  helpsvgR$history<-c(helpsvgR$history,query)
  
  pkg<-"svgR"
  path<-find.package(pkg)
  pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg), 'help', pkg))
  tools::findHTMLlinks(pkgDir=path,level=0)->links
  topics<-names(pkgRdDB)
  if(length(query)!=1){ browser() } # debug code!!!
  if(!(query %in% topics)){ # we default to the index
    # we generate the index page by cropping the original help page
    tmp<-readLines(file.path(path.package("svgR"), "html", "00Index.html") )
    pos<-which(str_detect(tmp, "body"))
    tmp[(pos[1]+1):(pos[2]-1)]->html
    pos<-which(str_detect(html, "arrow"))
    (pos[1]-1):(pos[2])->sss
    html<-html[-sss]
    pos<-which(str_detect(html,"toplog"))
    #replacement<-"" #<img  src=\"svgLogo.svg\" alt=\"[svgR logo]\" />"
    #html[pos]<-replacement
    pos<-which(str_detect(html,"</div><h2>"))
    html<-html[-(1:(pos-1))]
    html[1]<-sub( pattern = "^</div>", "", html[1] )
  } else {
    #query was found, now lets grab the page
    txtConnection<-textConnection("html","w")
    tools::Rd2HTML(
      pkgRdDB[[query]],
      package=pkg,
      Links=links,   
      out=txtConnection
    )
    close(txtConnection)
  }
  html<-paste(html,collapse="\n")
  html
  
  # Now we want to send a request back to the
  # server in the user clicks a  hyperlink to another page
  # We first modify samepage links to be non-links 
  html<-gsub('href="#','#href="',html)
  # Then we add onclick to all remaining links
  html<-gsub('(<a[[:space:]]+href=[^>]+)>','\\1 onclick="helpSvgRQuery(null,this.href); return false">', html)
  # Then undo the non-links back to interal links
  html<-gsub('#href="','href="#',html)
  
  html
}


# trigger help for short cut keys

observeEvent( input$keyBoardHelp,{
  kb<-input$keyBoardHelp
}) 

#handle back button for help
observeEvent(input$backHelp, {
  if(length(helpsvgR$history)>1){
    queryTopic<-tail(helpsvgR$history,2)[1]
    helpsvgR$history<-head(helpsvgR$history,-2)
    html<-svgQueryTopic2Help(queryTopic)
    helpsvgR$html<-html
  }
})




