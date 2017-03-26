output$htmlHelpSvg_Out<-renderText(
  htmlHelpSvgOut()
)

htmlHelpSvgOut<-reactive({
  HTML(htmlHelpR$elements)
})

htmlHelpR<-reactiveValues(
  elements="hi there"
)

observeEvent(input$helpSvgR,{
  print("entering input$helpSvgR")
  query<-input$helpSvgR
  htmlHelpR$elements<-svgQueryTopic2Help(query)
})

observeEvent(input$helpSvgRMssg,{
  print("entering input$helpSvgRMssg")
  mssg<-input$helpSvgRMssg
  if(length(mssg$queryTopic)>0){
    htmlHelpR$elements<-svgQueryTopic2Help(query)
  } else if(length(mssg$queryAddress)>0 ){
    htmlHelpR$elements<-svgQueryAddr2Help(mssg$queryAddress)
  }
})

#---- help popup  
modalHelp <- function(..., size = "m" ) {
  
  modalDialog(
    div( width="100%",
      htmlOutput(outputId = "htmlHelpSvg_Out", width="100%")
    ),
    title="Help",
    footer=tagList(actionButton("backHelp", "Back"),modalButton("Dismiss")),
    easyClose = TRUE,
    ...
  ) 
}
svgQueryAddr2Help<-function(queryAddr){
  print("entering svgQueryAddr2Help")
  # 1. trim off the front
  addr<-basename(queryAddr)
  
  #then if backHelp is called, it should return this address
  print(addr)
  #browser()
  if(addr=="00Index.html"){
    queryTopic<-"00Index.html"
  } else {
    # 2. look up in links
    pkg<-"svgR"
    path<-find.package(pkg)
    tools::findHTMLlinks(pkgDir=path,level=0)->links
    linksBase<-basename(links)
    #pos<-grep(addr,links)
    pos<-which(addr==linksBase)
    if(length(pos)>1){
      browser()
    }
    if(length(pos)>0){
      queryTopic<-names(links)[pos]
      
    } else {
      queryTopic<-"00Index.html"
    }    
  }
  svgQueryTopic2Help(queryTopic)
}

svgQueryTopic2Help<-function(query){
  print("entering svgQueryTopic2Help")
  print(query)
  query<-trimws(query)
  if(query=="[[.svgDoc"){
    query<-"sub-sub-.svgDoc"
  } else if (query=="%<c-%"){
    query<-"grapes-less-than-c-grapes"
  }
  #record this address some where
  history$help<-c(history$help,query)
  pkg<-"svgR"
  path<-find.package(pkg)
  pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg), 'help', pkg))
  tools::findHTMLlinks(pkgDir=path,level=0)->links
  topics<-names(pkgRdDB)
  if(length(query)!=1){ browser() } # debug code!!!
  if(!(query %in% topics)){ # we default to the index
    print("not found")
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
    print(query)
    txtConnection<-textConnection("html","w")
    #tools::Rd2HTML(pkgRdDB[[query]],Links=links, Links2=links, out=txtConnection)
    tools::Rd2HTML(
      pkgRdDB[[query]],
      package=pkg,
      Links=links,   
      out=txtConnection
    )
    close(txtConnection)
  }
  
  html<-paste(html,collapse="\n")
  
  #Now we want to send a request back to the
  #server in the user clicks a  hyperlink to another page
  # We first modify samepage links to be non-links 
  html<-gsub('href="#','#href="',html)
  # Then we add onclick to all remaining links
  html<-gsub('(<a[[:space:]]+href=[^>]+)>','\\1 onclick="helpSvgRQuery(null,this.href); return false">', html)
  # Then undo the non-links back to interal links
  html<-gsub('#href="','href="#',html)
  
  html
}


#----observer that triggers help popup  
observeEvent(input$helpMssg, {
  print("entering input$helpMssg")
  query<-input$helpMssg
  if(length(query)>0 && nchar(query)>0){
  
    htmlHelpR$elements<-svgQueryTopic2Help(query)
    showModal( modalHelp() )
  }    
}) 

observeEvent( input$keyBoardHelp,{
  kb<-input$keyBoardHelp
  print(kb)
}) 

observeEvent(input$backHelp, {
  print("entering$backHelp")
  print(length(history$help))
  if(length(history$help)>1){
    print(history$help)
    queryTopic<-tail(history$help,2)[1]
    print(queryTopic)
    history$help<-head(history$help,-2)
    html<-svgQueryTopic2Help(queryTopic)
    htmlHelpR$elements<-html
  }
})




