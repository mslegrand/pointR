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
    easyClose = TRUE,
    ...
  ) 
}
svgQueryAddr2Help<-function(queryAddr){
  print("entering svgQueryAddr2Help")
  # 1. trim off the front
  addr<-basename(queryAddr)
  print(addr)
  #browser()
  if(addr=="00Index.html"){
    queryTopic<-"00Index.html"
  } else {
    # 2. look up in links
    pkg<-"svgR"
    path<-find.package(pkg)
    tools::findHTMLlinks(pkgDir=path,level=0)->links
    pos<-grep(addr,links)
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
  pkg<-"svgR"
  #path<-file.path(find.package(pkg), 'help', pkg)
  path<-find.package(pkg)
  pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg), 'help', pkg))
  tools::findHTMLlinks(pkgDir=path,level=0)->links
  
  topics<-names(pkgRdDB)
  if(!(query %in% topics)){
    print("not found")
    tmp<-readLines(file.path(path.package("svgR"), "html", "00Index.html") )
    pos<-which(str_detect(tmp, "body"))
    tmp[(pos[1]+1):(pos[2]-1)]->html
    
    
  } else {
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
  # here we could try to correct for missing links
  #extract page from line 6
  
  #look for attr section
  #attrPos<-which(str_detect(html,"<h3>Available Attributes (Named Parameters)</h3>"))
  #look for element section
  #elPos<-which(str_detect(html,"Elements"))
  html<-paste(html,collapse="\n")
  
  
  #browser()
  #pattern='(<dt><code>([^<]+)</code></dt>)' # cause html has no links if Links==NULL
  # if Links=links , 
  # in Element-Index we have <code><a href="hkern.html">hkern</a>
  # and in animate we have <code><a href="desc.html">desc</a></code>
  # and the values in links appear to look like "../../svgR/html/vkern.html"
  # painful approach: 
  # 1. remove "../../svgR/html/" from links
  # 2. iterate through links and substitute link[x] with x in html
  # 3. replace href= by "onclick="helpSvgRTopic(\'x\')"
  
  #topics   <-names(links)
  #topics<-gsub("\\[", "bracket-",topics)
  #topicAddrs<-sub("\\.\\./\\.\\./svgR/html/","",links)
  
  
  #1. locate all anchors with href
  #pattern<-'(<a[:space:]+href=[^>]*)>'
  #2. insert 'onclick="svg
  #replace='\\2 onclick="helpSvgRQuery(this.href)"'
  #html<-gsub(pattern,replace, html)
  html<-gsub('(<a[[:space:]]+href=[^>]+)>','\\1 onclick="helpSvgRQuery(null,this.href); return false">', html)
  
  
  # topicAddrs<-sub("\\.\\./\\.\\./svgR/html/","",links)
  # patterns<-paste0('href="',topicAddrs,'"')
  # patterns<-sub("\\.","\\.",patterns)
  # insertion=' onclick="helpSvgRQuery(this.href)"'
  
  #print(patterns)
  # print(replacements)
  # browser()
  # for(i in 1:length(topics)){
  #   cat(i, patterns[i], str_detect(string = html, pattern = patterns[i]),"\n")
  # }
  # for(i in 1:length(topics)){
  #   if(str_detect(string = html, pattern = patterns[i])){
  #     cat("found",i, patterns[i],"\n")
  #     html<-gsub(pattern = patterns[i], replacement = replacements[i], html)
  #   }
  #     
  # }
  #browser()
  # for(i in 1:length(topics)){
  #   # print(i)
  #   # print(patterns[i])
  #   # print(replacements[i])
  #   rr<-replacements[i]
  #   pp<-patterns[i]
  #   print(pp)
  #   print(rr)
  #   print(i)
  #   if(str_detect(string = html, pattern = rr)){
  #     html<-gsub(pattern = pp, replacement = rr, x = html)
  #     print(paste("replacing",pp))
  #   }
  # }
  
  
  #pattern='<code>\ws*(<a href=[^<]+)<'
  
  #replace='<dt><code><a onclick="helpSvgRTopic(\'\\2\')" >(\\2)</a></code></dt>'
  #replace='href=\\"../../svgR/html/'
  #html<-gsub(pattern=pattern, replacement=replace, x=html, perl=FALSE)
  # browser()
  #cat(html)
  
  
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




