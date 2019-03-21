cmdFileNewPtR<-function(fileCmd="newPtrTibScript"){
    templateChoices<-list(
      newPtrTibScript=  "ptRTemplate.R",
      newPtRMatScript = "matTemplate.R",
      newPtRSVGScript = "svgRTemplate.R",
      newRScript      = "rTemplate.R"
    )
    templateName<-templateChoices[[fileCmd]]
    src<-fileTemplates[[ templateName ]] #ptR template
    tabName<-getNextAnonymousFileName()
    
    tabId<-addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptr',fileSaveStatus=FALSE)
    
    aceId<-tabID2aceID(tabId)
    
    mssg$error<-""
}

cmdFileNewRmd<-function(){
  src<-fileTemplates[[ "rmdTemplate.Rmd" ]] #rmdTemplate
  src<-sub('\ndate: "TODAY"', paste0('\ndate: "', Sys.Date(),'"'), src)
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptrrmd', fileSaveStatus=FALSE)
 
  mssg$error<-""
}

cmdFileNewIOSlides<-function(){
  src<-fileTemplates[[ "ioslidesTemplate.Rmd" ]] #rmdTemplate
  src<-sub('\ndate: "TODAY"', paste0('\ndate: "', Sys.Date(),'"'), src)
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptrrmd', fileSaveStatus=FALSE)
  
  mssg$error<-""
}


cmdFileNewSnippet<-function(){
  
  src<-fileTemplates[[ "snippetTemplate.snippets" ]] #rmdTemplate
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='snippets', fileSaveStatus=FALSE)
  
  mssg$error<-""
}

cmdFileNewTxt<-function(){
  src<-"A plain text document"
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='text', fileSaveStatus=FALSE)
  
  mssg$error<-""
}

cmdFileNewJavascript<-function(){
  src<-"// Javascript"
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='javascript', fileSaveStatus=FALSE)
  
  mssg$error<-""
}

cmdDndSnippetNew<-function(){
  src<-fileTemplates[[ "dndSnippetTemplate.dnippets" ]] #rmdTemplate
  src<-sub('\ndate: "TODAY"', paste0('\ndate:\ndate: "', Sys.Date(),'"'), src)
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='dnippets', fileSaveStatus=FALSE)
  #  mssg$error<-""
}


# cmdNewIOSlides<-function(){
#   src<-ioslidesTemplate
#   tabName<-getNextAnonymousFileName()
#   addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptrrmd')
#   delay(500,
#         updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE,  ok=TRUE )
#   )
#   mssg$error<-""
# }