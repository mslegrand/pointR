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
    #aceId<-tabName2AceId(tabId)
    addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptr')
    delay(500, 
      updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE, setDocFileSaved=TRUE, ok=TRUE )
    )
    mssg$error<-""
}

cmdFileNewRmd<-function(){
  src<-fileTemplates[[ "rmdTemplate.Rmd" ]] #rmdTemplate
  src<-sub('\ndate: "TODAY"', paste0('\ndate:\ndate: "', Sys.Date(),'"'), src)
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptrrmd')
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE, setDocFileSaved=TRUE, ok=TRUE )
  )
  mssg$error<-""
}


cmdFileNewSnippet<-function(){
  cat('cmdFileNewSnippet\n')
  src<-fileTemplates[[ "snippetTemplate.snippets" ]] #rmdTemplate
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='snippets')
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE, setDocFileSaved=TRUE, ok=TRUE )
  )
  mssg$error<-""
}

cmdFileNewTxt<-function(){
  src<-"A plain text document"
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='text')
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE, setDocFileSaved=TRUE, ok=TRUE )
  )
  mssg$error<-""
}

cmdDndSnippetNew<-function(){
  src<-fileTemplates[[ "dndSnippetTemplate.dnippets" ]] #rmdTemplate
  src<-sub('\ndate: "TODAY"', paste0('\ndate:\ndate: "', Sys.Date(),'"'), src)
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='markdown')
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE, setDocFileSaved=TRUE, ok=TRUE )
  )
  mssg$error<-""
}


cmdNewIOSlides<-function(){
  src<-ioslidesTemplate
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptrrmd')
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE, setDocFileSaved=TRUE, ok=TRUE )
  )
  mssg$error<-""
}