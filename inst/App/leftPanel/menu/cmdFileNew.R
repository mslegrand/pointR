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
    addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptr',fileSaveStatus=FALSE)
    delay(500, 
      updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE,  ok=TRUE )
    )
    mssg$error<-""
}

cmdFileNewRmd<-function(){
  src<-fileTemplates[[ "rmdTemplate.Rmd" ]] #rmdTemplate
  src<-sub('\ndate: "TODAY"', paste0('\ndate: "', Sys.Date(),'"'), src)
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptrrmd', fileSaveStatus=FALSE)
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE,  ok=TRUE )
  )
  mssg$error<-""
}

cmdFileNewIOSlides<-function(){
  src<-fileTemplates[[ "ioslidesTemplate.Rmd" ]] #rmdTemplate
  src<-sub('\ndate: "TODAY"', paste0('\ndate: "', Sys.Date(),'"'), src)
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptrrmd', fileSaveStatus=FALSE)
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE,  ok=TRUE )
  )
  mssg$error<-""
}


cmdFileNewSnippet<-function(){
  # cat('cmdFileNewSnippet\n')
  src<-fileTemplates[[ "snippetTemplate.snippets" ]] #rmdTemplate
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='snippets', fileSaveStatus=FALSE)
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE,  ok=TRUE )
  )
  mssg$error<-""
}

cmdFileNewTxt<-function(){
  src<-"A plain text document"
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='text', fileSaveStatus=FALSE)
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE,  ok=TRUE )
  )
  mssg$error<-""
}

cmdDndSnippetNew<-function(){
  src<-fileTemplates[[ "dndSnippetTemplate.dnippets" ]] #rmdTemplate
  src<-sub('\ndate: "TODAY"', paste0('\ndate:\ndate: "', Sys.Date(),'"'), src)
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='dnippets', fileSaveStatus=FALSE)
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE,  ok=TRUE )
  )
  mssg$error<-""
}


cmdNewIOSlides<-function(){
  src<-ioslidesTemplate
  tabName<-getNextAnonymousFileName()
  addFileTab(title=tabName, txt=src,  docFilePath="?", mode='ptrrmd')
  delay(500,
        updateAceExt(id=  getAceEditorId(), sender='cmd.file.new', getValue= TRUE,  ok=TRUE )
  )
  mssg$error<-""
}