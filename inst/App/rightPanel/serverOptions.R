#initialized by defaultOpts in configIO.R
editOption<-reactiveValues(
  fontSize=defaultOpts[['fontSize']],
  theme=defaultOpts   [['theme']],
  tabSize=defaultOpts [['tabSize']],
  tabType=defaultOpts [['tabType']],
  currentFilePath=defaultOpts [['currentFilePath']],
  #currentFile=defaultOpts ['currentFile'],
  #currentDirectory=defaultOpts ['currentDirectory'],
  useTribbleFormat= defaultOpts[['useTribbleFormat']],
  recentFiles=defaultOpts [['recentFiles']], 
  .saved=TRUE
)


history<-reactiveValues(
  #currentFilePath = defaultOpts [['currentFilePath']],
  #recentFiles=defaultOpts [['recentFiles']],
  recentFileMenuCount=0
  #code=NULL,
  #.saved=TRUE
)


getCurrentFile<-reactive({
  basename(editOption$currentFilePath)
})
getCurrentDir<-reactive({
  dirname(editOption$currentFilePath)
  #editOption$currentFile
})
getCurrentFilePath<-reactive({
  editOption$currentFilePath
})

getFileNameStatus<-reactive({
  !is.null(editOption$currentFilePath) &&
    editOption$currentFilePath!="" &&
    editOption$currentFilePath!="." &&
    editOption$currentFilePath!="./"
})

getFileSavedStatus<-reactive({editOption$.saved})
setCurrentFilePath<-function(filePath){
  editOption$currentFilePath<-filePath
}

# must both add and delete entries.
# This algorithm is very inefficient
observeEvent( editOption$recentFiles ,{
  # One strategy: remove all recentFiles and then reinsert
  # 1 remove menuDropdown("Recent Files")
  files<-unlist(editOption$recentFiles)
  N<-length(files)
  
  removeDMDM(
    session=session, menuBarId="editNavBar", entry="Recent Files")
  if(N>0){
    #1 make shortNames
    L<-str_split(files,"/")
    N<-length(files)
    toName<-function(x,n){
      t<-tail(L[[x]],n=n)
      paste(rev(t),collapse="~")
    }
    dupded<-function(S){
      sapply(1:length(S), function(i)sum(S==S[i])>1)
    }
    D<-rep(T,length.out=N)
    S<-D
    n<-0
    while(any(D)){
      I<-which(D)
      n<-n+1
      S[I]<-sapply(I, function(x)toName(x,n=n) )
      D<-dupded(S)
    }
    menuLabels<-S
    # 2 make dropdown containing menuItems for each recentFile 
    label="Recent Files"
    menuValues<-paste("recent",files, sep="-")
    items<-lapply(1:N, function(i){
      menuItem( label=menuLabels[i], value=menuValues[i])
    } )
    
    submenu=
      do.call(
        function(...){ menuDropdown(label,...) }, 
        items
      )
    #3 add dropdown to menu
    insertAfterDMDM(
      session, menuBarId = "editNavBar", 
      entry="Open", submenu= submenu)
  }
})

#update recentfiles whenever currentFile  changes
observeEvent( editOption$currentFilePath,{
  files<-editOption$recentFiles 
  N<-history$recentFileMenuCount
  #if(!is.null(editOption$currentFile) && editOption$currentFile!=""){
  if(getFileNameStatus()==TRUE){
    fileName<-editOption$currentFilePath
    N_Max<-10
    if(is.null(files)){
      files<-fileName
    }
    pos<-grep(fileName,files)
    if(length(pos)>0){
      files<-files[-pos]
    }
    files<-files[-N_Max]
    #remove tail
    files<-c(fileName, files)
    editOption$recentFiles<-files
  }
  
  editOption$recentFiles<-files
})
              
