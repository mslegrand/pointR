#initialized by defaultOpts in configIO.R
editOption<-reactiveValues(
  fontSize=defaultOpts[['fontSize']],
  theme=defaultOpts   [['theme']],
  tabSize=defaultOpts [['tabSize']],
  tabType=defaultOpts [['tabType']],
  currentFilePath=defaultOpts [['currentFilePath']],
  #currentFile=defaultOpts ['currentFile'],
  #currentDirectory=defaultOpts ['currentDirectory'],
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
  #browser()
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
    #browser()
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
  
  # fileSplit<-str_split(files,"/")
  # shortNames<-sapply(fileSplit,tail,n=1)
  # longNames<-lapply(fileSplit, tail, n=2)
  # menuNames<-sapply( longNames, function(ln){
  #   if(sum(ln[2]==shortNames)>1){
  #     paste(ln[2],"~",ln[1])
  #   } else {
  #     ln[2]
  #   }
  # })
  # entryVal<-function(i)entry=paste("recent",i,sep="-")
  # for(i in 1:length(menuNames)){
  #   if(i<=N){ #reset the menuItem name
  #     renameDMDM(
  #       session, 
  #       menuBarId="editNavBar",
  #       entry=entryVal(i), 
  #       newLabel=menuNames[i],
  #       newValue = NULL,
  #       type = "menuItem")
  #   } else { # append child
  #     appendDMDM(
  #       session, 
  #       menuBarId="editNavBar", 
  #       entry="Recent Files", 
  #       submenu=menuItem(
  #         label=menuNames[i],
  #         value=entryVal(i)
  #       )
  #     )
  #   }
  #}
  #history$recentFileMenuCount<-length(menuNames)
  editOption$recentFiles<-files
})
              
