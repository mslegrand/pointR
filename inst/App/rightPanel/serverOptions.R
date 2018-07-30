#initialized by defaultOpts in configIO.R
editOption<-reactiveValues(
  fontSize=defaultOpts[['fontSize']],
  theme=defaultOpts   [['theme']],
  tabSize=defaultOpts [['tabSize']],
  tabType=defaultOpts [['tabType']],
  currentFilePath=defaultOpts [['currentFilePath']],
  useTribbleFormat= defaultOpts[['useTribbleFormat']],
  dnippetsFiles=defaultOpts[['dnippetsFiles']],
  recentFiles=defaultOpts [['recentFiles']], 
  .saved=TRUE
)


# history<-reactiveValues(
#   recentFileMenuCount=0
# )


getCurrentFile<-reactive({
  basename(editOption$currentFilePath)
})
getCurrentDir<-reactive({
  dirname(editOption$currentFilePath)
})

getCurrentFilePath<-reactive({
  editOption$currentFilePath
})

# TO REVISE!!!
getFileNameStatus<-reactive({
  !is.null(editOption$currentFilePath) &&
    editOption$currentFilePath!="" &&
    editOption$currentFilePath!="." &&
    editOption$currentFilePath!="./"
})

# TO REVISE!!!
#getFileSavedStatus<-reactive({editOption$.saved})

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
      shinyDMDMenu::menuItem( label=menuLabels[i], value=menuValues[i])
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


addToRecentFiles<-function(filePath){
  if(filePath!='?'){
    editOption$recentFiles <-unique(c(filePath,editOption$recentFiles ))
  }
}

removeFromRecentFiles<-function(filePath){
  tmp<-editOption$recentFiles[]
  tmp<-tmp[tmp!=filePath]
  editOption$recentFiles<-tmp
}

addToDnippetsFiles<-function(filePath){
  tmp<-editOption$dnippetsFiles
  tmp<-tmp[tmp!=filePath]
  editOption$dnippetsFiles<-tmp
}


              
