
# this option file is being pushed to "~", maybe not such a 
#good idea


optionDirPath<-function(){
  dirPath<-switch(Sys.info()[['sysname']],
         Windows= '%localappdata%/ptR',
         Linux  = '~/.ptR',
         Darwin = '~/.ptR'
  )
  if(!file.exists(dirPath)){
    dir.create(dirPath)
  }
  workSpaceDir<-paste0(dirPath,"/",'workspace')
  if(!file.exists(workSpaceDir)){
    dir.create(workSpaceDir)
  }
  dirPath
}


initialFileDescDB<-function(){tibble(
  tabId='bogus',
  isSaved=FALSE,
  filePath="?",
  anonNo =1,
  mode='ptr'
)[0,]
}


# initialDnippetsDBUsage<-function(){
#   tibble(tabId='bogus')[0,]
# }

initialPreprocDB<-function(){
  points=tibble( tabId="bogus", tibName="bogus", ptColName='bogus', cmd="bogus", script='bogus')[0,]
  points
}

initialTribbleDB<-function(){
  tibble(
    tabId='bogus',
    show=FALSE,
    dx=50,
    dy=50,
    color='lightgrey'
  )[0,]
}

initialSvgGridDB<-function(){
  tibble(
    tabId='bogus',
    show=FALSE,
    dx=50,
    dy=50,
    color='lightgrey'
  )[0,]
}

initialBackDropDB<-function(){
  tibble(
    tabId='bogus',
    color='white',
    checked=TRUE
  )[0,]
}



# used by loader



dnippetsDirPath<-function(){ #!!! not used???
  opPath<-optionDirPath()
  dirPath<-paste(opPath,'drippets',sep='/')
  if(!file.exists(dirPath)){
    dir.create(dirPath)
  }
  dirPath
}

snippetsDirPath<-function(){  #!!! not used???
  opPath<-optionDirPath()
  dirPath<-paste(opPath,'snippets',sep='/')
  if(!file.exists(dirPath)){
    dir.create(dirPath)
  }
  dirPath
}


optionFile<-paste(path.expand("~"),".ptRProfile.csv",sep="/")


writeOptionsJSON<-function(opts){
  file<-paste(optionDirPath(),"ptRProfile.json", sep="/")
  write_json(opts, file, pretty=4)
}


# Load initial options to the global defaultOpts 
# prior to the the running the session
# Subsequently the reactive editOption will be 
# initialize using the value of defaultOpts
readOptionsJSON<-function(){
  defaultOpts<-list(
    fontSize=16,
    theme="chrome",
    tabSize=2,
    currentFile="",
    currentDirectory=".",
    currentFilePath="./",
    tabType="Use Soft Tabs",
    useTribbleFormat=TRUE,
    dnippetsFiles=NULL,
    recentFiles=NULL,
    recentProjects=NULL,
    currentProjectDirectory=NULL,
    currentProjectName=NULL
  )
  
  try({
    file<-paste(optionDirPath(),"ptRProfile.json", sep="/")
    tmp<-read_json(file)
    defaultOpts[names(tmp)]<-tmp
    defaultOpts[["currentFilePath"]]<-""
  })
  opts<-sapply(defaultOpts,unlist, USE.NAMES = T, simplify = F )
  opts
} #execute now!

defaultOpts<-readOptionsJSON()

toggleTabType<-function(type){
  tabType<-c("Use Soft Tabs", "Use Hard Tabs")
  indx<-which(type==tabType)
  ifelse(indx==2,"Use Soft Tabs", "Use Hard Tabs" )
}




readTemplate<-function(name="rTemplate.R"){
  path<-find.package('pointR')
  templateFilePath<-filePath(path, "App","templates",name)
  lines<-readLines(templateFilePath)
  src<-paste(lines,collapse="\n")
  src
}

fileTemplatesNames<-dir(filePath(find.package('pointR'), "App","templates"))
fileTemplates<-sapply( fileTemplatesNames, readTemplate)
