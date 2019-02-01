

# globals declared here are

# defaultOpts

# this is used to locate the user pointR options dir
optionDirPath<-function(){
  dirPath<-switch(Sys.info()[['sysname']],
         Windows= '%localappdata%/ptR',
         Linux  = '~/.ptR',
         Darwin = '~/.ptR'
  )
  if(!file.exists(dirPath)){
    dir.create(dirPath)
  }
  workSpaceDir<-paste0(dirPath,"/",'.workspace')
  if(!file.exists(workSpaceDir)){
    dir.create(workSpaceDir)
  }
  dirPath
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
    if(file.exists(file)){
      tmp<-read_json(file)
      defaultOpts[names(tmp)]<-tmp
      defaultOpts[["currentFilePath"]]<-""
    }
  })
  opts<-sapply(defaultOpts,unlist, USE.NAMES = T, simplify = F )
  opts
} #execute now!

defaultOpts<-readOptionsJSON() #this is the intial user options



if(!is.null(getShinyOption("initialPointRProject"))){
  initialPointRProject<-getShinyOption("initialPointRProject")
  defaultOpts$currentProjectName<-basename(initialPointRProject)
  defaultOpts$currentProjectDirectory<-dirname(initialPointRProject)
}


# we might want to move this under the server
writeOptionsJSON<-function(opts){
  file<-paste(optionDirPath(),"ptRProfile.json", sep="/")
  write_json(opts, file, pretty=4)
}


# used by loader

# dnippetsDirPath<-function(){ #!!! not used???
#   opPath<-optionDirPath()
#   dirPath<-paste(opPath,'drippets',sep='/')
#   if(!file.exists(dirPath)){
#     dir.create(dirPath)
#   }
#   dirPath
# }
# 
# snippetsDirPath<-function(){  #!!! not used???
#   opPath<-optionDirPath()
#   dirPath<-paste(opPath,'snippets',sep='/')
#   if(!file.exists(dirPath)){
#     dir.create(dirPath)
#   }
#   dirPath
# }


# specifies where to look for the ptR profile
# optionFile<-paste(path.expand("~"),".ptRProfile.csv",sep="/")






# toggleTabType<-function(type){
#   tabType<-c("Use Soft Tabs", "Use Hard Tabs")
#   indx<-which(type==tabType)
#   ifelse(indx==2,"Use Soft Tabs", "Use Hard Tabs" )
# }


