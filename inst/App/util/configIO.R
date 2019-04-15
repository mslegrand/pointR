

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

# we might want to move this under the server
writeOptionsJSON<-function(opts){
  file<-path_join(c(optionDirPath(),"ptRProfile.json"))
  write_json(opts, file, pretty=4)
}


# Load initial options to the global defaultOpts 
# prior to the the running the session
# Subsequently the reactive editOption will be 
# initialize using the value of defaultOpts
defaultOpts<-(function(){
  defaultOpts<-list(
    fontSize=16,
    theme="chrome",
    tabSize=2,
    currentFile="",       # !!! appears to be not used
    currentDirectory=".", # !!! appears to be not used
    
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
    file<-path_join(c(optionDirPath(),"ptRProfile.json"))
    if(file.exists(file)){
      tmp<-read_json(file)
      defaultOpts[names(tmp)]<-tmp
      defaultOpts[["currentFilePath"]]<-""
    }
  })
  opts<-sapply(defaultOpts,unlist, USE.NAMES = T, simplify = F )
  opts
})() #execute now!
 
# adjust defaultOpts as needed
if(!is.null(getShinyOption("initialPointRProject"))){
  initialPointRProject<-getShinyOption("initialPointRProject")
  defaultOpts$currentProjectName<-basename(initialPointRProject)
  defaultOpts$currentProjectDirectory<-dirname(initialPointRProject)
}

initialProj<-NULL # upon startup of server, this copied to pprj

if(is.null(defaultOpts$currentProjectName) || # check on defaultOptsProject existence
   is.null(defaultOpts$currentProjectDirectory) ||
  !file.exists(file.path(defaultOpts$currentProjectDirectory, defaultOpts$currentProjectName))
  ){
    defaultOpts$currentProjectName<-NULL
    defaultOpts$currentProjectDirectory<-NULL
} else {
  try({
     fullpathProjName=file.path(defaultOpts$currentProjectDirectory, defaultOpts$currentProjectName)
     initialProj<-read_json(fullpathProjName, simplifyVector = TRUE) 
  })
}


# upon startup of server defaultOpts is copied  to editOption
# upon startup of server, initialProj copied to pprj


# electron option tells pointR whether this is being run from ptR.
if(!is.null(getShinyOption("electron"))){
  usingElectron<-TRUE
  ptRPath<-find.package('pointR') # TODO??? place  pointR inside electron package???
} else {
  usingElectron<-FALSE
  ptRPath<-find.package('pointR')
}


