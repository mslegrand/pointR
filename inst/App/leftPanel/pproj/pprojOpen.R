
# example
# pathToProj = '~/AA/XX'
# projName='XX.pprj'
# fullpathProjName='~/AA/XX/XX.pprj'


openProj<-function(projName, pathToProj, projType="generic"){
    # cat(">---> openProj\n")
    
    pathToProj<-gsub(pattern = '^NA/', "~/", pathToProj)
    fullpathProjName=file.path(pathToProj, projName)
    if(!file.exists(fullpathProjName) ){
      err<-paste0(format(fullpathProjName), " not found!" )
      alert(err)
      return(NULL)
    }
    closeCurrentProj() # this needs to complete prior to loading new proj
    ptRproj<-read_json(fullpathProjName, simplifyVector = TRUE) 
    pprj(ptRproj)
    setUpProj(projName, pathToProj, projType=projType)
    #invoke startup
     requestStartUp()
  
    # cat("<---< openProj\n")
} 


