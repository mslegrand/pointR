
# example
# pathToProj = '~/AA/XX'
# projName='XX.pprj'
# fullpathProjName='~/AA/XX/XX.pprj'


openProj<-function(projName, pathToProj, projType="generic"){
    # cat(">---> openProj\n")
    
    closeCurrentProj() # this needs to complete prior to loading new proj
    pathToProj<-gsub(pattern = '^NA/', "~/", pathToProj)
    fullpathProjName=file.path(pathToProj, projName)
    ptRproj<-read_json(fullpathProjName, simplifyVector = TRUE) 
    pprj(ptRproj)
    setUpProj(projName, pathToProj, projType=projType)
    #invoke startup
     requestStartUp()
  
    # cat("<---< openProj\n")
} 


