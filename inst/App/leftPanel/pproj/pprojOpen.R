
openProj<-function(projName, pathToProj, projType="generic"){
  cat(">---> openProj\n")
  closeCurrentProj()
  cat('projName=', format(projName), " pathToProj=", format(pathToProj), "\n")
  pathToProj<-gsub(pattern = '^NA/', "~/", pathToProj)
  fullpathProjName=file.path(pathToProj, projName)
  cat( "fullpathProjName=", fullpathProjName, "\n" )
  pprj<-read_json(fullpathProjName)
  
  setUpProj(projName, pathToProj, projType="generic")
  #invoke startup
  request$sender<-'startup'
  cat("<---< openProj\n")
} 


