
openProj<-function(projName, pathToProj, projType="generic"){
  cat(">---> openProj\n")
  
  closeCurrentProj() # this needs to complete prior to loading new proj
  

  # delay(50,{  
      cat('projName=', format(projName), " pathToProj=", format(pathToProj), "\n")
      pathToProj<-gsub(pattern = '^NA/', "~/", pathToProj)
      fullpathProjName=file.path(pathToProj, projName)
      cat( "fullpathProjName=", fullpathProjName, "\n" )
      # !!!! pprj below is not currently used !!!!
      pprj<-read_json(fullpathProjName) 
      
      # should we use projName=pathToProj, projPath
      setUpProj(projName, pathToProj, projType="generic")
      #invoke startup
      request$sender<-'startup'
  # })
  cat("<---< openProj\n")
} 


