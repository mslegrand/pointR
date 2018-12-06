
# example
# pathToProj = '~/AA/XX'
# projName='XX.pprj'
# fullpathProjName='~/AA/XX/XX.pprj'


openProj<-function(projName, pathToProj, projType="generic"){
  cat(">---> openProj\n")
  
  closeCurrentProj() # this needs to complete prior to loading new proj
  

  # delay(50,{  
      cat('projName=', format(projName), " pathToProj=", format(pathToProj), "\n")
      pathToProj<-gsub(pattern = '^NA/', "~/", pathToProj)
      fullpathProjName=file.path(pathToProj, projName)
      cat( "fullpathProjName=", fullpathProjName, "\n" )
      # !!!! pprj below is now used !!!!
      # browser()
      ptRproj<-read_json(fullpathProjName) 
      pprj(ptRproj)
      # save pprj and use during loading of workspace (to be invoked by startup)
      
      # should we use projName=pathToProj, projPath
      setUpProj(projName, pathToProj, projType="generic")
      #invoke startup
      request$sender<-'startup'
  # })
  cat("<---< openProj\n")
} 


