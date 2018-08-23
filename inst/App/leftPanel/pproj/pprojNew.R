
newProj<-function(projName, parentDir, projType="generic"){
  closeCurrentProj()
  # test for write permissions
  # specific to  new proj
  # 1. create dir
  pathToProj=file.path(parentDir,projName)
  dir.create(pathToProj)
  # 2. create workspace
  wrkspacePath=file.path(pathToProj,'workspace') #TODO:: change to .workspace soon
  dir.create(wrkspacePath)
  # 3. fill pproj data
  if(!grepl('\\.pprj',projName)){
    projName<-paste0(projName, ".pprj")
  }
  ptRproj<-list(
    projName=projName,
    projType=projType
  )
  # 4. write pproj
  
  fullpathProjName=file.path(pathToProj, projName)
  write_json(ptRproj, fullpathProjName, pretty=4)
  setUpProj(projName, pathToProj, projType="generic")
  #invoke startup
  request$sender<-'startup'
} 

