
newProj<-function(projName, parentDir, projType="generic"){
    log.fin(newProj)
    
    closeCurrentProj()
    # todo!!! test for write permissions or put in tryCatch
    # specific to  new proj
    # 1. create dir
    pathToProj=file.path(parentDir,projName)
    dir.create(pathToProj)
    # 2. create workspace
    wrkspacePath=file.path(pathToProj,'.workspace') 
    dir.create(wrkspacePath)
    dir.create(path_join( c(pathToProj, resourceDir)))
    dir.create(path_join( c(pathToProj, resourceDir, 'preprocPts')))
    dir.create(path_join( c(pathToProj, resourceDir, 'preprocAts')))
    dir.create(path_join( c(pathToProj, resourceDir, 'dnds')))
    dir.create(path_join( c(pathToProj, resourceDir, 'snip')))
    # 3. fill pproj data
    if(!grepl('\\.pprj',projName)){
    projName<-paste0(projName, ".pprj")
    }
    ptRproj<-list(
      pathToProj=pathToProj,
      projName=projName,
      projType=projType
    )
    pprj(ptRproj)
    # 4. write pproj
    fullpathProjName=file.path(pathToProj, projName)
    write_json(ptRproj, fullpathProjName, pretty=4)
    # 5 setup
    setUpProj(projName, pathToProj, projType="generic")
    #6 invoke startup
    requestStartUp()
    log.fout(newProj)
} 

