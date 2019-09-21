

copyAndRenameProject<-function(sourceProject, targetName, pathToTargetParent ){
  # srce
  # 1 copies all files contained in templatePath to /tmpdir/clone/pattern/
  #    a. copies to /tmpdir/clone/basename(templatePath) 
  #    b. then renames to  tmpdir/clone/pattern
  # 2. then rename all files with names beginning 
  #     with /tmpdir/clone/pattern/..  to  /tmpdir/clone/projName/..
  # 3. then for all src file  replace contents
  #       ...pattern => ..projName
  # 4.
  
  # Tries to replace pattner with projName, but keep camelcase when encountered, 
  # For example if the pattern is pattern, and projName is projName
  # replace all  pattern => projName
  # replace all  Pattern => ProjName # to preserver any camelcase 
  # 
  # the problem is that projName may contain  Pattern, 
  # for example projname = 'xyzPatternUv'
  # hence we resort to an intermiary kludge, and do the replacement in 3 stages
  
  ptRproj<-read_json(sourceProject, simplifyVector = TRUE) 
  pattern<-sub('\\.pprj', '', ptRproj$projName)
  ptRproj$pathToProj<-sub(path_home(),'~', ptRproj$pathToProj) #l;udge for portability
  sourcePath<-dirname(sourceProject)
  
  projName<-sub('\\.pprj$', '', targetName)
  pathToProjParent<-pathToTargetParent
 
  
  # create clone dir (tmpDir)
  tempDir<-tempdir(check=TRUE)
  tempDir<-path_join(c(tempDir,'clone'))
  if(dir_exists(tempDir)){
    dir_delete(tempDir)
  }
  dir_create( tempDir )
  
  # copy source to tmpDir
  dir_copy(sourcePath, tempDir)
  bn<-basename(sourcePath)
  file.rename( path_join(c(tempDir, bn)), path_join(c(tempDir, pattern) )  )
  
  # define some aux helpers--------------
  lowerIt<-function(s){
    str_c(str_to_lower(substr(s, 1, 1)), substr(s, 2, nchar(s)))
  }
  upperIt<-function(s){
    str_c(str_to_upper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
  }
  pati<-lowerIt(pattern)
  Pati<-upperIt(pattern)
  prji<-lowerIt(projName)
  Prji<-upperIt(projName)
  camelReplace<-function(lines){
    patterns<-str_glue( '(', pati , ')|(', Pati , ')' )
    str_replace_all(lines, patterns, replacement=function(x){
      if(x==pati){
        prji
      } else {
        Prji
      }
    })
  } 
  
  # helper function to perform content susbstitution
  subContents<-function(fileName){
    if(is_file(fileName) ){
      lines<-readLines(fileName)
      lines<-camelReplace(lines)
      writeLines(text = lines,fileName)
    }
  }
  # helper function to perform path susbstitution
  pathSub<-function(start, paths){
    indx<-grep(start, paths)
    path_rel(path = paths[indx], start = start)->pr
    # do the replace
    zz<-camelReplace(paths)
    #patch together
    sapply(zz, function(p)path_join(c(start,p)))->zz2
    paths[indx]<-zz2
    paths
  }
  
  pathSubIt<-function(start, path){
    if(grepl(path_home(),path)){
      path<-sub(path_home(),"~", path)
    }
    if(grepl(start, path)){
      pr<-path_rel(path = path, start = start) #drop start
      if(!grepl('^source',pr)){
        pr<-camelReplace(pr)
      } 
      path<-path_join( c(start,pr))
    } 
    as.character(path)
  }
 
  # create listing of files to process
  filePaths<-rev(dir_ls(tempDir, recursive = T)) #skips hidden files
  indx<-grep(path_join(c(tempDir,pattern, resourceDir)),filePaths)
  if(length(indx)>0){
    filePaths<-filePaths[-indx] # skip resources
  }
  
  #filePaths<-filter(filePaths, glob=path_join(c(tempDir,pattern,resourceDir)), invert=TRUE)
  
 # filePaths<-filePaths[-grep('\\.((pprj)|(dnds))$',tmp)] # skip proj and dnds #!! replace with skip resources
  
  # substitute the contents of all files sans hidden and the pprj file 
  # with  projName replacing pattern
  lapply(filePaths,subContents)
  
  # rename listing files: replacing pattern by projName 
  # filePaths2<-path_split(filePaths)
  # filePaths2<-lapply(filePaths2, function(pth){ pth[length(pth)]<-camelReplace(pth[length(pth)])   ;pth })
  # filePaths2<-sapply(filePaths2, path_join)
  filePaths2<-sapply(filePaths, function(pth){
    path_join(c(dirname(pth),camelReplace(basename(pth))))
  })
  
  # filePaths2<-mapply( function(x,y){ path_join(c(x,y)}, dirname(filePaths),camelReplace(basename(pth)))

  for(i in which(filePaths!=filePaths2) ){ 
    file_move(filePaths[i],filePaths2[i]) 
  }
  
  
  # rename the page $fileDescriptor.filePath for each page in the workspace 
  workspace<-path_join(c(tempDir,projName,'.workspace'))
  #pathToProj= path_join(c(pathToProjParent,projNameOrig))
  pages<-dir_ls(path=workspace, regexp ='PTR')
  stem<-ptRproj$pathToProj
  for(page in pages){
    pg<-readRDS(page)
    fd<-pg$fileDescriptor.filePath
    
    if(identical(fd, '?')){ # if fd is "?" remove the page ??? 
      file.remove(page)
    } else { # rename the filePath accordingly 
      
      #fd<-pathSub(stem, fd )
      fd<-pathSubIt(stem, fd)
      # fd<-gsub( templatePath, pathToProj, fd)
      # fd<-gsub(pattern, projName, fd)
      #fd<-gsub(Pattern, ProjName, fd)
      pg$fileDescriptor.filePath<-fd
      saveRDS(pg, page)
    }
  }
  
  # finally we replace the .pprj
  ptRproj$projName<-paste0(projName,'.pprj')
    
  fullpathProjName1<- path_join(c(tempDir, projName,  ptRproj$projName ) )
  write_json(ptRproj, fullpathProjName1, pretty=4)
  dir_copy(path_join(c(tempDir,projName)), pathToProjParent)
  # return the fullproject name
  path_join(c(pathToProjParent, projName, ptRproj$projName))
}
