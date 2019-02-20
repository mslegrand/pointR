
copyAndRenameProject<-function(pattern, templatePath, projName, pathToProjParent ){
  # copy all files from templatePath to tempDir
  
  tempDir<-tempdir(check=TRUE)
  tempDir<-path_join(c(tempDir,'clone'))
  if(dir_exists(tempDir)){
    dir_delete(tempDir)
  }
  dir_create( tempDir )
  dir_copy(templatePath, tempDir)
  bn<-basename(templatePath)
  file.rename( path_join(c(tempDir, bn)), path_join(c(tempDir, projName) )  )
 
  #pattern<-str_to_lower(pattern)
  
  pattern<-paste0(tolower(substr(pattern, 1, 1)), substr(pattern, 2, nchar(pattern)))
  Pattern<-paste0(toupper(substr(pattern, 1, 1)), substr(pattern, 2, nchar(pattern)))
  #Pattern<-str_to_title(pattern)
  projNameOrig<-projName
  projName<-paste0(tolower(substr(projName, 1, 1)), substr(projName, 2, nchar(projName)))
  ProjName<-paste0(toupper(substr(projName, 1, 1)), substr(projName, 2, nchar(projName)))
  # projName<-str_to_lower(projName)
  # ProjName<-str_to_title(projName)
  
  # helper function to sub contentes of file
  subContents<-function(fileName){
    if(is_file(fileName) && !grepl('\\.pprj$', fileName)){
      lines<-readLines(fileName)
      lines<-gsub(pattern, projName, lines)
      lines<-gsub(Pattern, ProjName, lines)
      writeLines(text = lines,fileName)
    }
  }
  
  # substitute the contents of all files sans hidden and the pprj file 
  # with  projName replacing pattern
  #tmpl<-path_join(c(tempDir, basename(templatePath)))
  filePaths<-rev(dir_ls(tempDir, recursive = T))
  
  lapply(filePaths,subContents)
  
  
  # rename all files  with  projName replacing pattern (includes hidden files)
  filePaths2<-path_split(filePaths)
  filePaths2<-lapply(filePaths2, function(pth){ pth[length(pth)]<-gsub(pattern, projName, pth[length(pth)]);pth })
  filePaths2<-lapply(filePaths2, function(pth){ pth[length(pth)]<-gsub(Pattern, ProjName, pth[length(pth)]);pth })
  filePaths2<-sapply(filePaths2,path_join)
  indx<-which(filePaths!=filePaths2)
  for(i in indx){ file_move(filePaths[i],filePaths2[i]) }
  
  # move these files to pathToProj
  #path_join(c(tempDir, basename(templatePath)))
  
  # rename the page $fileDescriptor.filePath for each page in the workspace that is not '?'
  workspace<-path_join(c(tempDir,projNameOrig,'.workspace'))
  pathToProj= path_join(c(pathToProjParent,projNameOrig))
  pages<-dir_ls(path=workspace, regexp ='PTR')
  for(page in pages){
    pg<-readRDS(page)
    fd<-pg$fileDescriptor.filePath
    
    if(identical(fd, '?')){ # if fd is "?" remove the page ??? 
      file.remove(page)
    } else { # rename the filePath accordingly 
      fd<-gsub( templatePath, pathToProj, fd)
      fd<-gsub(pattern, projName, fd)
      fd<-gsub(Pattern, ProjName, fd)
      pg$fileDescriptor.filePath<-fd
      saveRDS(pg, page)
    }
  }
  page<-path_join(c(workspace, "loadedDnippets.rda"))
  if(file_exists(page)){
    pg<-readRDS( page)
    fd<-pg$fullpath
    fd<-gsub( templatePath, pathToProj, fd)
    fd<-gsub(pattern, projName, fd)
    fd<-gsub(Pattern, ProjName, fd)
    pg$fullpath<-fd
    saveRDS(pg, page)
  }
  
  # finally we replace the .pprj
  projNameExt<-paste0(projNameOrig,'.pprj')
  ptRproj<-list(
    pathToProj= pathToProj,
    projName=projNameExt,
    projType='custom'
  )
  
  fullpathProjName1<- path_join(c(tempDir,projNameOrig,  projNameExt ) )
  write_json(ptRproj, fullpathProjName1, pretty=4)
  dir_copy(path_join(c(tempDir,projNameOrig)), pathToProjParent)
  path_join(c(pathToProjParent, projNameOrig, projNameExt ) )
}
