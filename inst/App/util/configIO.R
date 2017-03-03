
optionFile<-paste(path.expand("~"),".ptRProfile.csv",sep="/")

#opts is a named vector of options
#optionFile is where to write the opts
writeOptions<-function(optionFile, opts){
  write.table(opts,file=optionFile, row.names=TRUE, col.names=FALSE)
}

# optionFile is where to read the opts
# return vector of options
readOptions<-function(optionFile){ 
  defaultOpts<-list(
    fontSize=16,
    theme="katzenmilch",
    tabSize=2,
    currentFile="",
    currentDirectory=".",
    currentFilePath="./",
    tabType="Use Soft Tabs",
    recentFiles=NULL
  )
  try({
    tmp<-read.table(optionFile, header=FALSE, stringsAsFactors=FALSE, row.names=1)
    readOpts<-structure(tmp[,1], names= row.names(tmp))
    indx<-intersect(names(defaultOpts), names(readOpts))
    defaultOpts[indx]<-readOpts[indx]
    #browser()
    indx<-grep("^recentFiles", names(readOpts))
    if(length(indx)>0){
      rf<-readOpts[indx]
      #names(rf)
      rf<-rf[sort(names(rf))]
      defaultOpts[["recentFiles"]]<-rf
    }
    defaultOpts[["currentFilePath"]]<-""
    # defaultOpts["recentFiles"]<-
    #   unlist(str_split(defaultOpts["recentFiles"],";"))
  })
  defaultOpts
  #browser()
  defaultOpts
} 

defaultOpts<-readOptions(optionFile)



toggleTabType<-function(type){
  tabType<-c("Use Soft Tabs", "Use Hard Tabs")
  indx<-which(type==tabType)
  ifelse(indx==2,"Use Soft Tabs", "Use Hard Tabs" )
}