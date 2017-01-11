
optionFile<-paste(path.expand("~"),".ptRProfile.csv",sep="/")

#opts is a named vector of options
#optionFile is where to write the opts
writeOptions<-function(optionFile, opts){
  write.table(opts,file=optionFile, row.names=TRUE, col.names=FALSE)
}

# optionFile is where to read the opts
# return vector of options
readOptions<-function(optionFile){ 
  defaultOpts<-c(
    fontSize=16,
    theme="katzenmilch",
    tabSize=2,
    currentFile="",
    currentDirectory="."
  )
  try({
    tmp<-read.table(optionFile, header=FALSE, stringsAsFactors=FALSE, row.names=1)
    readOpts<-structure(tmp[,1], names= row.names(tmp))
    indx<-intersect(names(defaultOpts), names(readOpts))
    defaultOpts[indx]<-readOpts[indx]
  })
  defaultOpts
} 

defaultOpts<-readOptions(optionFile)
