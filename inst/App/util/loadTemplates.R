#loads ptR templates

readTemplate<-function(name="rTemplate.R"){
  path<-find.package('pointR')
  templateFilePath<-filePath(path, "App","templates",name)
  lines<-readLines(templateFilePath)
  src<-paste(lines,collapse="\n")
  src
}

fileTemplatesNames<-dir(filePath(find.package('pointR'), "App","templates"))
fileTemplates<-sapply( fileTemplatesNames, readTemplate)
