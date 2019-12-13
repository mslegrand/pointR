#loads ptR templates

readTemplate<-function(name="rTemplate.R"){
  path<-ptRPath
  templateFilePath<-filePath(path, "App","templates",name)
  lines<-readLines(templateFilePath)
  src<-paste(lines,collapse="\n")
  src
}

fileTemplatesNames<-dir(filePath(find.package('pointR'), "App","templates"))
fileTemplates<-sapply( fileTemplatesNames, readTemplate)

projTemplateNames<-dir(filePath(ptRPath, "App","projectTemplates"))
projTemplatesPaths<-sapply( projTemplateNames, function(x){
  filePath(ptRPath, "App","projectTemplates", x )
})

projSamplesNames<-dir(filePath(ptRPath, "App","sampleProjects"))
projSamplesPaths<-sapply( projSamplesNames, function(x){
  filePath(ptRPath, "App","sampleProjects", x )
})
                            
  