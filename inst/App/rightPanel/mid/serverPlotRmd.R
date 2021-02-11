modulePlotRmdUI <- function(id, input, output) { 
  ns <- NS(id)
  htmlOutput(ns( "rmd_Html" ))
}

modulePlotRmd<-function(input, output, session, 
  getPanelName,
  getCode
){
  
  output$rmd_Html <- renderUI({ 
    if(getPanelName() %in% rmdPanelTag){
      src<-getCode()
      if(grepl("output: dnd_snippet",src)){
        src<-dripplets2Rmd(src)
      }
      div( style='background-color: #FFFFFF;',
        HTML(knit2html(text =src , fragment.only = TRUE, quiet = TRUE))
      )
     } else {
      HTML('')
    }
  }) #end of renderUI
  list()
}

rmdModuleList<-callModule(
  module=modulePlotRmd, 
  id="rmdMod", 
  getPanelName=getRightMidPanel,
  getCode=getCode4Rendering
)


extractCodeBlocksFromRmd<-function(txt){
  
  lines<-unlist(str_split(txt, '\n'))
  # print(lines)
  pos<-grep('^```', lines)
  np<-length(pos)
  # cat('np=',format(np),"\n")
  blocks<-NULL
  if(np>=2){
    if(np%%2==1){
      np=np-1
    }
    pow<-pos[1:np]
    pos<-matrix(pow,2)
    # print(pos)
    i<-pos[1,]
    ll<-lines[i]
    
    cols<-grep('^```\\s*\\{\\s*r[,[:space:]]',lines[i])
    # cat('cols=',format(cols),'\n')
    pos<-pos[,cols]
    if(length(cols)>0){
      pos<-matrix(pos,2)
      # print(pos)
      blocks<-apply(pos,2, function(x){
        paste0(lines[(x[1]+1):(x[2]-1)], collapse="\n")
      })
      # print(blocks)
    }
  } 
  blocks
}