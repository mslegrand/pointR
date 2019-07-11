# # dropdown for preprocessor
newPreProcPanel<-function(label){
  outputId<-paste0('ptPreProcAce',label)
  tabPanel(label,
   div(
       aceEditor(
         outputId=outputId,
         height = "300px",
         mode='r',
         value=label
      )
   )
  )
}

preProcTabSetPanel<-function(id='ptPreProcpages' ){
  #labels=c("onNewPt", "onMovePt", "onMoveMat")
  preprocChoices<-unlist(preprocChoices, use.names = FALSE)
  pptabs<-c(id=id, lapply(preprocChoices,newPreProcPanel))
  do.call(tabsetPanel, pptabs)
}

# dropdown for preprocessor
newPointPreprocessor<-function()
{ 
  dropdownId= "ptPreProcDropDown" #paste0("PtPreProc",id)
  absolutePanel( id='PtPreProcDiv', left=5, bottom=5, 
                 dropdown( inputId = dropdownId ,  
                           div( id='ptPreProcBackPanel', class='backPanel',
                                div( style='margin-left:20px; color: #00ffff; ', h4(textOutput("ptPreProcSource")) ) ,
                                preProcTabSetPanel(),
                                div(style="width: 100%; overflow: hidden; margin-top:5px; margin-bottom:5px",
                                    div( style="float:left;",
                                         actionButton(inputId= "commitPtPreProcButton", label='Commit', class="btn" )
                                    ),
                                    div( style="float:right;",
                                         actionButton(inputId= "dimissPtPreProcButton", label='Dismiss', class="btn" )
                                    )
                                )
                           ),
                           icon=icon("filter", lib = "glyphicon"), #icon=icon("wrench", lib = "glyphicon"), #icon = icon("toolbox"),
                           status = "primary", width = "400px", size='sm',
                           up=TRUE, right=FALSE,
                           animate = animateOptions(
                             enter = animations$fading_entrances$fadeInLeftBig,
                             exit  = animations$fading_exits$fadeOutRightBig
                           )
                 )
  ) 
}
