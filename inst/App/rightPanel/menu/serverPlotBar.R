#serverPlotBar
# this is the menubar for the plot window

observeEvent(input$plotNavBar, {
  cmd<-getRightMenuCmd()
  if(is.null(cmd)){
    return(NULL)
  }
  
  if(cmd == 'cmdShowGrid'){
    renameDMDM(session,  "plotNavBar", "cmdShowGrid", "Hide Grid", newValue="cmdHideGrid")
    setSvgGrid(input$pages, show=TRUE)
  }
  
  if(cmd == 'cmdHideGrid'){
    renameDMDM(session,  "plotNavBar",  "cmdHideGrid", "Show Grid",newValue="cmdShowGrid")
    setSvgGrid(input$pages, show=FALSE)
  }
  
  if(cmd == 'cmdAdjustGridSpacing'){
    spacingChoices<-c(.01, .05, .1, .5 ,1, 5,50,100,500)
    sgrid<-getSvgGrid()
    choiceDX<-sgrid$dx
    choiceDY<-sgrid$dy
    modalGridSpacing <- function() {
      modalDialog(
            selectInput( "selectGridDX", "Horizontal Spacing", spacingChoices,
              multiple=FALSE, selectize = FALSE, width="90px", selected=choiceDX
            ),
            selectInput( "selectGridDY", "Vertical Spacing", spacingChoices,
              multiple=FALSE, selectize = FALSE, width="90px", selected=choiceDY
            ),
        footer = tagList(actionButton("modalGridSpacingCancel", "Cancel"),actionButton("modalGridSpacingOk", "OK") )
      ) 
    }
    showModal( modalGridSpacing() )
    dirtyDMDM(session, "plotNavBar")
  }

  if(cmd == 'cmdBackDropColor'){
    # popup color picker
  }  
  
  if(cmd == 'cmdHideBack'){
    setBackDrop(hide=FALSE)
  } 
  
  if(cmd == 'cmdShowPointsNoLabels'){
    disableDMDM(session,  menuBarId="plotNavBar", entry="cmdShowPointsNoLabels")
    enableDMDM(session,  menuBarId="plotNavBar",  entry="cmdShowPointLabels")
    enableDMDM(session,  menuBarId="plotNavBar",  entry="cmdHidePoints")
    setDisplayOption(ptMode='Normal')
  }
  
  if(cmd == 'cmdShowPointLabels'){
    enableDMDM(session,  menuBarId="plotNavBar", entry="cmdShowPointsNoLabels")
    disableDMDM(session,  menuBarId="plotNavBar",  entry="cmdShowPointLabels")
    enableDMDM(session,  menuBarId="plotNavBar",  entry="cmdHidePoints")
    setDisplayOption(ptMode='Labeled')
  }
  
  if(cmd == 'cmdNewColumn'){
    showModal( addNewColModal() )
  }
  
  if(cmd == 'cmdNewPP'){ # disable unless ...
    # columnName<-getTibColumnName()
    type='points'
    labels<-preprocChoices[[type]]
    preprocScripts = fileTemplates[paste0(labels,'Template.R')]
    names(preprocScripts)<-labels
    cmdPreProcEdit(preprocScripts=preprocScripts, preprocName='', type=type)
    dirtyDMDM(session, "plotNavBar")
  }
  
  if(cmd == 'cmdImportPP'){ # disable unless ...
      cmdPreProcPtsImport()
      dirtyDMDM(session, "plotNavBar")
  }  
  
  # if(cmd=="cmdRemovePP"){ #-----save
  #   cmdPreProcPtsRemove('points')
  #   dirtyDMDM(session, "plotNavBar")
  # } 
  
  if(cmd == 'cmdNewAP'){ # disable unless ...
    log.fin(cmd == 'cmdNewAP' )
    type='attrs'
    labels<-preprocChoices[[type]]
    preprocScripts = fileTemplates[paste0(labels,'Template.R')]
    names(preprocScripts)<-labels
    cmdPreProcEdit(preprocScripts=preprocScripts, preprocName='', type=type)
    dirtyDMDM(session, "plotNavBar")
    log.fout(cmd == 'cmdNewAP')
    dirtyDMDM(session, "plotNavBar")
  }
  
  if(cmd == 'cmdImportAP'){ # disable unless ...
    cmdPreProcAtsImport()
    dirtyDMDM(session, "plotNavBar")
  }  
  
  if(cmd=="cmdRemoveAP"){ #-----save
    cmdPreProcAtsRemove('attrs')
    dirtyDMDM(session, "plotNavBar")
  } 
  
  if(!is.null(cmd)){
    dirtyDMDM(session, "plotNavBar")
  }
  
  
  if( grepl( '^editPP-points-', cmd)){
    type='points'
    preprocName<-sub("^editPP-points-","",cmd)
    tb<-filter(preProcScriptDB[[type]], scriptName==preprocName)
    preprocScripts<-unlist(tb$script)
    names(preprocScripts)<-unlist(tb$cmd)
    showModal( 
      modalPreProcEditor( preprocScripts, preprocName, type=type )
    )
  }
  if( grepl( '^editPP-attrs-', cmd)){
    type='attrs'
    preprocName<-sub("^editPP-attrs-","",cmd)
    tb<-filter(preProcScriptDB[[type]], scriptName==preprocName)
    preprocScripts<-tb$script
    names(preprocScripts)<-tb$cmd
    showModal( 
      modalPreProcEditor( preprocScripts, preprocName, type=type )
    )
  }
  
  
  
  
})



