buildRightMenu<-function(){
  dmdMenuBarPage(
    menuBarId="plotNavBar",
    menuDropdown('Display', 
                 menuDropdown('Points',
                              shinyDMDMenu::menuItem('Show Points without Labels', value='cmdShowPointsNoLabels'),
                              shinyDMDMenu::menuItem('Show Points with Labels',    value='cmdShowPointLabels')
                 ),
                 menuDropdown( 'Grid',
                               shinyDMDMenu::menuItem('Show Grid', value='cmdShowGrid')
                               # ,
                               # menuDropdown('lines',
                               #              shinyDMDMenu::menuItem('Width', value='cmdGridLineWidh'), 
                               #              shinyDMDMenu::menuItem('Color', value='cmdGridLineColor')
                               # )
                 )
                 ,
                 menuDropdown( 'Backdrop',
                               shinyDMDMenu::menuDropdown('colour',
                                 colourInput( 
                                 "backdropColour", NULL,
                                 palette = "limited",
                                 allowedCols = c(
                                   "white", "black", "red",
                                   "#DDD", "blue",
                                   "#0000FFA0", "#0000FF30",
                                   "rgb(255, 255, 0)", value='white')
                                 )
                                ),
                               shinyDMDMenu::menuItem(
                                  checkboxInput('solidBackdrop', 'Solid', TRUE), value="consolidate"
                                )
                 )
    ),
    menuDropdown(
      "Tools",
      shinyDMDMenu::menuDropdown(
        "Point Preprocessor",
        shinyDMDMenu::menuItem('New', value='cmdNewPP'),
        shinyDMDMenu::menuItem('Import', value='cmdImportPP'),
        shinyDMDMenu::menuItem('Export', value='cmdExportPP'),
        shinyDMDMenu::menuItem('Remove', value='cmdRemovePP')
      )
    )
  ) #menubar end 
}
