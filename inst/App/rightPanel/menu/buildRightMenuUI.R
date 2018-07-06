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
                 # ,
                 # menuDropdown( 'Backdrop',
                 #               shinyDMDMenu::menuItem('Hide', value='cmdHideBack'),
                 #               shinyDMDMenu::menuItem('Color', value='cmdBackDropColor')
                 # )
    ),
    menuDropdown(
      "Tools",
      shinyDMDMenu::menuDropdown(
        "Point Preprocessor",
        shinyDMDMenu::menuItem('New', value='cmdNewPP'),
        shinyDMDMenu::menuItem('Import', value='cmdImportPP')
      )
    )
  ) #menubar end 
}
