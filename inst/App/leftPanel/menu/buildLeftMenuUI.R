
buildLeftMenu<-function(version){
  dmdMenuBarPage( 
    title=version, 
    # theme=shinytheme("cerulean"),
    menuBarId="editNavBar",
    menuDropdown(
      "File", 
      shinyDMDMenu::menuItem("New"),
      shinyDMDMenu::menuItem("Open"),
      menuDropdown("Recent Files"),
      menuDivider(),
      shinyDMDMenu::menuItem("Export as SVG"),
      menuDivider(),
      shinyDMDMenu::menuItem("Save"),
      shinyDMDMenu::menuItem("Save As...", value="saveAs"),
      shinyDMDMenu::menuItem("Save  All", value="saveAll"),
      menuDivider(),
      shinyDMDMenu::menuItem("Close", value="close"),
      shinyDMDMenu::menuItem("Close All", value="closeAll"),
      menuDivider(),
      shinyDMDMenu::menuItem("Quit", value="quit")
    ),
    # menuDropdown('Tibble',
    #              shinyDMDMenu::menuItem('New Tibble', value='cmdNewTibble'),
    #              shinyDMDMenu::menuItem('Rename Tibble', value='cmdRenameTibble'),
    #              shinyDMDMenu::menuItem('Delete Tibble', value='cmdDeleteTibble')
    # ),
    menuDropdown(
      'Configure',
      menuDropdown(
        "Editor Options", 
        shinyDMDMenu::menuItem("Theme" ),
        shinyDMDMenu::menuItem("Font Size"), 
        shinyDMDMenu::menuItem("Adjust Tabs",       value="adjustTabs"),
        shinyDMDMenu::menuItem("Show White Space"),
        shinyDMDMenu::menuItem(defaultOpts$tabType)
      ),
      menuDropdown(
        "Snippets",
        shinyDMDMenu::menuItem("Import", value="importSnippetFile"),
        shinyDMDMenu::menuItem("Disable")
      )
    ),
    menuDropdown(
      "Help",
      shinyDMDMenu::menuItem("Editor ShortCuts"),
      shinyDMDMenu::menuItem("Element Reference"),
      menuDropdown(
        "Useful Links", 
        shinyDMDMenu::menuItem(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/User_Guide.html\"  target=\"_blank\" >svgR User Guide </a></li>")),
        shinyDMDMenu::menuItem(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/\"  target=\"_blank\" >io.svgR</a></li>")),
        shinyDMDMenu::menuItem(HTML("<li><a  href=\"https://www.w3.org/TR/SVG/intro.html\"  target=\"_blank\" >W3C SVG reference</a></li>"))
      ),
      shinyDMDMenu::menuItem("About", value="aboutCmd")
    )
  )
}
