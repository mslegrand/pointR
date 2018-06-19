
buildLeftMenu<-function(version){
  dmdMenuBarPage( 
    title=version, 
    # theme=shinytheme("cerulean"),
    menuBarId="editNavBar",
    menuDropdown(
      "File", 
      shinyDMDMenu::menuDropdown(
        "New File",
        shinyDMDMenu::menuDropdown('ptR script',
              shinyDMDMenu::menuItem("ptR list containing a tibble", value="newPtrTibScript"),
              shinyDMDMenu::menuItem("ptR list containing a matrix", value="newPtRMatScript"),
              shinyDMDMenu::menuItem("svgR without a ptR list", value="newPtRSVGScript")
        ),
        shinyDMDMenu::menuItem('R script', value='newRScript'),
        shinyDMDMenu::menuItem('R markdown doc', value='newRmd'),
        #shinyDMDMenu::menuItem('R ioslides doc', value='newIOSlides')
        shinyDMDMenu::menuItem('Snippet File', value='newSnippets'),
        shinyDMDMenu::menuItem('Plain Text Doc', value='newText')
      ),
      menuDivider(),
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
      shinyDMDMenu::menuItem("Print", value="print"),
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
