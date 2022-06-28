
buildLeftMenu<-function(version){
  dmdMenuBarPage( 
    title=version, 
    # theme=shinytheme("cerulean"),
    menuBarId="editNavBar",
    menuDropdown(
      "File", 
      shinyDMDMenu::subMenuDropdown(
        "New File",
        shinyDMDMenu::subMenuDropdown('ptR script',
              shinyDMDMenu::menuItem("ptR list containing a tibble", value="newPtrTibScript"),
              shinyDMDMenu::menuItem("ptR list containing a matrix", value="newPtRMatScript"),
              shinyDMDMenu::menuItem("svgR without a ptR list", value="newPtRSVGScript")
        ),
        shinyDMDMenu::menuItem('R script', value='newRScript'),
        menuDivider(),
        shinyDMDMenu::menuItem('R markdown doc', value='newRmd'),
        shinyDMDMenu::menuItem('R ioslides presentation', value='newIOSlides'),
        menuDivider(),
        shinyDMDMenu::menuItem('Javascript',            value='newJavascript'),
        shinyDMDMenu::menuItem('Snippet',            value='newSnippets'),
        shinyDMDMenu::menuItem("Drag&Drop",            value="newDndSnippetsFile")
        # menuDivider(),
        # shinyDMDMenu::menuItem('Plain Text Doc', value='newText')
      ),
      subMenuDropdown('New Project',
        # shinyDMDMenu::menuItem('New Basic Project', value='newBasicProject'),
        shinyDMDMenu::menuItem('Clone of Existing Project', value='newCloneProject'),
        # shinyDMDMenu::menuItem('svgR-based ShinyInput Control', value='newSimpleInputWidget'),
        UIProjectTemplateMenu()
      ), 
      subMenuDropdown("Recent Projects"),
      menuDivider(),
      shinyDMDMenu::menuItem("Open File", value='openFile'),
      subMenuDropdown("Recent Files"),
      menuDivider( id='recentProjDivider'),
      shinyDMDMenu::menuItem("Open Project...", value='openProject'),
      

      menuDivider(),
      shinyDMDMenu::menuItem("Export as SVG"),
      menuDivider(),
      shinyDMDMenu::menuItem("Save"),
      shinyDMDMenu::menuItem("Save As...", value="saveAs"),
      shinyDMDMenu::menuItem("Save  All", value="saveAll"),
      menuDivider(),
      shinyDMDMenu::menuItem("Print", value="print"),
      menuDivider(),
      shinyDMDMenu::menuItem("Close File", value="close"),
      shinyDMDMenu::menuItem("Close All Files", value="closeAll"),
      shinyDMDMenu::menuItem("Close Project", value="closeProject"),
      menuDivider(),
      shinyDMDMenu::menuItem("Quit", value="quit", id='ptRQuit')
    ),
    # menuDropdown('Tibble',
    #              shinyDMDMenu::menuItem('New Tibble', value='cmdNewTibble'),
    #              shinyDMDMenu::menuItem('Rename Tibble', value='cmdRenameTibble'),
    #              shinyDMDMenu::menuItem('Delete Tibble', value='cmdDeleteTibble')
    # ),
    menuDropdown(
      'Options',
      subMenuDropdown(
        "Editor", 
        shinyDMDMenu::menuItem("Theme" ),
        shinyDMDMenu::menuItem("Font Size"), 
        shinyDMDMenu::menuItem("Adjust Tabs",       value="adjustTabs"),
        shinyDMDMenu::menuItem("Toggle White Space", value="toggleWhiteSpace"),
        shinyDMDMenu::menuItem(defaultOpts$tabType)
      ),
      #menuDivider(),
      subMenuDropdown(
        "Import Addin",
        shinyDMDMenu::menuItem("Snippets",   value="importSnippetFile"),
        shinyDMDMenu::menuItem("Drag&Drops",   value='importDndSnippetsFile')
      ),
      subMenuDropdown(
        "Manage Template Menu",
        shinyDMDMenu::menuItem("Add Current Project",   value="addTemplate"),
        shinyDMDMenu::subMenuDropdown("Remove from Menu",   UIRemoveUserTemplate())
      )
    ),
    menuDropdown(
      "Help",
      shinyDMDMenu::menuItem("Editor ShortCuts"),
      shinyDMDMenu::menuItem("Element Reference"),
      
      if(usingElectron){
        shinyDMDMenu::menuItem("svgR User Guide", value="svgRUserGuide")
      } else {
        shinyDMDMenu::menuItem(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/User_Guide.html\"  target=\"_blank\" >svgR User Guide </a></li>"))
      },
      if(usingElectron){
        subMenuDropdown(
          "Preprocessing", 
          shinyDMDMenu::menuItem(HTML("attribute"), value="preprocAttrHelp"),
          shinyDMDMenu::menuItem(HTML("point"),     value="preprocPtHelp")
        )
      } else {
        NULL
      },
      if(usingElectron){
        subMenuDropdown(
          "Useful Links", 
          shinyDMDMenu::menuItem(HTML("youtube playlist")),
          shinyDMDMenu::menuItem(HTML("io.svgR")),
          shinyDMDMenu::menuItem(HTML("W3C SVG reference"))
        )
      } else{
        subMenuDropdown(
          "Useful Links", 
          shinyDMDMenu::menuItem(HTML("<li><a  href=\"https://www.youtube.com/playlist?list=PLpvG89XJyQhlucHJxb9pr708NY1hTqSun\"  target=\"_blank\" >youtube playlist</a></li>")),
          shinyDMDMenu::menuItem(HTML("<li><a  href=\"http://mslegrand.github.io/svgR/\"  target=\"_blank\" >io.svgR</a></li>")),
          shinyDMDMenu::menuItem(HTML("<li><a  href=\"https://www.w3.org/TR/SVG/intro.html\"  target=\"_blank\" >W3C SVG reference</a></li>"))
        )
      },
      shinyDMDMenu::menuItem("About", value="aboutCmd")
    ), 
    menuItem('project: ',value="project")
  )
}
