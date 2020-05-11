
## User Installation 

We strongly recommend installing **ptR** from one of the installations packages (deb, dmg, msis) found at https://github.com/mslegrand/pointR/releases. But before installing, there are a few requirements to be successful.

### Pre-Installation

- If not already installed, install [R](https://www.datacamp.com/community/tutorials/installing-R-windows-mac-ubuntu) 
- Pandoc is required. This can be done by installing either i or ii
	- i [pandoc](https://pandoc.org) (and placing it on your path)  
	- ii [RStudio](https://rstudio.com/products/rstudio/download/)  (RStudio contains a copy of pandoc)
- The R package [tidyverse ](https://www.tidyverse.org) is required. This can be installed in 2 ways
	- i from a terminal, open R and inside the R interpretor issue the command 
	**install.packages('tidyverse'**, then when finished issue command quit() and close terminal
	- ii open **RSTudio** (by clicking on it's icon) and then select the *packages tab* From there click on 
	*install* and in the popup type tidyverse. When finished, close **RStudio**.
### Installation

- go to https://github.com/mslegrand/ptR/releases and download the appropriate installer (dmg for mac, isis for windows and deb for most linux) 
- after downlo=ading, double click on the installer and be patient.

### Post-Instalation

- Upon opening, ptR will look for additiona R-=packages that it requires. If some of these packages are missing, you will be prompted to either quit or let ptR install them for you.  When completed, the ptR IDE will start.


Note, **pandoc** is used for **knitr** and is normally included in the **Rstudio** installation. 

## pointR and ptR

- **pointR** is an R package providing a *shiny-based* minimalist IDE for R scripting using svgR. **pointR** is primarly written in R, with a little bit of javascript for seasoning.
- **ptR** is an **Electron** wrapper around **pointR**. 
- Using **Electron** 
	- Removes potential hiccups by removing and dependency on the local browser 
	- Provides independent windows for help
	- Provides an integrated approach for building **shiny custom inputs**

- ***Related information*** can be found at http://mslegrand.github.io/svgR/.
- ***Rambling thoughts*** can be found on the wiki at https://github.com/mslegrand/pointR/wiki

## Some Points on pointR
-  pointR is a shiny server application
-  pointR uses the ACE editor for coding
-  pointR is intended to ease the writing of R scripts using svgR.
-  pointR is an attempt to bridge the gap between coding and point&click.
-  svgR bridges the gap between R and SVG

## Some Points on ptR
- ptR is an Electron wrapper around pointR
- ptR is javascript (since Electron is)
- Electron is required for full functionality (such as building shiny input controls)



## Building from Source

0. This can be painful.
1. [Download pointR](https://github.com/mslegrand/pointR) and build the **pointR** library
2. [Download ptR](https://github.com/mslegrand/ptR) and place in a folder (like pointR-electron/ptRMigrate)
3. cd to ptRMigrate and edit script ./build/mklib.sh to copy the ptR lib to 2 locations (for testing & production)
4. Install node.js, npm, electon
5. from terminal run **npm start**

**NOTE** I've built both on linux and mac, but not windows.


## Reporting issues
Please report any bugs/issue in the 
[Github Issue tracker](https://github.com/mslegrand/pointR)

#Contributing
If you wish to contribute to the pointR project, now is the time. Thanks.

## Caution
To quote from TrestleTech: *As with any online application, it is a genuinely bad idea to allow arbitrary users to execute code on your server.* **BE CAREFUL!!**

## Licensing

			NOTICE

Copyright (c) 2020 M. S. Legrand. All rights reserved

The pointR package as a whole is distributed under the GPL-3 License,
GNU GENERAL PUBLIC LICENSE version 3, see below.

The pointR package is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

The pointR package is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
GNU General Public License for more details.

                   
