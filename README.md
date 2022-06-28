
## Installation 

For a full featured install go to http://mslegrand.github.io/pointR/. There you will find package installers for linux, mac and windows. 

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

0. Warning, you might find this painful.
1. [Download pointR](https://github.com/mslegrand/pointR) and build the **pointR** library
2. [Download ptR](https://github.com/mslegrand/ptR) and place in a folder (like pointR-electron/ptRMigrate)
3. cd to ptRMigrate and edit script ./build/mklib.sh to copy the ptR lib to 2 locations (for testing & production)
4. Install node.js, npm, electon
5. from terminal run **npm start**

**NOTE** I've built both on linux and mac machines, but not windows.


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

                   
