# pointR and ptR

- **pointR** is an R package providing a *shiny-based* minimalist ide for R scripting using svgR. **pointR** is primarly written in R, with a little bit of javascript for seasoning.
- **ptR** is an **electron** wrapper around **pointR**. Using **electron** 
	- Removes the dependency on the local browser
	- Provides independent windows for help
	- Provides an integrated approach for building **shiny custom inputs**
	- Prepackaged installations **ptR** are readily available for *mac* and *linux* (dmg and deb).
- ***Videos on pointR*** can be found at http://mslegrand.github.io/pointRmedia/.
- ***Related information*** can be found at http://mslegrand.github.io/svgR/.
- ***Rambling thoughts*** can be found on the wiki at https://github.com/mslegrand/pointR/wiki

## Installation 

We strongly recommend installing **ptR** from one of the installations packages. Prior to installation, you may want to 
consider installing **pandoc** and the R package **tidyverse**. Note, **pandoc** is used for **knitr** and is normally included in the **Rstudio** installation. 

## Some Points on pointR
-  pointR is a shiny server application
-  pointR uses the ACE editor for coding
-  pointR is intended to ease the writing of R scripts using svgR.
-  pointR is an attempt to bridge the gap between coding and point&click.
-  svgR bridges the gap between R and SVG


# Reporting issues
Please report any bugs/issue in the 
[Github Issue tracker](https://github.com/mslegrand/pointR)

#Contributing
If you wish to contribute to the pointR project, now is the time. Thanks.

# Caution
To quote from TrestleTech: *As with any online application, it is a genuinely bad idea to allow arbitrary users to execute code on your server.* **BE CAREFUL!!**

# Licensing

			NOTICE

Copyright (c) 2018 M. S. Legrand. All rights reserved

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

                   
