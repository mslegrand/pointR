---
layout: default
---

##  Installation Instructions

The recommend installation of **ptR** is to use one of the prebuilt packages shown found to the left. But *prior to installing*, there are a few required dependencies that should be installed.

### Pre-Installation (Requirements)
- **R** is required. If not already installed, **[install R](https://www.datacamp.com/community/tutorials/installing-R-windows-mac-ubuntu)** 
- **Pandoc** is required. This means either 1. or 2. below
	1. **[install pandoc](https://pandoc.org)** (and placing it on your path)  
	2. **[install RStudio](https://rstudio.com/products/rstudio/download/)**  (RStudio contains a copy of pandoc)
- The **R** package **[tidyverse ](https://www.tidyverse.org)** is required. This can be installed by either 1. or 2. below
	1. From a terminal, open **R** and inside the **R** interpretor issue the command 
	*install.packages('tidyverse')*, then when finished issue command *quit()* and close terminal
	2. Open **RStudio** (by clicking on it's icon) and then select the *packages tab* From there click on 
	*install* and in the popup type *tidyverse*. When finished, close **RStudio**.
    
### Installation (Using the Installer)
- Choose the button on the left which matches your os to download the approiate installer.
- go to https://github.com/mslegrand/ptR/releases and download the appropriate installer (dmg for mac, isis for windows and deb for most linux) 
- After downloading, double click on the installer and be patient. **Note**: I have not yet purchased digital signatures, so you may be prompted with a warning. Please ignore.

### Post-Instalation (First time running)
- Upon opening, ptR will look for *additiona R-packages* that it requires. If some of these packages are missing, you will be prompted to either quit or let **ptR** install them for you.  When completed, the ptR IDE will start.


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




## Reporting issues
Please report any bugs/issue in the 
[Github Issue tracker](https://github.com/mslegrand/pointR)

#Contributing
If you wish to contribute to the pointR project, now is the time. Thanks.

## Caution
To quote from TrestleTech: *As with any online application, it is a genuinely bad idea to allow arbitrary users to execute code on your server.* **BE CAREFUL!!**


Your Text can be **bold**, _italic_, or ~~strikethrough~~.

[Link to another page](./another-page.html).

There should be whitespace between paragraphs.

There should be whitespace between paragraphs. We recommend including a README, or a file with information about your project.

# Header 1

This is a normal paragraph following a header. GitHub is a code hosting platform for version control and collaboration. It lets you and others work together on projects from anywhere.

## Header 2

> This is a blockquote following a header.
>
> When something is important enough, you do it even if the odds are not in your favor.

### Header 3

```js
// Javascript code with syntax highlighting.
var fun = function lang(l) {
  dateformat.i18n = require('./lang/' + l)
  return true;
}
```

```ruby
# Ruby code with syntax highlighting
GitHubPages::Dependencies.gems.each do |gem, version|
  s.add_dependency(gem, "= #{version}")
end
```

#### Header 4

*   This is an unordered list following a header.
*   This is an unordered list following a header.
*   This is an unordered list following a header.

##### Header 5

1.  This is an ordered list following a header.
2.  This is an ordered list following a header.
3.  This is an ordered list following a header.

###### Header 6

| head1        | head two          | three |
|:-------------|:------------------|:------|
| ok           | good swedish fish | nice  |
| out of stock | good and plenty   | nice  |
| ok           | good `oreos`      | hmm   |
| ok           | good `zoute` drop | yumm  |

### There's a horizontal rule below this.

* * *

### Here is an unordered list:

*   Item foo
*   Item bar
*   Item baz
*   Item zip

### And an ordered list:

1.  Item one
1.  Item two
1.  Item three
1.  Item four

### And a nested list:

- level 1 item
  - level 2 item
  - level 2 item
    - level 3 item
    - level 3 item
- level 1 item
  - level 2 item
  - level 2 item
  - level 2 item
- level 1 item
  - level 2 item
  - level 2 item
- level 1 item

### Small image

![Octocat](https://github.githubassets.com/images/icons/emoji/octocat.png)

### Large image

![Branching](https://guides.github.com/activities/hello-world/branching.png)


### Definition lists can be used with HTML syntax.

<dl>
<dt>Name</dt>
<dd>Godzilla</dd>
<dt>Born</dt>
<dd>1952</dd>
<dt>Birthplace</dt>
<dd>Japan</dd>
<dt>Color</dt>
<dd>Green</dd>
</dl>

```
Long, single-line code blocks should not wrap. They should horizontally scroll if they are too long. This line should be long enough to demonstrate this.
```

```
The final element.
```
