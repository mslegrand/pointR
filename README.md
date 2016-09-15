# pointR

**pointR** is a *shiny-based* minimalist ide for R scripting using svgR
- ***Videos on pointR*** can be found at http://mslegrand.github.io/pointRmedia/.
- ***Related information*** can be found at http://mslegrand.github.io/svgR/.
- ***Rambling thoughts*** can be found on the wiki at https://github.com/mslegrand/pointR/wiki

## Some Points on PointR
-  pointR is a shiny server application
-  pointR uses the ACE editor for coding
-  pointR is intended to ease the writing of R scripts using svgR.
-  pointR is an attempt to bridge the gap between coding and point&click.
-  svgR bridges the gap between R and SVG

## Try Out
- runGitHub("pointR", "mslegrand)
  
# Installation
- install.packages("devtools")
- library(devtools)
- install_github("shiny", "rstudio")
- install_github("shinyAce", "trestletech")
- install_github("shinyjs", "daattali")
- install_github("svgR", "mslegrand")

# Reporting issues
Please report any bugs/issue in the 
[Github Issue tracker](https://github.com/mslegrand/pointR)

#Contributing
If you wish to contribute to the pointR project, now is the time. Thanks.

#Caution
To quote from TrestleTech: *As with any online application, it is a genuinely bad idea to allow arbitrary users to execute code on your server.* **BE CAREFUL!!**

#Licensing

pointR is released under MIT license. Copyright (c) 2016 mslegrand. All rights reserved.

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
