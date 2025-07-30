fabryka
================

# [<img src="https://raw.githubusercontent.com/marchaeologist/fabryka/main/inst/www/hex_Fabryka4.png" height="250" align="center"/>](https://github.com/marchaeologist/fabryka)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.16601423.svg)](https://doi.org/10.5281/zenodo.16601423)

# Package dependencies

Find below a list of all packages used in fabryka.

``` r
fabryka_packages=c("shiny", "dplyr","ggplot2","plotly", "ggsci", "ggtern", "circular", "CircStats", "Ternary", 
                   "RStoolbox", "shinyjs", "rmarkdown", "shinythemes",  "ggalt", "DT", "raster")
print(fabryka_packages)
```

    ##  [1] "shiny"       "dplyr"       "ggplot2"     "plotly"      "ggsci"      
    ##  [6] "ggtern"      "circular"    "CircStats"   "Ternary"     "RStoolbox"  
    ## [11] "shinyjs"     "rmarkdown"   "shinythemes" "ggalt"       "DT"         
    ## [16] "raster"

Find in the code chunk below a way to install all the packages used in
fabryka.

``` r
fabryka_packages=c("shiny", "dplyr","ggplot2","plotly", "ggsci", "ggtern", "circular", "CircStats", "Ternary", 
                   "RStoolbox", "shinyjs", "rmarkdown", "shinythemes",  "ggalt", "DT", "raster")
is_installed=sapply(fabryka_packages,require,character.only=T)
sapply(fabryka_packages[!is_installed],install.packages)
```

Find below a list of all packages dependencies used in fabryka.

``` r
options(repos = c(CRAN = "https://cloud.r-project.org"))
dependencies <- tools::package_dependencies(fabryka_packages, db = available.packages())
print(dependencies)
```

    ## $shiny
    ##  [1] "methods"     "utils"       "grDevices"   "httpuv"      "mime"       
    ##  [6] "jsonlite"    "xtable"      "fontawesome" "htmltools"   "R6"         
    ## [11] "sourcetools" "later"       "promises"    "tools"       "cli"        
    ## [16] "rlang"       "fastmap"     "withr"       "commonmark"  "glue"       
    ## [21] "bslib"       "cachem"      "lifecycle"  
    ## 
    ## $dplyr
    ##  [1] "cli"        "generics"   "glue"       "lifecycle"  "magrittr"  
    ##  [6] "methods"    "pillar"     "R6"         "rlang"      "tibble"    
    ## [11] "tidyselect" "utils"      "vctrs"     
    ## 
    ## $ggplot2
    ##  [1] "cli"       "glue"      "grDevices" "grid"      "gtable"    "isoband"  
    ##  [7] "lifecycle" "MASS"      "mgcv"      "rlang"     "scales"    "stats"    
    ## [13] "tibble"    "vctrs"     "withr"    
    ## 
    ## $plotly
    ##  [1] "ggplot2"      "tools"        "scales"       "httr"         "jsonlite"    
    ##  [6] "magrittr"     "digest"       "viridisLite"  "base64enc"    "htmltools"   
    ## [11] "htmlwidgets"  "tidyr"        "RColorBrewer" "dplyr"        "vctrs"       
    ## [16] "tibble"       "lazyeval"     "rlang"        "crosstalk"    "purrr"       
    ## [21] "data.table"   "promises"    
    ## 
    ## $ggsci
    ## [1] "ggplot2"   "grDevices" "scales"   
    ## 
    ## $ggtern
    ##  [1] "ggplot2"      "compositions" "grid"         "gridExtra"    "gtable"      
    ##  [6] "latex2exp"    "MASS"         "plyr"         "scales"       "stats"       
    ## [11] "proto"        "utils"        "lattice"      "hexbin"       "methods"     
    ## [16] "rlang"       
    ## 
    ## $circular
    ## [1] "stats"   "boot"    "mvtnorm"
    ## 
    ## $CircStats
    ## [1] "MASS" "boot"
    ## 
    ## $Ternary
    ## [1] "PlotTools"     "RcppHungarian" "shiny"         "sp"           
    ## 
    ## $RStoolbox
    ##  [1] "caret"         "sf"            "terra"         "XML"          
    ##  [5] "dplyr"         "ggplot2"       "tidyr"         "reshape2"     
    ##  [9] "lifecycle"     "exactextractr" "Rcpp"          "methods"      
    ## [13] "magrittr"      "RcppArmadillo"
    ## 
    ## $shinyjs
    ## [1] "digest"   "jsonlite" "shiny"   
    ## 
    ## $rmarkdown
    ##  [1] "bslib"       "evaluate"    "fontawesome" "htmltools"   "jquerylib"  
    ##  [6] "jsonlite"    "knitr"       "methods"     "tinytex"     "tools"      
    ## [11] "utils"       "xfun"        "yaml"       
    ## 
    ## $shinythemes
    ## [1] "shiny"
    ## 
    ## $ggalt
    ##  [1] "ggplot2"      "utils"        "graphics"     "grDevices"    "dplyr"       
    ##  [6] "RColorBrewer" "KernSmooth"   "proj4"        "scales"       "grid"        
    ## [11] "gtable"       "ash"          "maps"         "MASS"         "extrafont"   
    ## [16] "tibble"       "plotly"      
    ## 
    ## $DT
    ## [1] "htmltools"   "htmlwidgets" "httpuv"      "jsonlite"    "magrittr"   
    ## [6] "crosstalk"   "jquerylib"   "promises"   
    ## 
    ## $raster
    ## [1] "sp"      "Rcpp"    "methods" "terra"

Please note that there are currently conflicts between the functions of
several packages. You will find a list below. This problem will be
addressed more carefully in future versions of the application. For now,
all functions have been prefixed by their namespace.

``` r
library(conflicted)
conflicted::conflict_scout(fabryka_packages)
```

    ## Registered S3 methods overwritten by 'ggtern':
    ##   method           from   
    ##   grid.draw.ggplot ggplot2
    ##   plot.ggplot      ggplot2
    ##   print.ggplot     ggplot2

    ## Registered S3 methods overwritten by 'ggalt':
    ##   method                  from   
    ##   grid.draw.absoluteGrob  ggplot2
    ##   grobHeight.absoluteGrob ggplot2
    ##   grobWidth.absoluteGrob  ggplot2
    ##   grobX.absoluteGrob      ggplot2
    ##   grobY.absoluteGrob      ggplot2

    ## 24 conflicts

    ## • `aes()`: ggplot2 and ggtern

    ## • `annotate()`: ggplot2 and ggtern

    ## • `dataTableOutput()`: shiny and DT

    ## • `deg()`: circular and CircStats

    ## • `ggplot_build()`: ggplot2 and ggtern

    ## • `ggplot_gtable()`: ggplot2 and ggtern

    ## • `ggsave()`: ggplot2 and ggtern

    ## • `layer_data()`: ggplot2 and ggtern

    ## • `pp.plot()`: circular and CircStats

    ## • `rad()`: circular and CircStats

    ## • `renderDataTable()`: shiny and DT

    ## • `rose.diag()`: circular and CircStats

    ## • `rstable()`: circular and CircStats

    ## • `runExample()`: shiny and shinyjs

    ## • `select()`: dplyr and raster

    ## • `theme_bw()`: ggplot2 and ggtern

    ## • `theme_classic()`: ggplot2 and ggtern

    ## • `theme_dark()`: ggplot2 and ggtern

    ## • `theme_gray()`: ggplot2 and ggtern

    ## • `theme_light()`: ggplot2 and ggtern

    ## • `theme_linedraw()`: ggplot2 and ggtern

    ## • `theme_minimal()`: ggplot2 and ggtern

    ## • `theme_void()`: ggplot2 and ggtern

    ## • `wind()`: plotly and circular

Below find the list of imported packaged by using the
usethis::use_package() command.

``` r
usethis::use_package("shiny")
usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("plotly")
usethis::use_package("ggsci")
usethis::use_package("ggtern")
usethis::use_package("circular")
usethis::use_package("CircStats")
usethis::use_package("Ternary")
usethis::use_package("RStoolbox")
usethis::use_package("shinyjs")
usethis::use_package("rmarkdown")
usethis::use_package("shinythemes")
usethis::use_package("ggalt")
usethis::use_package("DT")
usethis::use_package("raster")
```
