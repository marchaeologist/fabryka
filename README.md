fabryka
================

# [<img src="https://raw.githubusercontent.com/marchaeologist/fabryka/main/inst/www/hex_Fabryka4.png" height="250" align="center"/>](https://github.com/marchaeologist/fabryka)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15236695.svg)](https://doi.org/10.5281/zenodo.15236695)

## README

fabryka is an open access R *shiny* user-friendly web application
dedicated to the fabric analysis of archaeological or geological
assemblage. fabryka uses classical analytical procedures (Benn, rose,
Schmidt and Woodcock diagrams and statistical tests on orientations) for
analysing the fabrics of objects, an spatial method and new ways of
spatial and statistical exploration. A descriptive article is available
at <https://zenodo.org/records/15236695>.

## USE fabryka

The web application is available online at
<https://marchaeologist.shinyapps.io/fabryka/>.

You can also use fabryka in RStudio. To do so :

- Install [R](https://www.r-project.org) and [Rstudio
  Desktop](https://posit.co/download/rstudio-desktop/).

- clone the fabryka repository from GitHub:
  <https://github.com/marchaeologist/fabryka> or download the code in
  “Code” and “Download ZIP” at the same address. This will give you the
  latest version of the source code.

- Open fabryka.Rproj

- If you are using the application for the first time, you have to
  install several packages used in the application. Lists of all these
  packages and dependencies are available in a DESCRIPTION file and a
  DEPENDECIES file in the github repository. To install fabryka packages
  and dependencies, copy and paste the following code lines into the R
  console:

``` r
fabryka_packages = c("shiny", "dplyr","ggplot2","plotly", "ggsci", "ggtern", "circular", "CircStats", "Ternary", 
                   "RStoolbox", "shinyjs", "rmarkdown", "shinythemes",  "ggalt", "DT", "raster")

is_installed = sapply(fabryka_packages, require, character.only=T)

sapply(fabryka_packages[!is_installed], install.packages)
```

The application can be launched by opening the app.R file in the main
folder and pressing “Run App” or by running the following commands:

``` r
pkgload::load_all(".")

fabryka()
```

This can be useful if you want to use fabryka in the field without an
internet access.

## CONTACT

If you have problems, questions or suggestions, please contact me at
<marcthomas1@hotmail.fr> or via the fabryka repository issues page at
<https://github.com/marchaeologist/fabryka/issues>. All contributors to
the project are listed in the ‘Contributors’ panel in the application.

## SOURCE CODE

The source code is available on github:
<https://github.com/marchaeologist/fabryka>. You will also find previous
versions of the application archived there.

## TUTORIAL AND EXAMPLE DATASET

A tutorial is included in the application (*i.e.* ‘Tutorial” panel) and
can also be found in a dedicated article:
<https://zenodo.org/records/15236695>. ’fabryka’ includes an example
file you can download from the application (‘Upload data’ subpanel).

## NOTICE

This project is licensed under the
[MIT](https://choosealicense.com/licenses/mit/) License — Copyright (c)
2025 fabryka.

## CITATION

THOMAS, M. (2025). Fabryka: A web application for easily analysing &
exploring the fabric of archaeological assemblages. In PCI archaeology
(Version 1). Zenodo. <https://doi.org/10.5281/zenodo.15236695>
