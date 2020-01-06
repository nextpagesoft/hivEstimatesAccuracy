# Status

[![Travis-CI Build Status](https://travis-ci.org/nextpagesoft/hivEstimatesAccuracy.svg?branch=1.0)](https://travis-ci.org/nextpagesoft/hivEstimatesAccuracy.svg?branch=1.0)

# European Centre for Disease Prevention and Control

An agency of the European Union

# ECDC HIV Estimates Accuracy tool

The [ECDC](https://ecdc.europa.eu/en/home) HIV Estimates Accuracy Tool is an application that uses 
advanced statistical methods to correct for missing values in key HIV surveillance variables as well 
as for reporting delay.

The tool accepts case based HIV surveillance data prepared in a specific format.

The outputs include results from pre-defined analyses in the form of a report containing tables and 
graphs, and datasets, in which the adjustments have been incorporated and which may be exported for 
further analysis.

# Dependencies

## External tools

1. Latex - choises vary: [TinyTex](https://yihui.name/tinytex/), [Miktex](https://miktex.org/), 
  [Tex Live](https://www.tug.org/texlive/)

## R packages

1. data.table (>= 1.10.4-3)
2. foreign (>= 0.8-0)
3. grid (>= 3.0.0)
4. ggplot2 (>= 2.2.1)
5. graphics (>= 3.0.0)
6. jomo (>= 2.5-1)
7. knitr (>= 1.17)
8. mice (>= 2.46.0)
9. mitools (>= 2.3)
10. readxl (>= 1.0.0)
11. rmarkdown (>= 1.6)
12. shiny (>= 1.0.5)
13. shinycssloaders (>= 0.2.0)
14. shinydashboard (>= 0.6.1)
15. shinyjs (>= 0.9.1)
16. stats (>= 3.0.0)
17. utils (>= 3.0.0)
18. quantreg (>= 5.33)
19. yaml (>= 2.1.14)

plus dependencies these packages bring.

# License

See the [LICENSE](https://github.com/nextpagesoft/hivEstimatesAccuracy/blob/master/LICENSE) file for 
license rights and limitations (EUPL-1.2).
