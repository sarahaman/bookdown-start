# Data Quality Metrics
First, we import the packages that we use in this section and read in the uncleaned data.

## Libararies

The libraries loaded in below acts as a list of all of the libraries that will be used in this document. 


```r
###############################################
##########    ###  LIBRARIES  ###    ##########
###############################################

library(tidyverse)
library(plotly)
library(ggplot2)
library(gganimate)
library(magick)
library(gifski)
library(png)
library(knitr)
library(PerformanceAnalytics)
library(ggpubr)
library(lubridate)
library(ggthemes)
library(extrafont)
library(tm)
library(tidytext)
library(textdata)
library(gridExtra)
library(scales)
library(wordcloud)
library(reshape2)
library(textstem)
library(RColorBrewer)
library(echarts4r)
library(devtools)
library(rayshader)
library(kableExtra)
library(stringi)
```


```r
# SETTING UP FORMATTING 

kableFormat <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                full_width = FALSE)
}
```

## The Functions

**CONSISTENT REPRESENTATION**


```r
con_rep <- function(df){
  "
  A function that quantitatively scores an input data frame on the consistancy of representation data quality metric.
  
  Input: 
    df: a data frame
  Output: 
    con_rep_score: A numeric score on consistency of representation ranging from 1 to 0, where 1 is perfectly consistent representation and 0 is inconsistent representation.
  "
  
  type = vector()
  for(i in 1:ncol(df)){
    col_type <- typeof(df[1,i])
    type[i] <- col_type
  }
  
  con_rep_score <- 1 - ((length(unique(type)) - 1)/6)
  return(con_rep_score)
}
```


## Quality Assessments
We assessed data quality using the metrics outlined in Pipino, Lee, and Wang (2002). For each of these metrics, we provide a brief commentary on how the data fared. Several of the data quality metrics were more pertinent to our analysis, so we provide deeper insight into them.
