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
library(data.table)
```


```r
# SETTING UP FORMATTING 

kableFormat <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                full_width = FALSE)
}
```


```r
#Reading in the data for the functions 

c <- read_csv("cards.csv")
s <- read_csv("sets.csv")
c19 <- read_csv("mean_2019_prices.csv")
c20 <- read_csv("mtgMarketInfo.csv")
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

**COMPLETENESS AND EASE OF MANIPULATION** 


```r
data_quality <- function(df){
  
  "
  A function to quantitatively compute scores for a dataframe on the completeness and ease of manipulation data quality metrics. 
  
  Input: 
    df: A data frame
    
  Output: 
    qualityTable: A table reporting the scores on completeness and ease of manipulation for each column in the input data frame. 
  "
  
  # Setting the index value, which will be used to index the column name 

  index <- 1
  
  # Instantiating empty data frames for each of the queries
  
  completeness <- data.frame(Completeness=double())
  eom <- data.frame(Ease_of_Manipulation=double())
  names <- data.frame(ColumnName=character())
  
  # Populating the data frames using a for-loop
  
  for (i in df){
    
    # COLLECTING THE NAMES OF EACH COLUMN PASSED
    
    col <- colnames(df[index])
    
    # COMPLETENESS
    # Takes the sum of the total NA, NULL, and NaN values in a column
    # Divides them by the length of the column
    # Subtracts this from one, as was suggested by Pipinio, Lee, and Wang
    # And then rounds to output to the third decimal place
    
    c <- 1-(sum(is.na(i) + is.null(i) + is.nan(i))/length(i)) %>%
      round(digits = 3)
    
    # EASE OF MANIPULATION
    # "Case when" vectorises a series of if/else statements
    # The function checks the type of the column and then sets the variable,
    # e, to the corresponding value. 
    
    e <- case_when(
      typeof(i) == "logical" ~ 1,
      typeof(i) == "integer" ~ .9,
      typeof(i) == "numeric" ~ .8,
      typeof(i) == "double" ~ .8,
      typeof(i) == "complex" ~ .7,
      typeof(i) == "character" ~ .6,
      typeof(i) == "list" ~ .5, 
      typeof(i) == "raw" ~ 0,
      TRUE ~ 0)
    
    #The index used to collect column names is increased by one
    
    index = index + 1
    
    #Appending the output for each column to their respective data frames
    
    completeness[nrow(completeness)+1,] <- c
    eom[nrow(eom)+1,] <- e
    names[nrow(names)+1,] <- col
  }
  
  #Binding the columns of the three tables into an output table
  qualityTable <- cbind(names, completeness, eom)
  
  return(qualityTable)
}
```

## Quality Assessments
We assessed data quality using the metrics outlined in Pipino, Lee, and Wang (2002). For each of these metrics, we provide a brief commentary on how the data fared. Several of the data quality metrics were more pertinent to our analysis, so we provide deeper insight into them. For the subjective measures, each member of the research team produced a subjective score based off of their experience with working with the data. These scores were averaged to produce the score given by the team to the data as a whole. The objective measurements will be preformed on all four of the raw datasets used. 

1. **Accessibility**  
The data were fairly accessible; the card and set information could be directly downloaded as a CSV. Given our skill set, the JSON file used in the 2019 data was not difficult to gather. Collecting the 2020 market data presented a slight hurtle, but not so far as it made the data inaccessible. All of the raters provided similar scores, which was averaged for a total score of 7. 

2. **Believability**   
All of these data were originally web-scraped from either official information about the cards and the sets, the process for which is explained in detail on the [MTGJSON](https://mtgjson.com/faq/) website, or from the official Magic the Gathering card market itself. All of the raters provided similar scores, which were averaged for a score of 9.3. 

3. **Conciseness**  
Conciseness varied between data sets; the cards data provided extraneous and duplicate information. However, the other data sets were more streamlined because we had control over which variables we scraped or selected. All raters provided very similar scores for this metric, for an average score of 7.

4. **Consistent Representation**

The output of our objective function can be seen below. 


```r
cc <- con_rep(c)
cs <- con_rep(s)
c19c <- con_rep(c19)
c20c <- con_rep(c20)

con_vector <- c("Cards" = cc, "Sets" = cs, "Market Data 2019" = c19c, "Market Data 2020" = c20c)
  
con_vector %>% 
  kable("html", col.names="Score", escape = FALSE, caption = "Consistent Representation Scores") %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                full_width = FALSE)
```

<table class="table table-striped table-condensed table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-6)Consistent Representation Scores</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Score </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Cards </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sets </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Market Data 2019 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Market Data 2020 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>
5. **Completeness and Ease of Manipulation** 


```r
dc <- data_quality(c) %>%
  kableFormat("Completeness and EoM for Cards")
dc
```

<table class="table table-striped table-condensed table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-7)Completeness and EoM for Cards</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> ColumnName </th>
   <th style="text-align:right;"> Completeness </th>
   <th style="text-align:right;"> Ease_of_Manipulation </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> index </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> id </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> artist </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> asciiName </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> availability </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> borderColor </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cardKingdomFoilId </td>
   <td style="text-align:right;"> 0.506 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cardKingdomId </td>
   <td style="text-align:right;"> 0.762 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> colorIdentity </td>
   <td style="text-align:right;"> 0.890 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> colorIndicator </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> colors </td>
   <td style="text-align:right;"> 0.780 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> convertedManaCost </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> duelDeck </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> edhrecRank </td>
   <td style="text-align:right;"> 0.911 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> faceConvertedManaCost </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> faceName </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> flavorName </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> flavorText </td>
   <td style="text-align:right;"> 0.547 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> frameEffects </td>
   <td style="text-align:right;"> 0.047 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> frameVersion </td>
   <td style="text-align:right;"> 0.998 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hand </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hasAlternativeDeckLimit </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hasContentWarning </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hasFoil </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hasNonFoil </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isAlternative </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isFullArt </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isOnlineOnly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isOversized </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isPromo </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isReprint </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isReserved </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isStarter </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isStorySpotlight </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isTextless </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isTimeshifted </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> keywords </td>
   <td style="text-align:right;"> 0.362 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> layout </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> leadershipSkills </td>
   <td style="text-align:right;"> 0.063 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> life </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> loyalty </td>
   <td style="text-align:right;"> 0.014 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> manaCost </td>
   <td style="text-align:right;"> 0.872 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mcmId </td>
   <td style="text-align:right;"> 0.773 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mcmMetaId </td>
   <td style="text-align:right;"> 0.691 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mtgArenaId </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mtgjsonV4Id </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mtgoFoilId </td>
   <td style="text-align:right;"> 0.441 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mtgoId </td>
   <td style="text-align:right;"> 0.567 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> multiverseId </td>
   <td style="text-align:right;"> 0.737 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> name </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> number </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> originalReleaseDate </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> originalText </td>
   <td style="text-align:right;"> 0.722 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> originalType </td>
   <td style="text-align:right;"> 0.737 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> otherFaceIds </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> power </td>
   <td style="text-align:right;"> 0.461 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> printings </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> promoTypes </td>
   <td style="text-align:right;"> 0.102 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> purchaseUrls </td>
   <td style="text-align:right;"> 0.884 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rarity </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> scryfallId </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> scryfallIllustrationId </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> scryfallOracleId </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> setCode </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> side </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> subtypes </td>
   <td style="text-align:right;"> 0.608 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> supertypes </td>
   <td style="text-align:right;"> 0.134 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tcgplayerProductId </td>
   <td style="text-align:right;"> 0.878 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> text </td>
   <td style="text-align:right;"> 0.983 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> toughness </td>
   <td style="text-align:right;"> 0.461 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> type </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> types </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> uuid </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> variations </td>
   <td style="text-align:right;"> 0.143 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> watermark </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
</tbody>
</table>


```r
ds <- data_quality(s) %>%
  kableFormat("Completeness and EoM for Sets")
ds
```

<table class="table table-striped table-condensed table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-8)Completeness and EoM for Sets</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> ColumnName </th>
   <th style="text-align:right;"> Completeness </th>
   <th style="text-align:right;"> Ease_of_Manipulation </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> index </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> id </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> baseSetSize </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> block </td>
   <td style="text-align:right;"> 0.461 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> booster </td>
   <td style="text-align:right;"> 0.266 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> code </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isFoilOnly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isForeignOnly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isNonFoilOnly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isOnlineOnly </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> isPartialPreview </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> keyruneCode </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mcmId </td>
   <td style="text-align:right;"> 0.398 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mcmName </td>
   <td style="text-align:right;"> 0.398 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mtgoCode </td>
   <td style="text-align:right;"> 0.314 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> name </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> parentCode </td>
   <td style="text-align:right;"> 0.211 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> releaseDate </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tcgplayerGroupId </td>
   <td style="text-align:right;"> 0.507 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> totalSetSize </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> type </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
</tbody>
</table>

```r
dc19 <- data_quality(c19) %>%
  kableFormat("Completeness and EoM for the 2019 Market Data")
dc19
```

<table class="table table-striped table-condensed table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-9)Completeness and EoM for the 2019 Market Data</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> ColumnName </th>
   <th style="text-align:right;"> Completeness </th>
   <th style="text-align:right;"> Ease_of_Manipulation </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> uuid </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mtgo </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mtgoFoil </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> paper </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> paperFoil </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
</tbody>
</table>

```r
dc20 <- data_quality(c20) %>%
  kableFormat("Completeness and EoM for the 2020 Market Data")
dc20
```

<table class="table table-striped table-condensed table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-10)Completeness and EoM for the 2020 Market Data</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> ColumnName </th>
   <th style="text-align:right;"> Completeness </th>
   <th style="text-align:right;"> Ease_of_Manipulation </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Card Name </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rarity </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Serial no. </td>
   <td style="text-align:right;"> 0.798 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Market Price </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Buylist Price </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Listed Median </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Set Name </td>
   <td style="text-align:right;"> 1.000 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
</tbody>
</table>


6. **Reputation**   
As the data was scraped from official sources on the topic, the apparent reputation of the data is presumed to be high. Online research into public opinion on MTGJSON on the official Magic the Gathering Forum and the Magic the Gathering Reddit suggests that it has a very good reputation among data savvy MtG players and is thought to be very useful. The inter-rater scores for this category were very similar, and were considered in conjunction with a score produced for visible public opinion on the data. The reputation score was calculated as 8.75. 

7. **Timeliness**    
The timeliness of the data is variable. Though the 2019 data was pulled at the end of the year and is relatively recent, respective to other data sets on the internet, was not sufficient for all of our needs. This is why we pulled the 2020 data directly from the live website. The cards and sets data were up to date. The inter-rater scores were again very similar for this category. These data were attributed a timeliness score of 7.33. 





