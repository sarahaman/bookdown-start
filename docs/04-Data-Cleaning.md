# Data Cleaning

## Packages 


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
```

## The Cleaning Script

The script below reads in the csv's needed and, at the end, outputs a merged CSV file containing the card and 2019 prices data joined by applicable columns. The inline comments explain what the script is doing at each stage. This data will later be compared with the 2020 price data, which is fundamentally a very different data set, though it has some variables that can be directly compared. This will be expanded on during the analysis. 


```r
###############################################
##########    ###    CARDS    ###    ##########
###############################################

# READING IN THE DATA
cardsRaw <- read_csv("cards.csv", col_names = TRUE)
# head(cardsRaw)

#SELECTING RELEVANT COLUMNS
keepCols <- c("uuid", "mcmId", "mtgjsonV4Id", "multiverseId", "name", "artist", "type", "subtypes", "supertypes", "manaCost", "convertedManaCost", "keywords", "text", "flavorText",  "power", "toughness", "rarity", "edhrecRank", "isOnlineOnly")
cards <- select_(cardsRaw, .dots = keepCols)
# head(cards)

# CLEANING TYPES
cards$type <- word(cards$type, 1, sep=" —")

# REMOVING SPECIAL CHARACTERS FROM THE TEXT VARIABLES
cards$text <- str_replace_all(cards$text, "[[:punct:]]", "")
cards$flavorText <- str_replace_all(cards$flavorText, "[[:punct:]]", "")

# CLEANING THE MANA CLUSTERF**K
# If the manaCost is NA, replace it with Z so that it can be processed by str_detect. Converted back later.
cards$manaCost <- replace_na(cards$manaCost, "Z")

#Removing the brackets
cards$manaCost = str_replace_all(cards$manaCost, "[{}]", "")

#Creating a variable to hold the 'generic' mana cost
cards$genericManaCost <- NA

# Counting the generic mana cost for each row
  # If X is included in the data, that means the # of generic mana is determined by the game environment

c = 1
for (i in cards$manaCost){
  a = str_detect(i, ".*[0-9].*")
  if ( a == TRUE){
    if (substr(i, start = 1, stop = 1) == 'X'){
      cards$genericManaCost[c] <- "Determined in Game"
    } else {
    cards$genericManaCost[c] <- substr(i, start = 1, stop = 1) 
    }
  } else {
    cards$genericManaCost[c] <- "No Generic Cost"
  }
c = c + 1
}

# Cleaning manaCost to remove data pertaining to generic mana
cards$manaCost <- str_replace_all(cards$manaCost, 'X', '') %>%
  removeNumbers()

# HANDLING CARD COLOR
# Creating the color variable
cards$manaColor <- NA

# Splitting the letters so that they can be compared, these are put in a temporary variable 
cards$temp <- str_split(cards$manaCost, "")

# Identifying the mana color 
  # If the card deals with more than one type of mana, it is assigned to the category "Multiple Types"
c = 1
for (i in cards$temp){
  if (length(i) <= 1){
    cards$manaColor[c] <- i[1]
  } else if(i[1] == i[2]) {
    cards$manaColor[c] <- i[1]
  } else {
    cards$manaColor[c] <- "Multiple Types"
}
c = c+1
}

# Colored mana cost is calculated by simply taking the length of the manaCost string (i.e., WWW would be 3)
cards$colorManaCost <- nchar(cards$manaCost) 

#Handling NA's 
cards$manaColor[is.na(cards$manaColor)] <- "No Color"
cards$manaColor[cards$manaColor == 'Z'] <- "No Color"
cards$colorManaCost <- ifelse(cards$manaColor == "No Color", "No Color Cost", cards$colorManaCost)

#Removing the depreciated columns
cards <- subset(cards, select = -c(temp, manaCost))

###############################################
##########    ###    SETS     ###    ##########
###############################################

# READING IN THE DATA
setsRaw <- read_csv("sets.csv", col_names = TRUE)
# head(setsRaw)

#SELECTING RELEVANT COLUMNS
keepColsSets <- c("mcmId", "name", "releaseDate")
sets <- select_(setsRaw, .dots = keepColsSets) %>%
  rename(
    "setName" = "name"
  )
sets <- sets[!is.na(sets$mcmId), ]


###############################################
##########    ###    JOIN     ###    ##########
###############################################
# Joining cards with sets on MCM ID
mtg <- left_join(cards, sets, by = "mcmId")

# head(mtg)
# sum(is.na(mtg$releaseDate))


# write.csv(mtg, "cleanData.csv")
```

### Post-Download Formatting
Because certain facets of the data cannot be retained when the data is formatted as a .CSV, we produced a 'post-download formatting' block which can be copied and pasted in at the beginning of any R file using the above CSV to convert the data to the ideal format for manipulation. 


```r
# TOKENIZING SUBTYPES
mtg$subtypes <- str_split(mtg$subtypes, ",")

# TOKENIZING KEYWORDS
mtg$keywords <- str_split(mtg$keywords, ",")

# TURNING RARITY INTO A FACTOR 
mtg$rarity <- factor(mtg$rarity, levels = c("common", "uncommon", "rare", "mythic"), ordered = TRUE)

#FORMATTING POWER AND TOUGHNESS CORRECTLY
  # forces some to numeric, however upon investigation the cards turned to NA's are 'booster' cards, which are like spell cards
  # these cards can be identified by their key words
mtg$power <- as.numeric(mtg$power)
```

```
## Warning: NAs introduced by coercion
```

```r
mtg$toughness <- as.numeric(mtg$toughness)
```

```
## Warning: NAs introduced by coercion
```


```r
head(mtg, 20)
```

```
## # A tibble: 20 x 23
##    uuid  mcmId mtgjsonV4Id multiverseId name  artist type  subtypes supertypes
##    <chr> <dbl> <chr>              <dbl> <chr> <chr>  <chr> <list>   <chr>     
##  1 3851~ 16413 1669af17-d~       130483 Abun~ Rebec~ Ench~ <chr [1~ <NA>      
##  2 b8a6~ 16227 047d5499-a~       132072 Acad~ Steph~ Crea~ <chr [2~ <NA>      
##  3 1172~ 16511 ee19938c-4~       129458 Adar~ John ~ Land  <chr [1~ <NA>      
##  4 9acf~ 16289 8774e18f-3~       135206 Affl~ Roger~ Inst~ <chr [1~ <NA>      
##  5 0dea~ 16414 4e875bca-0~       130525 Aggr~ Chris~ Inst~ <chr [1~ <NA>      
##  6 4429~ 16290 13fbd62d-8~       135228 Agon~ Adam ~ Sorc~ <chr [1~ <NA>      
##  7 76dd~ 16228 2854f284-9~       129459 Air ~ Kev W~ Crea~ <chr [1~ <NA>      
##  8 76d4~ 16228 9d9ce25a-f~           NA Air ~ Kev W~ Crea~ <chr [1~ <NA>      
##  9 2972~ 16229 f9db3498-1~       129913 Amba~ Jim M~ Lege~ <chr [2~ Legendary 
## 10 5119~ 16351 fe4aa077-8~       134753 Anab~ Greg ~ Crea~ <chr [1~ <NA>      
## 11 0809~ 16351 067159f1-c~           NA Anab~ Greg ~ Crea~ <chr [1~ <NA>      
## 12 5f82~ 16165 ad41be73-5~       130550 Ance~ Pete ~ Crea~ <chr [2~ <NA>      
## 13 b7c1~ 16165 fcd5d3ab-d~           NA Ance~ Pete ~ Crea~ <chr [2~ <NA>      
## 14 57aa~ 16166 9eb2e54c-a~       129465 Ange~ Volka~ Crea~ <chr [1~ <NA>      
## 15 8fd4~ 16166 e2be2630-a~           NA Ange~ Volka~ Crea~ <chr [1~ <NA>      
## 16 da0a~ 16475 cf706172-3~       129466 Ange~ Alan ~ Arti~ <chr [1~ <NA>      
## 17 55bd~ 16167 8fb2ccd5-7~       129711 Ange~ Mark ~ Sorc~ <chr [1~ <NA>      
## 18 c565~ 16167 53438513-1~           NA Ange~ Mark ~ Sorc~ <chr [1~ <NA>      
## 19 3b77~ 16168 bef2dc94-7~       129710 Ange~ Jim M~ Ench~ <chr [1~ <NA>      
## 20 fadd~ 16169 cff007ed-d~       129671 Ange~ John ~ Crea~ <chr [1~ <NA>      
## # ... with 14 more variables: convertedManaCost <dbl>, keywords <list>,
## #   text <chr>, flavorText <chr>, power <dbl>, toughness <dbl>, rarity <ord>,
## #   edhrecRank <dbl>, isOnlineOnly <dbl>, genericManaCost <chr>,
## #   manaColor <chr>, colorManaCost <chr>, setName <chr>, releaseDate <date>
```
