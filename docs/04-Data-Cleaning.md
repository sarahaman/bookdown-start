# Data Cleaning




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
keepCols <- c("artist","uuid", "mcmId", "mtgjsonV4Id", "multiverseId", "name", "artist", "type", "subtypes", "supertypes", "manaCost", "convertedManaCost", "keywords", "text", "flavorText",  "power", "toughness", "rarity", "edhrecRank", "isOnlineOnly")
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

###############################################
##########    ##    PRICES     ##    ##########
###############################################

# READING IN THE DATA

prices19 <- read_csv("mean_2019_prices.csv", col_names = TRUE)
mtg_prices19 <- inner_join(mtg, prices19, by = c("mtgjsonV4Id" = "uuid"))

columnOrder <- c("uuid", "mtgjsonV4Id", "name", "artist", "type", "subtypes", "supertypes", "keywords", "text", "flavorText", "power", "toughness", "rarity", "edhrecRank", "isOnlineOnly", "convertedManaCost", "genericManaCost", "manaColor", "colorManaCost", "setName", "releaseDate", "mtgo", "mtgoFoil", "paper", "paperFoil")
mtg_prices19 <- mtg_prices19[, columnOrder]

# write.csv(mtg_prices19, "cleanData_New.csv")
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
mtg$toughness <- as.numeric(mtg$toughness)
```


```r
head(mtg, 5) %>%
  kableFormat("Clean Magic the Gathering Data")
```

<table class="table table-striped table-condensed table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-4)Clean Magic the Gathering Data</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> artist </th>
   <th style="text-align:left;"> uuid </th>
   <th style="text-align:right;"> mcmId </th>
   <th style="text-align:left;"> mtgjsonV4Id </th>
   <th style="text-align:right;"> multiverseId </th>
   <th style="text-align:left;"> name </th>
   <th style="text-align:left;"> type </th>
   <th style="text-align:left;"> subtypes </th>
   <th style="text-align:left;"> supertypes </th>
   <th style="text-align:right;"> convertedManaCost </th>
   <th style="text-align:left;"> keywords </th>
   <th style="text-align:left;"> text </th>
   <th style="text-align:left;"> flavorText </th>
   <th style="text-align:right;"> power </th>
   <th style="text-align:right;"> toughness </th>
   <th style="text-align:left;"> rarity </th>
   <th style="text-align:right;"> edhrecRank </th>
   <th style="text-align:right;"> isOnlineOnly </th>
   <th style="text-align:left;"> genericManaCost </th>
   <th style="text-align:left;"> manaColor </th>
   <th style="text-align:left;"> colorManaCost </th>
   <th style="text-align:left;"> setName </th>
   <th style="text-align:left;"> releaseDate </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Rebecca Guay </td>
   <td style="text-align:left;"> 38513fa0-ea83-5642-8ecd-4f0b3daa6768 </td>
   <td style="text-align:right;"> 16413 </td>
   <td style="text-align:left;"> 1669af17-d287-5094-b005-4b143441442f </td>
   <td style="text-align:right;"> 130483 </td>
   <td style="text-align:left;"> Abundance </td>
   <td style="text-align:left;"> Enchantment </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> If you would draw a card you may instead choose land or nonland and reveal cards from the top of your library until you reveal a card of the chosen kind Put that card into your hand and put all other cards revealed this way on the bottom of your library in any order </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> rare </td>
   <td style="text-align:right;"> 1099 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> G </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stephen Daniele </td>
   <td style="text-align:left;"> b8a68840-4044-52c0-a14e-0a1c630ba42c </td>
   <td style="text-align:right;"> 16227 </td>
   <td style="text-align:left;"> 047d5499-a21c-5f5c-9679-1599fcaf9815 </td>
   <td style="text-align:right;"> 132072 </td>
   <td style="text-align:left;"> Academy Researchers </td>
   <td style="text-align:left;"> Creature </td>
   <td style="text-align:left;"> Human , Wizard </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> When Academy Researchers enters the battlefield you may put an Aura card from your hand onto the battlefield attached to Academy Researchers </td>
   <td style="text-align:left;"> They brandish their latest theories as warriors would wield weapons </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> uncommon </td>
   <td style="text-align:right;"> 12014 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> U </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> John Avon </td>
   <td style="text-align:left;"> 11727081-4070-56db-8162-b970dd7f94bc </td>
   <td style="text-align:right;"> 16511 </td>
   <td style="text-align:left;"> ee19938c-4007-58f1-8904-fae28007b422 </td>
   <td style="text-align:right;"> 129458 </td>
   <td style="text-align:left;"> Adarkar Wastes </td>
   <td style="text-align:left;"> Land </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> T Add C
T Add W or U Adarkar Wastes deals 1 damage to you </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> rare </td>
   <td style="text-align:right;"> 597 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> No Generic Cost </td>
   <td style="text-align:left;"> No Color </td>
   <td style="text-align:left;"> No Color Cost </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Roger Raupp </td>
   <td style="text-align:left;"> 9acf4d13-d68f-5fec-93b4-caf0a14725a5 </td>
   <td style="text-align:right;"> 16289 </td>
   <td style="text-align:left;"> 8774e18f-3752-5c06-af94-5da3960da9ed </td>
   <td style="text-align:right;"> 135206 </td>
   <td style="text-align:left;"> Afflict </td>
   <td style="text-align:left;"> Instant </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Target creature gets 11 until end of turn
Draw a card </td>
   <td style="text-align:left;"> One rarely notices a heartbeat save when it is stolen </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> common </td>
   <td style="text-align:right;"> 16583 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> B </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Christopher Moeller </td>
   <td style="text-align:left;"> 0dea6b2e-25bc-5c31-aa1b-a7690af6b853 </td>
   <td style="text-align:right;"> 16414 </td>
   <td style="text-align:left;"> 4e875bca-0c52-5d60-889d-1db67e261737 </td>
   <td style="text-align:right;"> 130525 </td>
   <td style="text-align:left;"> Aggressive Urge </td>
   <td style="text-align:left;"> Instant </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Target creature gets +1+1 until end of turn
Draw a card </td>
   <td style="text-align:left;"> The power of the wild concentrated in a single charge </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:left;"> common </td>
   <td style="text-align:right;"> 10248 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> G </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>
