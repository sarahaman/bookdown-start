# Data the Gathering

Magic the Gathering cards represent a constantly fluctuating ecosystem of distinct cards and market demand. New sets of cards are released multiple times a year, and since the features of the new cards influence how applicable older cards are to the evolving game, the prestige of the cards is constantly changing. The cards also exist in different mediums.There is an online version of Magic the Gathering which accompanies the classical printed card game. The cost of cards between these mediums fluctuates widely, as there is obviously a limited supply of printed cards. Given this information, we thought that it was necessary to examine up-to-date market prices about the cards. This is where we reached an impasse, data collection-wise. We built a scraper that could pull up-to-date market data off of the official Magic the Gathering card market but, because of the data included in the card market, scant information about the cards themselves could be pulled from this site. Detailed card information could be found at [MTGJSON](https://mtgjson.com/), a website dedicated to scraping current information about all of the Magic the Gathering Cards. However, the most recent market data they had was pulled at the end of 2019. Because the card markets fluctuate, we did not believe that data pulled in 2019 could provide us with the most accurate information about current trends between features of Magic the Gathering cards and their prices. So, we decided to use both of these data sets and combine them where we could.

The data from the cards was downloaded from MTGJSON in CSV format and cleaned in R. We combined information about the cards and the sets they belong to in order to provide a holistic view of the cards. You can download the data for the cards [here](https://github.com/sarahaman/MtG_Group_Project/blob/main/cards.csv) and for the sets [here](https://github.com/sarahaman/MtG_Group_Project/blob/main/sets.csv).

The market data was not immediately accessible in a format that could be transfered directly into R. Because the members of our team were unfamiliar with both web-scraping and JSON-wrangling in R, we utilized Python and MongoDB to access the raw data and transform into usable CSV’s. These CSV’s were then cleaned and formatted for analysis in R. For the sake of replicability, we have provided links to the annotated Python scripts used to convert the raw data into a format that could be processed in R. The raw data can be downloaded from the following Github Repository: [MtG_Group_Project](https://github.com/sarahaman/MtG_Group_Project). 


## 2019 Market Data

[Click here!](https://github.com/sarahaman/MtG_Group_Project/blob/main/tocsv.ipynb)

## 2020 Market Data

[Click here!](https://github.com/sarahaman/MtG_Group_Project/blob/main/MTG_market_webscrape.py)

## Attributes of the Data
Data Attributes Guide

* UUID: skew number of card.
* Name: The name of the card.
* Type: The type of card eg: (Creature, Sorcery, Land, etc.)
* Subtype(s): Applies to Creature Types, denotes what tribes or species a creature belongs to eg: (Merfolk, Goblin, Wizard, etc).
* Keywords: Ability keywords/commonly adopted game mechanics printed on a card eg: (Flying, Haste, Trample, etc.)
* Text: Longer description of all game mechanics belonging to the card.
* Flavor Text: Non-game mechanics based text that provides insight into the lore of the card.
* Power: The attack attribute belonging to cards with a creature type (an integer).
* Toughness: The defense/health attribute belonging to cards of the creature type (an integer).
* Rarity: The commonness or exclusivity of a card eg:(uncommon, rare, mythic rare, common).
* Converted Mana Cost: The total mana cost to play the card during a game. 
* Generic Mana Cost: How much unspecified mana a card costs.
* Mana Color: The color(s) of mana that a card costs.
* Color Mana Cost: The amount of colored mana that a card costs.
* Set Name: The name of the set in which the card was printed.
* Release Date: The date of release for a card.
* MTGO: Magic The Gathering Online card.
* MTGO Foil: Magic The Gathering Online foil (shiney) card.
* Paper: The card is physically printed on paper.
* Paper Foil: Physically printed foil (holographic) .


