# Text Analysis








```r
#READING IN THE DATA 

cmtg <- read_csv("cleanData_New.csv", col_names = TRUE)
cmtg <- cmtg[,-1] ; cmtg <- cmtg[,-4]
```


```r
# TOKENIZING SUBTYPES
cmtg$subtypes <- str_split(cmtg$subtypes, ",")

# TOKENIZING KEYWORDS
cmtg$keywords <- str_split(cmtg$keywords, ",")

# TURNING RARITY INTO A FACTOR 
cmtg$rarity <- factor(cmtg$rarity, levels = c("common", "uncommon", "rare", "mythic"), ordered = TRUE)

#FORMATTING POWER AND TOUGHNESS CORRECTLY
  # forces some to numeric, however upon investigation the cards turned to NA's are 'booster' cards, which are like spell cards
  # these cards can be identified by their key words

cmtg$power <- as.numeric(cmtg$power)
cmtg$toughness <- as.numeric(cmtg$toughness)
```


## Tidying the Text 


```r
# PRE-PROCESSING

# General cleaning
text_mtg <- distinct(cmtg, flavorText, .keep_all=TRUE)
text_mtg$allText <- paste(text_mtg$text, text_mtg$flavorText) 
text_mtg <- text_mtg[, !(names(text_mtg) %in% c('flavorText', 'text'))]
text_mtg$allText <- text_mtg$allText %>%
  removeNumbers() %>%
  str_replace_all("[^[:alnum:]]", " ") %>%
  stri_trans_tolower() %>%
  str_squish()

# Handling type
text_mtg$type <- text_mtg$type %>%
  removeNumbers() %>%
  str_replace_all("[^[:alnum:]]", " ") %>%
  stri_trans_tolower() %>%
  str_squish()

text_mtg <- text_mtg %>%
  mutate(type = str_remove_all(type, "legendary |snow |world | ongoing |basic ")) 

# Handling subtypes

text_mtg <- text_mtg %>%
  unnest(subtypes)

text_mtg$subtypes <- text_mtg$subtypes %>%
  str_replace_all("[^[:alnum:]]", " ") %>%
  stri_trans_tolower() %>%
  str_squish()
```



```r
text_mtg %>%
  select(type, subtypes, allText) %>%
  group_by(type, subtypes) %>%
  mutate(word_count = n()) %>%
  select(type, subtypes, word_count) %>% 
  distinct() %>%
  ungroup() %>%
  arrange(desc(word_count)) %>%
  filter(word_count > 100) %>%
  kableFormat("Three Sources Stats")
```

<table class="table table-striped table-condensed table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-6)Three Sources Stats</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> type </th>
   <th style="text-align:left;"> subtypes </th>
   <th style="text-align:right;"> word_count </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> instant </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 2177 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> human </td>
   <td style="text-align:right;"> 1881 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sorcery </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 1876 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enchantment </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 981 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> artifact </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 802 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> enchantment </td>
   <td style="text-align:left;"> aura </td>
   <td style="text-align:right;"> 706 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> soldier </td>
   <td style="text-align:right;"> 586 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> warrior </td>
   <td style="text-align:right;"> 581 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> wizard </td>
   <td style="text-align:right;"> 498 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> zombie </td>
   <td style="text-align:right;"> 355 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> goblin </td>
   <td style="text-align:right;"> 353 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> elf </td>
   <td style="text-align:right;"> 344 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> cleric </td>
   <td style="text-align:right;"> 332 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> elemental </td>
   <td style="text-align:right;"> 325 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> spirit </td>
   <td style="text-align:right;"> 324 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> land </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 321 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> shaman </td>
   <td style="text-align:right;"> 294 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> beast </td>
   <td style="text-align:right;"> 293 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> knight </td>
   <td style="text-align:right;"> 282 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> rogue </td>
   <td style="text-align:right;"> 216 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> bird </td>
   <td style="text-align:right;"> 196 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> merfolk </td>
   <td style="text-align:right;"> 195 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> vampire </td>
   <td style="text-align:right;"> 183 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> druid </td>
   <td style="text-align:right;"> 177 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> cat </td>
   <td style="text-align:right;"> 161 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> insect </td>
   <td style="text-align:right;"> 140 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> artifact </td>
   <td style="text-align:left;"> equipment </td>
   <td style="text-align:right;"> 133 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> scout </td>
   <td style="text-align:right;"> 129 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> wall </td>
   <td style="text-align:right;"> 128 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> giant </td>
   <td style="text-align:right;"> 127 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> horror </td>
   <td style="text-align:right;"> 125 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> dragon </td>
   <td style="text-align:right;"> 123 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> creature </td>
   <td style="text-align:left;"> angel </td>
   <td style="text-align:right;"> 121 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> artifact creature </td>
   <td style="text-align:left;"> construct </td>
   <td style="text-align:right;"> 110 </td>
  </tr>
</tbody>
</table>



```r
# ALL CARDS INFORMATION
all_cards <- text_mtg[23] %>%
  summarise(allText = paste(allText, collapse = ","))

# GROUPED BY TYPE COMBINATION

# Identifying significant types
n_occur_type <- data.frame(table(text_mtg$type))
significant_types <- n_occur_type %>%
  filter(n_occur_type$Freq > 50) %>%
  select(Var1)

type_cards <- aggregate(allText ~ type, text_mtg[c(4, 23)],paste,collapse="") %>%
  subset(type %in% as.vector(unlist(significant_types)))

# GROUPED BY SUBTYPE

# Identifying significant subtypes 

n_occur <- data.frame(table(text_mtg$subtypes))
significant_subtypes <- n_occur %>%
  filter(n_occur$Freq > 100) %>%
  select(Var1)

subtype_cards <- aggregate(allText ~ subtypes, text_mtg[c(5, 23)],paste,collapse="") %>%
  subset(subtypes %in% as.vector(unlist(significant_subtypes)))

# REMOVING TEMPORARY VARIABLES FROM THE ENVIRONMENT
remove(significant_subtypes, significant_types, n_occur, n_occur_type)
```


```r
# PREPARATION FUNCTION
prepare_text <- function(df){
  
  df <- df %>%
    unnest_tokens(word,allText)
  df <- df %>%
    anti_join(stop_words)
  
  return(df)

  }
```

## All cards


```r
all_cards_count <- prepare_text(all_cards) %>%
  mutate(word = lemmatize_words(word)) %>%
  count(word,sort=TRUE)
```

```
## Joining, by = "word"
```



```r
# TEXT STATISTICS 

mean_use <- mean(all_cards_count$n)
median_use <- median(all_cards_count$n)
```



```r
AllFreqBar <- all_cards_count%>%
  head(10)%>%
  ggplot(mapping = aes(x = reorder(word,n), n, text = paste("Count:", n))) + 
  geom_bar(stat = "identity",fill="lightblue", alpha = 0.90) +
 
 labs(title = "Top 10 Most Used Words on MTG Cards", x = "", y = "Frequency") +
  scale_y_continuous(breaks = pretty_breaks()) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major.x = element_line(size = 1.25),
        axis.text.x = element_text(size = 12, face = "bold"),
        text=element_text(size=16,  family="serif"), 
        axis.title.y = element_text(vjust=2),
        plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +
geom_hline(yintercept = mean_use, linetype="dotted", 
                color = "red", size=0.5) +
geom_hline(yintercept = median_use, linetype="dotted", 
                color = "black", size=0.5)

ggplotly(AllFreqBar, tooltip="text")
```

<!--html_preserve--><div id="htmlwidget-988dbf5eee8ecba02cd6" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-988dbf5eee8ecba02cd6">{"x":{"data":[{"orientation":"h","width":[0.899999999999999,0.899999999999999,0.899999999999999,0.9,0.9,0.9,0.9,0.9,0.9,0.9],"base":[0,0,0,0,0,0,0,0,0,0],"x":[15218,7902,6605,5467,3895,3626,3378,3245,3092,3053],"y":[10,9,8,7,6,5,4,3,2,1],"text":["Count: 15218","Count: 7902","Count: 6605","Count: 5467","Count: 3895","Count: 3626","Count: 3378","Count: 3245","Count: 3092","Count: 3053"],"type":"bar","marker":{"autocolorscale":false,"color":"rgba(173,216,230,0.9)","line":{"width":1.88976377952756,"color":"transparent"}},"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[18.0847910734004,18.0847910734004],"y":[0.4,10.6],"text":"","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(255,0,0,1)","dash":"dot"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[2,2],"y":[0.4,10.6],"text":"","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)","dash":"dot"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":51.7326691573267,"r":7.30593607305936,"b":51.0751349107513,"l":104.474885844749},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"serif","size":21.2536322125363},"title":{"text":"Top 10 Most Used Words on MTG Cards","font":{"color":"rgba(0,0,0,1)","family":"serif","size":25.5043586550436},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-760.9,15978.9],"tickmode":"array","ticktext":["0","5000","10000","15000"],"tickvals":[0,5000,10000,15000],"categoryorder":"array","categoryarray":["0","5000","10000","15000"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"serif","size":15.9402241594022},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":1.6604400166044,"zeroline":false,"anchor":"y","title":{"text":"Frequency","font":{"color":"rgba(0,0,0,1)","family":"serif","size":21.2536322125363}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,10.6],"tickmode":"array","ticktext":["fly","block","player","deal","damage","battlefield","control","card","target","creature"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["fly","block","player","deal","damage","battlefield","control","card","target","creature"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"serif","size":17.0029057700291},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"serif","size":21.2536322125363}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"serif","size":17.0029057700291}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"13207a5c7d1f":{"x":{},"y":{},"text":{},"type":"bar"},"13201b043024":{"yintercept":{}},"132040a37ac":{"yintercept":{}}},"cur_data":"13207a5c7d1f","visdat":{"13207a5c7d1f":["function (y) ","x"],"13201b043024":["function (y) ","x"],"132040a37ac":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


```r
# plot_gg(AllFreqBar, width=7, height=5, raytrace=TRUE, multicore=TRUE)
```

<img src="raytraced_plot.png" width="755" />


```r
all_cards_count %>%
  head(100) %>%
  e_charts() %>%
  e_cloud(word, n, shape='square') %>% 
  e_title("Highest frequency words", "All Magic the Gathering Cards") %>% 
  e_tooltip(trigger = "item") %>%
  e_theme("westeros") %>%
  e_color(background='#edf4f5')
```

<!--html_preserve--><div id="htmlwidget-b9bdf05980d6abb61b96" style="width:100%;height:500px;" class="echarts4r html-widget"></div>
<script type="application/json" data-for="htmlwidget-b9bdf05980d6abb61b96">{"x":{"theme":"westeros","tl":false,"draw":true,"renderer":"canvas","events":[],"buttons":[],"opts":{"series":[{"type":"wordCloud","data":[{"value":15218,"name":"creature"},{"value":7902,"name":"target"},{"value":6605,"name":"card"},{"value":5467,"name":"control"},{"value":3895,"name":"battlefield"},{"value":3626,"name":"damage"},{"value":3378,"name":"deal"},{"value":3245,"name":"player"},{"value":3092,"name":"block"},{"value":3053,"name":"fly"},{"value":2890,"name":"enter"},{"value":2881,"name":"life"},{"value":2804,"name":"spell"},{"value":2562,"name":"gain"},{"value":2436,"name":"cast"},{"value":2297,"name":"counter"},{"value":2246,"name":"sacrifice"},{"value":2223,"name":"attack"},{"value":2155,"name":"hand"},{"value":1990,"name":"draw"},{"value":1944,"name":"land"},{"value":1690,"name":"library"},{"value":1650,"name":"enchant"},{"value":1587,"name":"artifact"},{"value":1587,"name":"opponent"},{"value":1556,"name":"destroy"},{"value":1539,"name":"graveyard"},{"value":1415,"name":"return"},{"value":1374,"name":"tap"},{"value":1358,"name":"add"},{"value":1257,"name":"cost"},{"value":1233,"name":"begin"},{"value":1233,"name":"power"},{"value":1227,"name":"discard"},{"value":1154,"name":"mana"},{"value":1137,"name":"pay"},{"value":1115,"name":"create"},{"value":1091,"name":"combat"},{"value":1086,"name":"strike"},{"value":1051,"name":"token"},{"value":1025,"name":"time"},{"value":1009,"name":"lose"},{"value":983,"name":"ability"},{"value":965,"name":"choose"},{"value":964,"name":"exile"},{"value":919,"name":"untap"},{"value":906,"name":"die"},{"value":851,"name":"permanent"},{"value":844,"name":"upkeep"},{"value":804,"name":"owner"},{"value":802,"name":"goblin"},{"value":700,"name":"trample"},{"value":695,"name":"do"},{"value":692,"name":"step"},{"value":682,"name":"color"},{"value":680,"name":"equal"},{"value":671,"name":"haste"},{"value":629,"name":"black"},{"value":628,"name":"top"},{"value":617,"name":"reveal"},{"value":614,"name":"controller"},{"value":601,"name":"na"},{"value":582,"name":"activate"},{"value":569,"name":"world"},{"value":562,"name":"death"},{"value":550,"name":"leave"},{"value":541,"name":"white"},{"value":531,"name":"fight"},{"value":531,"name":"shuffle"},{"value":529,"name":"search"},{"value":506,"name":"reach"},{"value":500,"name":"regenerate"},{"value":496,"name":"forest"},{"value":491,"name":"enchantment"},{"value":482,"name":"defend"},{"value":452,"name":"band"},{"value":449,"name":"battle"},{"value":449,"name":"green"},{"value":449,"name":"prevent"},{"value":443,"name":"red"},{"value":440,"name":"protection"},{"value":434,"name":"vigilance"},{"value":430,"name":"war"},{"value":410,"name":"defender"},{"value":408,"name":"planeswalker"},{"value":389,"name":"warrior"},{"value":388,"name":"blood"},{"value":385,"name":"sorcery"},{"value":384,"name":"equip"},{"value":383,"name":"type"},{"value":379,"name":"soldier"},{"value":372,"name":"mind"},{"value":363,"name":"fear"},{"value":362,"name":"spirit"},{"value":361,"name":"dragon"},{"value":360,"name":"additional"},{"value":359,"name":"dead"},{"value":357,"name":"fall"},{"value":357,"name":"fire"},{"value":356,"name":"elf"}],"shape":"square"}],"title":[{"text":"Highest frequency words","subtext":"All Magic the Gathering Cards"}],"tooltip":{"trigger":"item"},"backgroundColor":"#edf4f5"},"dispose":true},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


## Characteristic Words by Type 



```r
type_count <- prepare_text(type_cards) %>%
  mutate(word = lemmatize_words(word)) %>%
  count(type, word,sort=TRUE)

type_tf_idf <- type_count %>%
  bind_tf_idf(word, type, n) %>%
  group_by(type)%>%
  select(-n)%>%
  arrange(desc(tf_idf))
```


```r
type_tf_idf_plot <- type_tf_idf %>%
  group_by(type) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~type, ncol = 1, scales = "free") +
  labs(title = "TF-IDF Results by Type", 
       x = "tf-idf", 
       y = NULL) +
  theme(axis.titleaxis.text=element_text(size=20),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(hjust = 0.5, vjust=10)) +
  theme_bw()
type_tf_idf_plot
```

<img src="07-Text-Analysis_files/figure-html/unnamed-chunk-15-1.png" width="960" />


## Characteristic Words by Subtype


```r
subtype_count <- prepare_text(subtype_cards) %>%
  mutate(word = lemmatize_words(word)) %>%
  count(subtypes, word,sort=TRUE)
```

```
## Joining, by = "word"
```

```r
subtype_tf_idf <- subtype_count %>%
  bind_tf_idf(word, subtypes, n) %>%
  group_by(subtypes)%>%
  select(-n)%>%
  arrange(desc(tf_idf))
```


```r
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(30)

subtype_tf_idf %>%
  group_by(subtypes) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = subtypes)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~subtypes, ncol = 1, scales = "free") +
  scale_fill_manual(values = mycolors) +
  labs(title="TF-IDF Report for Subtypes",
       subtitle = "", 
       x = "tf-id valuef", 
       y = NULL) + 
    theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14,face="bold")) 
```

<img src="07-Text-Analysis_files/figure-html/unnamed-chunk-17-1.png" width="960" />

## Words and Prices


```r
word_price <- text_mtg[19:23] %>%
  summarise(allText = allText, meanPrice = rowMeans(text_mtg[,19:22]))

word_price_count <- prepare_text(word_price) %>%
  group_by(word) %>%
  filter(n()>5) %>%
  summarise(mean_price_word = mean(meanPrice)) 
```

```
## Joining, by = "word"
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
word_price_count%>%
  arrange(desc((mean_price_word))) %>%
  head(10) %>%
  kableFormat("Words Associated with Greatest Mean Cost")
```

<table class="table table-striped table-condensed table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-18)Words Associated with Greatest Mean Cost</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> word </th>
   <th style="text-align:right;"> mean_price_word </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> joining </td>
   <td style="text-align:right;"> 114.66682 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> parents </td>
   <td style="text-align:right;"> 114.40149 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> explore </td>
   <td style="text-align:right;"> 113.85063 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> brother </td>
   <td style="text-align:right;"> 80.40931 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ccc </td>
   <td style="text-align:right;"> 77.14685 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sneak </td>
   <td style="text-align:right;"> 62.74305 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> de </td>
   <td style="text-align:right;"> 53.62400 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tocasia </td>
   <td style="text-align:right;"> 50.36793 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> seedling </td>
   <td style="text-align:right;"> 45.01387 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> clarity </td>
   <td style="text-align:right;"> 41.77917 </td>
  </tr>
</tbody>
</table>

```r
word_price_count%>%
  arrange((mean_price_word)) %>%
  head(10) %>%
  kableFormat("Words Associated with Lowest Mean Cost")
```

<table class="table table-striped table-condensed table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-18)Words Associated with Lowest Mean Cost</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> word </th>
   <th style="text-align:right;"> mean_price_word </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> american </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> asia </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> european </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fc </td>
   <td style="text-align:right;"> 0.0000000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> championship </td>
   <td style="text-align:right;"> 0.0075000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> riverlands </td>
   <td style="text-align:right;"> 0.0130357 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> chao </td>
   <td style="text-align:right;"> 0.0132246 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> jiang </td>
   <td style="text-align:right;"> 0.0227500 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> bureaucrats </td>
   <td style="text-align:right;"> 0.0230901 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> skirmisher </td>
   <td style="text-align:right;"> 0.0233333 </td>
  </tr>
</tbody>
</table>

```r
mean(word_price_count$mean_price_word)
```

```
## [1] 1.296415
```


