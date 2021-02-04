library(readxl)
Air_France <- read_excel("Maestria MsBA/R/Air france busines case/Air France Case Spreadsheet Supplement (1).xls", 
                         sheet = "DoubleClick")
View(Air_France)

# Drop publisher ID is the same as the publisher Name ###
# Publisher Name convert into FACTOR ##
# keyword ID drop ####
# Match type Factor ####
# Bid Strategy drop ###
# keyword type Drop ###
# Status convert to FACTOR 

Air_France

colnames(Air_France)




 

library(dplyr)
library(tidyverse)

#install.packages("dplyr")
#install.packages("tidyverse")

Air_France <- select(Air_France, -`Publisher ID`)
Air_France <- select(Air_France, -`Keyword ID`)
Air_France <- select(Air_France, -`Bid Strategy`)
Air_France <- select(Air_France, -`Keyword Type`)


Air_France$factor_publisher_name <- as.factor(Air_France$`Publisher Name`)
Air_France$factor_match_type <- as.factor(Air_France$`Match Type`)
Air_France$factor_status <- as.factor(Air_France$Status)
Air_France$`Publisher Name`<-as.factor(Air_France$`Publisher Name`)

######### Word Graph ###########

# Install
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
# Load

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


text <- Air_France$Keyword
doc <- Corpus(VectorSource(text))
inspect(doc)

# Eliminate extra white spaces
docs <- tm_map(doc, stripWhitespace)

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, ",")
docs <- tm_map(docs, toSpace, ".")
docs <- tm_map(docs, toSpace, "2006::")


dtm <- TermDocumentMatrix(doc)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


########### Text Cleaning / Formatting ############
#install.packages("stringr")
library(stringr)

text_col_cleaner <- function(df,col_num) {
  new_col_vector <- c()
  for (i in 1:nrow(df)) {
    if (is.character(df[,col_num]) == TRUE) {
      clean_text <- str_replace_all(df[i,col_num], "[^[:alnum:]]", " ")
      clean_text <- tolower(clean_text)
      new_col_vector <- c(new_col_vector,clean_text)
    } else {
      next
    }#close if-else
  }#close for loop
  result_df <- as.data.frame(cbind(df, new_col_vector))
  return(result_df)
}#close func

####################################################

d<-text_col_cleaner(d,1)


word_graph <- set.seed(1234)

wordcloud(words = d$new_col_vector, freq = d$freq, min.freq = 4,
          random.order=FALSE,  
          colors=brewer.pal(8, "Dark2"))


findFreqTerms(dtm, lowfreq = 5)

#correlation between words
findAssocs(dtm, terms = "cheap", corlimit = 0.05)
findAssocs(dtm, terms = "paris", corlimit = 0.05)
findAssocs(dtm, terms = "ticket", corlimit = 0.05)
  


barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


#filtering first words 
first_words <- d[1:20,]

######## Analysis ##############

#Opimize roa 
#Return on advertisement spend 
#Revenue/Cost adquision 

# publisher Juan 
# campaign Mami
# Kewwords Li
# Bidding / ad position Yaiz 
# Match Type Arnitha


ROA 
sum(Air_France$Amount)/sum(Air_France$`Total Cost`)

6.172136
############## PUBLISHE ANALYSIS ########################
#publisher most trafic , revenue and cost 

publisher_l <- data.frame(Air_France$`Publisher Name`,Air_France$Clicks,Air_France$`Avg. Cost per Click`,Air_France$Impressions,
                   Air_France$Amount, Air_France$`Total Cost`,Air_France$`Total Volume of Bookings`)

colnames(publisher_l)

library(tidyverse)
publisher_l <- as_tibble(publisher_l)

names(publisher_l)[names(publisher_l) == "Air_France..Publisher.Name."] <- "Publisher_name"
names(publisher_l)[names(publisher_l) == "Air_France.Clicks"] <- "Clicks"
names(publisher_l)[names(publisher_l) == "Air_France..Avg..Cost.per.Click."] <- "Avg_cost_per_click"
names(publisher_l)[names(publisher_l) == "Air_France.Impressions"] <- "Impressions"
names(publisher_l)[names(publisher_l) == "Air_France..Total.Cost."] <- "Total_cost"
names(publisher_l)[names(publisher_l) == "Air_France.Amount"] <- "Amount"
names(publisher_l)[names(publisher_l) == "Air_France..Total.Volume.of.Bookings."] <- "Total_vol_bookings"


publisher_l$Publisher_name <- as.factor(publisher_l$Publisher_name)


### SUBSET BY FACTORS - ANALYZING INDIVIDUAL 

Google_Global   <- subset (publisher_l, Publisher_name == "Google - Global")
Google_US       <- subset (publisher_l, Publisher_name == "Google - US")
MSN_Global      <- subset (publisher_l, Publisher_name == "MSN - Global")
MSN_US          <- subset (publisher_l, Publisher_name == "MSN - US")
Overture_Global <- subset (publisher_l, Publisher_name == "Overture - Global")
Overture_US     <- subset (publisher_l, Publisher_name == "Overture - US")
yahoo_US        <- subset (publisher_l, Publisher_name == "Yahoo - US")



list_factor <- list(Google_Global,Google_US,MSN_Global,MSN_US,Overture_US,Overture_Global,yahoo_US)


###### ROA 

ROA_FUNCTION <-  function(df){
  
  ROA <- sum(df["Amount"])/sum(df["Total_cost"])
  
  return(ROA)
  
} # closing ROA_FUNCTION function

sum(Google_US$Amount)/sum(Google_US$Total_cost)

ROA_FUNCTION(Google_US)
ROA_FUNCTION(Google_Global)
ROA_FUNCTION(MSN_Global)
ROA_FUNCTION(MSN_US)
ROA_FUNCTION(Overture_Global)
ROA_FUNCTION(Overture_US)
ROA_FUNCTION(yahoo_US)

list_factor[1]

roa <- c()

Roa_df <- as.data.frame(matrix(ncol=3, nrow = 7))


for (i in list_factor){
  x <- ROA_FUNCTION(i)
  roa <- c(roa,x)
}


length(roa)


rownames(Roa_df) <- c("Google_Global","Google_US","MSN_Global","MSN_US","Overture_US","Overture_Global","yahoo_US")
name <- c("Google_Global","Google_US","MSN_Global","MSN_US","Overture_US","Overture_Global","yahoo_US")
publisher <- c("Google_Global","Google_US","MSN_Global","MSN_US","Overture_US","Overture_Global","yahoo_US")



amount <- c()


sum_func <-  function(df){
  
  X <- sum(df["Amount"])
  return(X)
}

for (i in list_factor){
  y <- sum_func(i)
  amount <- c(amount, y)
}


click_cost <- c()

colnames(MSN_Global)

cost_per_click <-  function(df){
  
  X <- sum(df["Avg_cost_per_click"])
  y<- x / length(df)
  return(y)
}


for (i in list_factor){
  z <- cost_per_click(i)
  click_cost <- c(click_cost, z)
}


Total_cost <- c()


sum_cost <-  function(df){
  
  X <- sum(df["Total_cost"])
  return(X)
}

for (i in list_factor){
  y <- sum_cost(i)
  Total_cost <- c(Total_cost, y)
}




Roa_df$roa_1 <- roa
Roa_df$names <- name
Roa_df$amount <- amount
Roa_df$cost_click <- click_cost
Roa_df$Total_cost <- Total_cost

Roa_df <- select(Roa_df, -V2)
Roa_df <- select(Roa_df, - V1)
Roa_df <- select(Roa_df, - V3)

#### plot roa by publisher ########
library(ggplot2)



roa_chart <-plot_ly(data=Roa_df, y=~publisher, x=~roa_1, color=~publisher)

roa_chart <- roa_chart %>% layout(title = "Roa By publisher")
roa_chart

library(plotly)

ggplotly(roa_chart)

### amount by publisher in thousands 

amount_chart <- plot_ly(data=Roa_df, y=~publisher, x=~amount/1000, color=~publisher)
amount_chart <- amount_chart %>% layout(title = "Revenue by publisher")
amount_chart



Total_cost <- plot_ly(data=Roa_df, y=~publisher, x=~Total_cost/1000, color=~publisher)

Total_cost <- Total_cost %>% layout(title = "Total cost by publisher")
Total_cost



#####################################
library(rockchalk)

Air_France_1 <- Air_France

Air_France_1$`Publisher Name`   <- combineLevels(Air_France_1$`Publisher Name`,levs = c("Google - US","MSN - US","Overture - US","Yahoo - US"), newLabel = c("US") )
Air_France_1$`Publisher Name`   <- combineLevels(Air_France_1$`Publisher Name`,levs = c("Google - Global","MSN - Global","Overture - Global"), newLabel = c("Global") )

Global   <- subset (Air_France_1, `Publisher Name` == "Global")
US       <- subset (Air_France_1, `Publisher Name` == "US")


summary(Global)
summary(US)

ROA_FUNCTION(Global)
ROA_FUNCTION(US)

#Geo target 
plot_ly(data=Air_France_1, x=~Clicks, y=~Amount, color=~`Publisher Name`)

#plot by publisher 
plot_ly(data=Air_France, x=~Clicks, y=~Amount, color=~`Publisher Name`)


library(ggplot2)
library(plotly)
#install.packages("gapminder")
library(gapminder)

# Most basic bubble plot - by publisher 
g <- ggplot(Air_France, aes(x=Clicks, y=Amount/1000, size = `Total Cost`,color= `Publisher Name`)) +
  geom_point(alpha=0.7) + ggtitle("Revenue vs Click by publisher") +
  theme(plot.title = element_text(color="Black", size=10, face="bold.italic"))


ggplotly(g)

# by geo target
geo <- ggplot(Air_France_1, aes(x=Clicks, y=Amount/1000, size = `Total Cost`,color= `Publisher Name`)) +
  geom_point(alpha=0.7) + ggtitle("US vs Global Revenue vs Click") +
  theme(plot.title = element_text(color="Black", size=10, face="bold.italic"))

ggplotly(geo)




