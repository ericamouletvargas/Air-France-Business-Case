############################################################################## ################# Created By Team 4 ############################################ ################# Air France Team Assignment ################################### ################# Date: 11.06.2021 ############################################# ################# Version 1.0 ################################################## ############################################################################## ##############################################################################
## Importing our Air France and Kayak data ##
library(readxl)
Air_France <- read_excel("Downloads/Air France Case Spreadsheet Supplement.xls", sheet = "DoubleClick")
Kayak <- read_excel("Downloads/Air France Case Spreadsheet Supplement.xls",
                    sheet = "Kayak")
View(Air_France) View(Kayak)
## Creating a copy of Air France data set ##
Air_France_Copy <- Air_France
############################################################################## #### 1. Introducing the Kayak Data Set into the Air France data set ####
############################################################################## #Taking out unnecessary columns in Kayak Data Set and creating new variable "Kayak_new" # Kayak_new <- Kayak[-c(1,2,4,5),]
## Installing and Uploading "gapminder" and "dplyr" libraries ## install.packages("gapminder")
install.packages("dplyr")
library(gapminder) library(dplyr)
## Renaming the columns of the new Kayak data set ##
Kayak_new <- Kayak_new %>%
  rename(Search_Engine = `Sponsored Links - Air France`,
         Clicks = ...2, Media_Cost = ...3, Total_Bookings = ...4,
         Avg_Tickets = ...5, Total_Revenue = ...6, Net_Revenue = ...7)
## Converting the data from Character to Numeric for relevant variables ##
Kayak_new$Clicks <- as.numeric(Kayak_new$Clicks) Kayak_new$Media_Cost <- as.numeric(Kayak_new$Media_Cost) Kayak_new$Total_Bookings <- as.numeric(Kayak_new$Total_Bookings) Kayak_new$Avg_Tickets <- as.numeric(Kayak_new$Avg_Tickets) Kayak_new$Total_Revenue <- as.numeric(Kayak_new$Total_Revenue) Kayak_new$Net_Revenue <- as.numeric(Kayak_new$Net_Revenue)
## Creating a ROI Variable for Kayak data set ##
Kayak_new$ROI <- ((Kayak_new$Total_Revenue- Kayak_new$Media_Cost)/Kayak_new$Media_Cost)*100
## Binding and Organizing the data so that it matches the layout of the Air France data ##
Kayak_organized <- data.frame(cbind(Kayak_new$Search_Engine, Kayak_new$Media_Cost, Kayak_new$Total_Revenue, Kayak_new$Total_Bookings, Kayak_new$ROI, Kayak_new$Net_Revenue))
## Renaming the columns of the organized Kayak data ##
Kayak_organized <- Kayak_organized %>% rename(`Publisher Name` = X1,
                                              cost_sum = X2, amount_sum = X3, booking_sum = X4, ROI = X5, NetRevenue_sum = X6)
## Converting the organized data from Character to Numeric for relevant variables ##
Kayak_organized$cost_sum <- as.numeric(Kayak_organized$cost_sum) Kayak_organized$amount_sum <- as.numeric(Kayak_organized$amount_sum) Kayak_organized$booking_sum <- as.numeric(Kayak_organized$booking_sum) Kayak_organized$ROI <- as.numeric(Kayak_organized$ROI) Kayak_organized$NetRevenue_sum<- as.numeric(Kayak_organized$NetRevenue_sum)
############################################################################## #### 2. Cleaning the Air France data set ####
##############################################################################

## Identifying NAs from our Air France Data Set ##
is.na(Air_France)
## Categorizing NAs in "Bid Strategy" as "unknown" to proceed ##
Air_France$BidStrategy_cleared <- Air_France$`Bid Strategy` Air_France[["BidStrategy_cleared"]][is.na(Air_France[["BidStrategy_cleared"]])]<-"unknown"
## N/A is also found in "Match Type", so we clean this also and replaces with blanks ##
Air_France$MatchType_cleared <- gsub("N/A","",Air_France$`Match Type`)
############################################################################## #### 3. Aggregation of the Air France data set - First Analysis ####
############################################################################## ## Uploading the libraries that we need for aggregation ##
library(gapminder) library(dplyr)
## Creating the Net Revenue column ##
Air_France$NetRevenue <- Air_France$Amount - Air_France$`Total Cost`
## Aggregating the numerical data together, grouping by Publisher Name and Campaign ##
Air_F_table <- Air_France %>%
  group_by(`Publisher Name`) %>%
  select(`Total Cost`, `Publisher Name`, `Total Volume of Bookings`, Amount, NetRevenue)
%>%
  summarise(cost_sum =sum(`Total Cost`),
            amount_sum = sum(Amount),
            booking_sum = sum(`Total Volume of Bookings`), ROI=((amount_sum-cost_sum)/cost_sum)*100, NetRevenue_sum=sum(NetRevenue)) %>%
  arrange(desc(booking_sum))
Air_France_Campaign <- Air_France %>%
  group_by(Campaign) %>%
  select(`Total Cost`, Campaign, `Total Volume of Bookings`, Amount, NetRevenue) %>% summarise(cost_sum = sum(`Total Cost`),
                                                                                               amount_sum = sum(Amount),
                                                                                               booking_sum = sum(`Total Volume of Bookings`), ROI = ((amount_sum - cost_sum)/cost_sum)*100,
                                                                                               
                                                                                               Net_Rev_sum = sum(NetRevenue)) %>% arrange(desc(booking_sum))
## Compiling Air France search engine table with Kayak data set ## Complete_table <- tibble(rbind(Air_F_table,Kayak_organized))
## Plotting the data to analyise our results for Publishers ##
## Installing and Uploading the library for plots ## install.packages("ggplot")
library(ggplot2)
## Plotting Net Revenue (Y) according to the Publisher (X) ##
ggplot(data = Complete_table, aes(x=`Publisher Name`, y=NetRevenue_sum)) +geom_col()
## Plotting Booking (Y) according to the Publisher (X) ##
ggplot(data = Complete_table, aes(x=`Publisher Name`, y=booking_sum)) + geom_col() + coord_polar()
#Plotting the ROI (Y) according to the Publisher (X) ##
ggplot(data = Complete_table, aes(x=`Publisher Name`, y=ROI)) +geom_col()
## Plots show that people are using Google moreso, and would explain the higher Net Revenue for the company compared to the others ##
## We now dive into our second analysis ##
############################################################################## #### 4. Aggregation of the Air France data set - Second Analysis ####
##############################################################################
## Aggregate information with the Publisher "Google US" ##
Google_table <- Air_France %>% filter(`Publisher Name` == "Google - US") %>% select(`Clicks`, `Total Volume of Bookings`)
## Aggregate the numerical data together, grouping by keywords ## keywords <- Air_France %>%

group_by(Keyword) %>%
  select(Keyword, `Trans. Conv. %`, `Engine Click Thru %`, Amount, `Total Volume of Bookings`) %>%
  summarise(average_TCR = mean(`Trans. Conv. %`), average_CTR = mean(`Engine Click Thru %`), book_sum = sum(`Total Volume of Bookings`)) %>%
  arrange(desc(book_sum))
## Plotting Net Revenue (Y) according to the Publisher (X) ##
ggplot(data = Complete_table, aes(x=`Publisher Name`, y=`NetRevenue_sum`)) +geom_col() ggplot(data = Complete_table, aes(x=reorder(`Publisher Name`,-`NetRevenue_sum`), y=`NetRevenue_sum`, fill = `Publisher Name`)) +geom_col(show.legend = FALSE)
## Plotting Booking Sum (Y) according to the Publisher (X) ##
ggplot(data = Complete_table, aes(x=`Publisher Name`, y=booking_sum)) + geom_col() + coord_polar()
ggplot(data = Complete_table, aes(x=reorder(`Publisher Name`,-booking_sum) , y=booking_sum, fill = `Publisher Name`)) + geom_col(show.legend = FALSE) + coord_polar()
## Plotting the ROI (Y) according to the Publisher (X) ##
ggplot(data = Complete_table, aes(x=`Publisher Name`, y=ROI)) +geom_col()
ggplot(data = Complete_table, aes(x=reorder(`Publisher Name`,-ROI), y=ROI, fill = `Publisher Name`)) +geom_col(show.legend = FALSE)
## Aggregation of Digital Marketing Variable by Publisher ##
Air_F_Digital_Marketing <- Air_France %>%
  group_by(`Publisher Name`) %>%
  select(`Publisher Name`, `Trans. Conv. %`, `Engine Click Thru %`, `Total Volume of
         Bookings`) %>%
  summarise(average_TCR = mean(`Trans. Conv. %`),
            average_CTR = mean(`Engine Click Thru %`),
            book_sum = sum(`Total Volume of Bookings`)) %>% arrange(desc(average_TCR))
## Plotting the Digital Marketing Variable by Publisher ##
ggplot(data = Air_F_Digital_Marketing, aes(x=reorder(`Publisher Name`,-average_TCR), y=average_TCR, fill = `Publisher Name`)) +geom_col(show.legend = FALSE)
## Plots show that Kayak and Yahoo have better ROI compared to the likes of Google ## ## We now dive into our third analysis ##

############################################################################## #### 5. Creating a Word-Cloud for Keyword Analysis - Third Analysis ####
############################################################################## ## Installing and Uploading required libraries for Word-Cloud analysis ##
install.packages("tm") # for text mining install.packages("SnowballC") # for text stemming install.packages("wordcloud") # for wordcloud generator install.packages("RColorBrewer") # for the color palette install.packages("syuzhet") # for sentiment analysis install.packages("ggplot2") # for plotting graphs
library(tm) library(SnowballC) library(RColorBrewer) library(wordcloud) library(syuzhet) library(ggplot2)
## Reading and Loading the text file from local machine ## text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))
## Cleaning the text of special characters ##
toSpace <- content_transformer(function(x,pattern)gsub(pattern,"",x)) Textdocs <- tm_map(docs,toSpace, "/")
Textdocs <- tm_map(docs, toSpace, "@")
Textdocs <- tm_map(docs,toSpace, "\\|")
Textdocs <- tm_map(docs, content_transformer, tolower) # Converting the text to lower case symbols
Textdocs <- tm_map(docs, removeNumbers) # Removing numbers
Textdocs <- tm_map(docs, removeWords, stopwords("english")) # Removing english common stopwords
Textdocs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) # Specifying stopwords as a character vector
Textdocs <- tm_map(docs, removePunctuation) # Remove punctuations
Textdocs <- tm_map(docs, stripWhitespace) # Eliminating extra white spaces
Textdocs <- tm_map(docs, stemDocument) # Text stemming ## Building a Term Frequency matrix ##
TextDoc_dtm <- TermDocumentMatrix(Textdocs)

dtm_m <- as.matrix(TextDoc_dtm)
## Sorting by decreasing frequency ##
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE) dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
## Displaying the top 20 most frequent words ## head(dtm_d, 25)
## Plotting the most frequent words ##
barplot(dtm_d[1:30,]$freq, las = 2, names.arg = dtm_d[1:30,]$word, col ="lightgreen", main ="Top 30 most booked words",
        ylab = "Word frequencies")
## Generating Word Cloud ##
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, colors=brewer.pal(8, "Dark2"))