################################## MA331 Homework 2 ######################################################

### Install Libraries/ call packages ###

library(dsEssex)
require(dsEssex)
library(tidytext)
require(tidytext)
library(dplyr)
require(dplyr)
require(stringr)
require(stringi)
require(formattable)
require(tidyr)
require(tidyverse)
require(ggpubr)



### Download Data ###

alice <- read.csv("C:/Users/Home/OneDrive/practical_w3/11_Alice's Adventures in Wonderland.csv")
class(alice) #look at class of object
head(alice, n=20) #quickly check imported df 
frank <- read.csv("C:/Users/Home/Downloads/84_Frankenstein")
class(frank)#look at class of object 
head(frank, n=20) #quick look at imported df 

## Drop gutenberg_id for both books:
alice <- subset(alice, select = -c(`gutenberg_id`)) #use subset function and select everything that isnt gutenberg id and create new df 
frank <- subset(frank, select = -c(`gutenberg_id`)) #use subset function and select everything that isnt gutenberg id and create new df

################################ Data Processing ######################################################## 


## Perform tokenisation using unnest_tokens function for words  
alice_df <- alice %>% 
  mutate_all(as.character)%>% 
  mutate(chapter = cumsum(str_detect(text,
                                         regex("^CHAPTER [\\dIVX]")))) %>%
  ungroup() #create alice_df by passing frank to mutate all to characters then using cumsum, str_detct and regex to count chapters

alice_df <- alice_df %>% 
  mutate_all(as.character)%>% ## pass the df object and mutate all to string and unnest 
  unnest_tokens(word, text)

#%>% ## pass the df object and mutate all to string and unnest
  # unnest_tokens(wrd, text)

frank_df <- frank %>% 
  mutate_all(as.character)%>% 
  mutate(letter = cumsum(str_detect(text,
                                     regex("^(Letter) [\\d]"))),
                                    chapter = cumsum(str_detect(text,
                                                                regex("^Chapter [\\dIVX]")))) %>%
  ungroup() #create frank_df by passing frank to mutate all to characters then using cumsum, str_detct and regex to count chapters and letters 

frank_df <- frank_df %>% 
  mutate_all(as.character) %>% ## pass the df object and mutate all to string and unnest 
  unnest_tokens(word, text)

head(alice_df, n=10) #display first 10
head(frank_df, n=10) #display first 10

dim(frank_df)
frank_df[5635:nrow(frank_df),1] <- 0 #return values to 0 once letters at start have finished 

alice_words <- as.data.frame(alice_df$word) #index object to create column vector with just tokens 
frank_words <- as.data.frame(frank_df$word) #index object to create column vector with just tokens 

## Remove contents from texts ###
alice_words <- alice_words[-c(1:88),] %>% as.data.frame() #index df to remove contents section 
alice_df <- alice_df[-c(1:88),] %>% as.data.frame()
frank_words <- frank_words[-c(1:67),] %>% as.data.frame() #index df to remove contents section 
frank_df <- frank_df[-c(1:67),] %>% as.data.frame()
# colnames(frank_words) <- "word" #rename colname


## Remove words with _ at the start or the end of the word for alice
colnames(alice_words) <- "word" #name column
a <- str_replace(alice_words$word, pattern = "_", "") #use str_replace to change words with _ at start
b <- str_replace(a, pattern = "_$", "")#use str_replace to change words with _ at end
alice_words <- as.data.frame(b) # as df
colnames(alice_words) <- "word" #rename
colnames(frank_words) <- "word"

### Break texts into chapters 
## Chapters alice 
alice_c1 <- alice_df %>% filter(chapter==1) #pass object to filter by chapter number 
alice_c2 <- alice_df %>% filter(chapter==2)
alice_c3 <- alice_df %>% filter(chapter==3)
alice_c4 <- alice_df %>% filter(chapter==4)
alice_c5 <- alice_df %>% filter(chapter==5)
alice_c6 <- alice_df %>% filter(chapter==6)
alice_c7 <- alice_df %>% filter(chapter==7)
alice_c8 <- alice_df %>% filter(chapter==8)
alice_c9 <- alice_df %>% filter(chapter==9)
alice_c10 <- alice_df %>% filter(chapter==10)
alice_c11 <- alice_df %>% filter(chapter==11)
alice_c12 <- alice_df %>% filter(chapter==12)

#chapters frank 
frank_l1 <- frank_df %>% filter(letter==1) #pass object to filter by chapter number 
frank_l2 <- frank_df %>% filter(letter==2)
frank_l3 <- frank_df %>% filter(letter==3)
frank_l4 <- frank_df %>% filter(letter==4)
frank_c1 <- frank_df %>% filter(chapter==1)
frank_c2 <- frank_df %>% filter(chapter==2)
frank_c3 <- frank_df %>% filter(chapter==3)
frank_c4 <- frank_df %>% filter(chapter==4)
frank_c5 <- frank_df %>% filter(chapter==5)
frank_c6 <- frank_df %>% filter(chapter==6)
frank_c7 <- frank_df %>% filter(chapter==7)
frank_c8 <- frank_df %>% filter(chapter==8)
frank_c9 <- frank_df %>% filter(chapter==9) 
frank_c10 <- frank_df %>% filter(chapter==10)
frank_c11 <- frank_df %>% filter(chapter==11)
frank_c12 <- frank_df %>% filter(chapter==12)
frank_c13 <- frank_df %>% filter(chapter==13)
frank_c14 <- frank_df %>% filter(chapter==14)
frank_c15 <- frank_df %>% filter(chapter==15)
frank_c16 <- frank_df %>% filter(chapter==16)
frank_c17 <- frank_df %>% filter(chapter==17)
frank_c18 <- frank_df %>% filter(chapter==18)
frank_c19 <- frank_df %>% filter(chapter==19)
frank_c20 <- frank_df %>% filter(chapter==20)
frank_c21 <- frank_df %>% filter(chapter==21)
frank_c22 <- frank_df %>% filter(chapter==22)
frank_c23 <- frank_df %>% filter(chapter==23)
frank_c24 <- frank_df %>% filter(chapter==24)

#number of chapters in alice 
n_chaps_alice <- as.numeric(alice_df$chapter) %>% max()

#number of chapters / letters in frank 
n_chaps_frank <- as.numeric(frank_df$chapter) %>% max()
n_let_frank <- as.numeric(frank_df$letter) %>% max()
t_chaps_frank <- n_chaps_frank + n_let_frank

#average length of chapter 
avg_c_len_alice <- nrow(alice_words)/n_chaps_alice
avg_c_len_frank <- nrow(frank_words)/(n_chaps_frank + n_let_frank)

############################################ Features of the text ###########################################

## total number of words in Alice in Wonderland (approx.) = 
num_words_alice <- nrow(alice_words) #assign numeric value to object for total words 

## Total number of words in Frankenstein (approx) = 
num_words_frank <- nrow(frank_words) #assign numeric value to object for total words 

## Contractions within the text 
t <- (str_detect(alice_words$word, "(?<=[a-z*])\\W")) 
contractions_alice <- sum(t)
tt <- (str_detect(frank_words$word, "(?<=[a-z*])\\W")) 
contractions_frank <- sum(tt)
contractions_alice
contractions_frank
## Contractions ratio 
alice_con_ratio <- contractions_alice/nrow(alice_words)
frank_con_ratio <- contractions_frank/nrow(frank_words) 

# ## Rename colname in df 
colnames(alice_words) <- "word" #name column 
colnames(frank_words) <- "word" #name column 

## Count most common words 
alice_words %>% count(word) %>% arrange(desc(n)) %>% head(n=10) #take alice_words, count words, arrange in descending order, display first 10. 
frank_words %>% count(word) %>% arrange(desc(n)) %>% head(n=10) #as above 

## Filter out stop words which do not contribute to sentiment analysis (joining words/uninteresting words)
alice_words_interest <- alice_words %>% filter(!word %in% stop_words$word) #pass alice_words object to filter out words not in stop words library, creating alice_words_interest object
frank_words_interest <- frank_words %>% filter(!word %in% stop_words$word)
alice_total_words_interest <- as.numeric(count(alice_words_interest)) #as numeric, assign to object
(alice_total_words_interest) #print
frank_total_words_interest <- as.numeric(count(frank_words_interest)) #as numeric, assign to object
(frank_total_words_interest) #print

## Ratio of words of intertest:total words 
alice_words_ratio <- as.numeric(alice_total_words_interest)/num_words_alice
frank_words_ratio <- as.numeric(frank_total_words_interest)/num_words_frank
frank_words_ratio

## Count most common words and display from highest down
alice_words_interest %>% count(word) %>% arrange(desc(n)) %>% head(n=10) #pass object to count function, count words, arrange in desending order, print top 10.
frank_words_interest %>% count(word) %>% arrange(desc(n)) %>% head(n=10) #pass object to count function, count words, arrange in desending order, print top 10.

## Create table with total words and total words interest 
a <- as.data.frame(c(num_words_alice, alice_total_words_interest, alice_words_ratio, n_chaps_alice, avg_c_len_alice, contractions_alice, alice_con_ratio))
a <- t(a)
b <- as.data.frame(c(num_words_frank, frank_total_words_interest, frank_words_ratio, t_chaps_frank, avg_c_len_frank, contractions_frank, frank_con_ratio))
b <- t(b)
c <- rbind(a,b)
c <- as.data.frame(c)
rownames(c) <- c("Alice in Wonderland", "Frankenstein") #name rows
colnames(c) <- c("Total Number of Words", "Total Words of Interest", "Ratio (interest words:total)", "no. chapters", "avg chapter length", "Total contractions", "Contractions Ratio") #name cols 


## Create table for words total/interest/chapters
formattable(c, list(
  `Total Words of Interest` = color_tile("lightblue", "lightblue"),
  `Total Number of Words` = color_tile("lightgreen", "lightgreen"),
  `Ratio (interest words:total)` = color_tile("aquamarine", "aquamarine"),
  `no. chapters` = color_tile("lightgoldenrod1", "lightgoldenrod1"),
  `avg chapter length` = color_tile("slategray2", "slategray2"),## use formattable to make table 
  `Total contractions` = color_tile("darkorchid1", "darkorchid1"),
  `Contractions Ratio` = color_tile("red", "red")))
 

## Looking at the length of words of interest for alice (longest)
long_words_alice <- lapply(alice_words_interest, function(x) x[order(nchar(x), decreasing = TRUE)]) #using lapply function on alice_words_interest object to order words by the number of characters from the largest to the smallest
long_words_alice <- as.data.frame(long_words_alice) #convert to df 
long_words_alice %>% head(n=10) #display 10 longest 
top_long_words_alice <- long_words_alice[1:10,1]


long_words_frank <- lapply(frank_words_interest, function(x) x[order(nchar(x), decreasing = TRUE)]) #using lapply function on frank_words_interest object to order words by the number of characters from the largest to the smallest
long_words_frank <- as.data.frame(long_words_frank) #convert to df 
long_words_frank %>% head(n=10) #display 10 longest
top_long_words_frank <- long_words_frank[1:10,1]

## Longest word for each text
alice_longest_word <- str_count(long_words_alice[1,]) #count longest word of each text from words of interest 
frank_longest_word <- str_count(long_words_frank[1,])

## Create object with words with 10 characters or more from words of interest 
no_words_10_alice <- long_words_alice %>% #pass object to filter and str_detect function to filter out words with 10 or characters 
  filter(str_detect(word, "[a-z]{10,}")) %>% 
  count(word, sort=TRUE) #count and sort 
no_words_10_alice %>% head(n=10) #display first 10 obs 
alice_10_words_total <- sum(no_words_10_alice$n)# make ratio so can be compared ## number of words >= 10
ratio_of_words_10_alice <- alice_10_words_total/alice_total_words_interest #>=10/total
ratio_of_words_10_alice #print ratio 


no_words_10_frank <- long_words_frank %>% #as above 
  filter(str_detect(word, "[a-z]{10,}")) %>% 
  count(word, sort=TRUE) 
no_words_10_frank %>% head(n=10)
frank_10_words_total <- sum(no_words_10_frank$n)   #make ratio so can be compared 
ratio_of_words_10_frank <- frank_10_words_total/frank_total_words_interest
ratio_of_words_10_frank

## mean length of word of interest for alice 
y <- lapply(alice_words_interest, str_length)
y <- as.data.frame(y)
mean_word_alice <- mean(y$word)
mean_word_alice

## mean length of words of interest frank 
q <- lapply(frank_words_interest, str_length)
q <- as.data.frame(q)
mean_word_frank <- mean(q$word)
mean_word_frank

### Create table for longest word, total words >= 10, ratio (>=10:total interest)
j <- as.data.frame(c(mean_word_alice, alice_longest_word, alice_10_words_total, ratio_of_words_10_alice))
j <- t(j)
k <- as.data.frame(c(mean_word_frank, frank_longest_word, frank_10_words_total, ratio_of_words_10_frank))
k <- t(k)
l <- rbind(j,k)
l <- as.data.frame(l)
rownames(l) <- c("Alice in Wonderland", "Frankenstein") #name rows
colnames(l) <- c("Average Word Length", "Longest Word", "Words >= 10", "Ratio (>=10/interest total)") #name cols 

formattable(l, list(
  `Average Word Length` = color_tile("darkorchid1", "darkorchid1"),
  `Longest Word` = color_tile("lightblue", "lightblue"),
  `Words >= 10` = color_tile("lightgreen", "lightgreen"),
  `Ratio (>=10/interest total)` = color_tile("orange", "orange")))


################################## Sentiment Analysis ###################################################

### Sentiment analysis using the respective libraries ###
## View lexcicons for analysis to establish which are suitable 

get_sentiments("bing") %>% head(n=10) #call sentiment library and display first 10 observations 

get_sentiments("afinn") %>% head(n=10) #call sentiment library and display first 10 observations 

get_sentiments("loughran") %>% head(n=10) #call sentiment library and display first 10 observations 

get_sentiments("nrc") %>% head(n=10) #call sentiment library and display first 10 observations 

bing <- get_sentiments("bing") %>% # join words in bing with their sentiment ready for use 
  select(word, sentiment)

### Examine overall sentiment in novels using all words and words of interest ###
## alice words of interest 
alice_words_sent_interest <- alice_words_interest %>% inner_join(bing, by = "word") #use inner_join to join relevant words from the interest words object with sentiment from bing  
alice_words_bing_pos <- alice_words_sent_interest %>% count(sentiment) #count sentiment column to reveal total pos/neg words 
(alice_words_bing_pos)
alice_words_bing_total <- sum(alice_words_bing_pos$n)

## all words from alice 
alice_words_sent_all <- alice_words %>% inner_join(bing, by = "word") 
alice_words_bing_pos_all <- alice_words_sent_all %>% count(sentiment)
(alice_words_bing_pos_all)

## frank words of interest 
frank_words_sent_interest <- frank_words_interest %>% inner_join(bing, by = "word") #use inner_join to join relevant words from the interest words object with sentiment from bing  
frank_words_bing_pos <- frank_words_sent_interest %>% count(sentiment) #count sentiment column to reveal total pos/neg words 
(frank_words_bing_pos)
frank_words_bing_total <- sum(frank_words_bing_pos$n)

## frank words all 
frank_words_sent_all <- frank_words %>% inner_join(bing, by = "word") #use inner_join to join relevant words from the interest words object with sentiment from bing  
frank_words_bing_pos_all <- frank_words_sent_all %>% count(sentiment) #count sentiment column to reveal total pos/neg words 
(frank_words_bing_pos_all)

## Ratio of negative words to positive words for words of interest bing
alice_ratio_sent_bing <- alice_words_bing_pos[2,2]/alice_words_bing_pos[1,2] #create ratio of the positive words of interest / negative words of interest to make a comparable value 
alice_ratio_sent_bing

frank_ratio_sent_bing <- frank_words_bing_pos[2,2]/frank_words_bing_pos[1,2] #create ratio of the positive words of interest / negative words of interest to make a comparable value 
frank_ratio_sent_bing 

## ratio of sentiment bing words to words of interest 
alice_ratio_sent_to_total_interest <- alice_words_bing_total/as.numeric(alice_total_words_interest)
frank_ratio_sent_to_total_interest <- frank_words_bing_total/as.numeric(frank_total_words_interest)

## Create table to show pos/neg bing words and ratio
d <- as.data.frame(c(alice_words_bing_pos[1,2], alice_words_bing_pos[2,2], alice_words_bing_total,  alice_ratio_sent_bing, alice_ratio_sent_to_total_interest))
d <- t(d)
e <- as.data.frame(c(frank_words_bing_pos[1,2], frank_words_bing_pos[2,2], frank_words_bing_total, frank_ratio_sent_bing, frank_ratio_sent_to_total_interest))
e <- t(e)
f <- rbind(d,e)
f <- as.data.frame(f)
rownames(f) <- c("Alice in Wonderland", "Frankenstein") #name rows
colnames(f) <- c("Negative Words", "Positive Words", "Total", "Ratio (pos:neg)", "Ratio (sent:interest)") #name cols 

## create table for sentiment analysis of bing 
formattable(f, list(
  `Negative Words` = color_tile("lightblue", "lightblue"),
  `Positive Words` = color_tile("lightgreen", "lightgreen"),
  `Total` = color_tile("grey", "grey"),
  `Ratio (pos:neg)` = color_tile("orange", "orange"),
  `Ratio (sent:interest)` = color_tile("red", "red")))## use formattable to make table 

## Sentiment using AFINN - prepare afinn
afinn <- get_sentiments("afinn") %>% ## join words in afinn  with their value ready for use 
  select(word, value)

## afinn sentiment analysis for alice 
alice_words_sent_interest_afinn <- alice_words_interest %>% inner_join(afinn, by = "word") #create object of alice words joined with their respective sentiment value from the afinn library with inner_join function 
alice_words_sent_interest_afinn #print
alice_words_afinn_pos <- alice_words_sent_interest_afinn %>% count(value) #count number of words with repsective score, create object 
alice_words_afinn_pos #print


## afinn for frank 
frank_words_sent_interest_afinn <- frank_words_interest %>% inner_join(afinn, by = "word") #create object of alice words joined with their respective sentiment value from the afinn library with inner_join function 
frank_words_sent_interest_afinn #print
frank_words_afinn_pos <- frank_words_sent_interest_afinn %>% count(value)
frank_words_afinn_pos #print


## bar chart for alice afinn
bb <- ggplot(data = alice_words_afinn_pos)+
  labs(title = "Sentiment for Alice in Wonderland (AFINN)", 
       x = "Values assigned to words (from -5 to 5)",
       y = "Number of words with AFINN value")+
  geom_bar(aes(x = value, y = n, fill=value),stat="identity", position ="dodge")+ 
  theme_bw()+ 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(hjust=0.1))+
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3,4))

## bar chart for frank afinn
aa <- ggplot(data = frank_words_afinn_pos)+
  labs(title = "Sentiment for Frankenstein (AFINN)", 
       x = "Values assigned to words (from -5 to 5)",
       y = "Number of words with AFINN value")+
  expand_limits(x = c(-4,4))+
  geom_bar(aes(x = value, y = n, fill=value), stat = "identity", position ="dodge")+ 
  theme_bw()+ 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(hjust=0.1))+
  scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4))

ggarrange(bb, aa)


################################ Sentiment Analysis with NRC ############################################

nrc <- get_sentiments("nrc") %>% ##join words in NRC  with their value ready for use 
  select(word, sentiment)

## alice
alice_words_interest_nrc <- alice_words_interest %>% inner_join(nrc, by ="word") 
head(alice_words_interest_nrc, n=10)
sentiment_count_alice_nrc <- alice_words_interest_nrc %>% count(sentiment) 


## plot bar chart alice
cc <- ggplot(data = sentiment_count_alice_nrc)+
  labs(title = "Sentiment Analysis Alice (NRC)", 
       x = "Sentiment",
       y = "Amount of Words")+
  geom_bar(aes(x = reorder(sentiment, n), y = n, fill = sentiment), stat="identity", position ="dodge")+ 
  theme_bw()+ 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=-40, hjust=0.1))
  

## frank 
frank_words_interest_nrc <- frank_words_interest %>% inner_join(nrc, by ="word") 
head(frank_words_interest_nrc, n=10)
sentiment_count_frank_nrc <- frank_words_interest_nrc %>% count(sentiment) 

## plot bar chart frank
dd<- ggplot(data = sentiment_count_frank_nrc)+
  labs(title = "Sentiment Analysis Frank (NRC)", 
       x = "Sentiment",
       y = "Amount of Words")+
  geom_bar(aes(x = reorder(sentiment, n), y = n, fill = sentiment), stat="identity", position ="dodge")+ 
  theme_bw()+ 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=-40, hjust=.1))

ggarrange(cc, dd)

############################################# SENTIMENT ANALYSIS BY CHAPTER WITH AFINN #########################################

## Assign AFINN to chapters of alice/frank 
#alice
alice_c1_af <- alice_c1 %>% inner_join(afinn, by = "word")
alice_c2_af <- alice_c2 %>% inner_join(afinn, by = "word")
alice_c3_af <- alice_c3 %>% inner_join(afinn, by = "word")
alice_c4_af <- alice_c4 %>% inner_join(afinn, by = "word")
alice_c5_af <- alice_c5 %>% inner_join(afinn, by = "word")
alice_c6_af <- alice_c6 %>% inner_join(afinn, by = "word")
alice_c7_af <- alice_c7 %>% inner_join(afinn, by = "word")
alice_c8_af <- alice_c8 %>% inner_join(afinn, by = "word")
alice_c9_af <- alice_c9 %>% inner_join(afinn, by = "word")
alice_c10_af <- alice_c10 %>% inner_join(afinn, by = "word")
alice_c11_af <- alice_c11 %>% inner_join(afinn, by = "word")
alice_c12_af <- alice_c12 %>% inner_join(afinn, by = "word")

#frank
frank_l1_af <- frank_l1 %>% inner_join(afinn, by="word")
frank_l2_af <- frank_l2 %>% inner_join(afinn, by="word")
frank_l3_af <- frank_l3 %>% inner_join(afinn, by="word")
frank_l4_af <- frank_l4 %>% inner_join(afinn, by="word")
frank_c1_af <- frank_c2 %>% inner_join(afinn, by="word")
frank_c2_af <- frank_c2 %>% inner_join(afinn, by="word")
frank_c3_af <- frank_c3 %>% inner_join(afinn, by="word")
frank_c4_af <- frank_c4 %>% inner_join(afinn, by="word")
frank_c5_af <- frank_c5 %>% inner_join(afinn, by="word")
frank_c6_af <- frank_c6 %>% inner_join(afinn, by="word")
frank_c7_af <- frank_c7 %>% inner_join(afinn, by="word")
frank_c8_af <- frank_c8 %>% inner_join(afinn, by="word")
frank_c9_af <- frank_c9 %>% inner_join(afinn, by="word")
frank_c10_af <- frank_c10 %>% inner_join(afinn, by="word")
frank_c11_af <- frank_c11 %>% inner_join(afinn, by="word")
frank_c12_af <- frank_c12 %>% inner_join(afinn, by="word")
frank_c13_af <- frank_c13 %>% inner_join(afinn, by="word")
frank_c14_af <- frank_c14 %>% inner_join(afinn, by="word")
frank_c15_af <- frank_c15 %>% inner_join(afinn, by="word")
frank_c16_af <- frank_c16 %>% inner_join(afinn, by="word")
frank_c17_af <- frank_c17 %>% inner_join(afinn, by="word")
frank_c18_af <- frank_c18 %>% inner_join(afinn, by="word")
frank_c19_af <- frank_c19 %>% inner_join(afinn, by="word")
frank_c20_af <- frank_c20 %>% inner_join(afinn, by="word")
frank_c21_af <- frank_c21 %>% inner_join(afinn, by="word")
frank_c22_af <- frank_c22 %>% inner_join(afinn, by="word")
frank_c23_af <- frank_c23 %>% inner_join(afinn, by="word")
frank_c24_af <- frank_c24 %>% inner_join(afinn, by="word")

## Create overall sentiment value by chapter 
# alice
a_c1_af <- mean(alice_c1_af$value)
a_c2_af <- mean(alice_c2_af$value)
a_c3_af <- mean(alice_c3_af$value)
a_c4_af <- mean(alice_c4_af$value)
a_c5_af <- mean(alice_c5_af$value)
a_c6_af <- mean(alice_c6_af$value)
a_c7_af <- mean(alice_c7_af$value)
a_c8_af <- mean(alice_c8_af$value)
a_c9_af <- mean(alice_c9_af$value)
a_c10_af <- mean(alice_c10_af$value)
a_c11_af <- mean(alice_c11_af$value)
a_c12_af <- mean(alice_c12_af$value)

# frank 
f_l1_af <- mean(frank_l1_af$value)
f_l2_af <- mean(frank_l2_af$value)
f_l3_af <- mean(frank_l3_af$value)
f_l4_af <- mean(frank_l4_af$value)
f_c1_af <- mean(frank_c1_af$value)
f_c2_af <- mean(frank_c2_af$value)
f_c3_af <- mean(frank_c3_af$value)
f_c4_af <- mean(frank_c4_af$value)
f_c5_af <- mean(frank_c5_af$value)
f_c6_af <- mean(frank_c6_af$value)
f_c7_af <- mean(frank_c7_af$value)
f_c8_af <- mean(frank_c8_af$value)
f_c9_af <- mean(frank_c9_af$value)
f_c10_af <- mean(frank_c10_af$value)
f_c11_af <- mean(frank_c11_af$value)
f_c12_af <- mean(frank_c12_af$value)
f_c13_af <- mean(frank_c13_af$value)
f_c14_af <- mean(frank_c14_af$value)
f_c15_af <- mean(frank_c15_af$value)
f_c16_af <- mean(frank_c16_af$value)
f_c17_af <- mean(frank_c17_af$value)
f_c18_af <- mean(frank_c18_af$value)
f_c19_af <- mean(frank_c19_af$value)
f_c20_af <- mean(frank_c20_af$value)
f_c21_af <- mean(frank_c21_af$value)
f_c22_af <- mean(frank_c22_af$value)
f_c23_af <- mean(frank_c23_af$value)
f_c24_af <- mean(frank_c24_af$value)

# Create df with values for graph 
# alice 
aaaa <- as.data.frame(c(a_c1_af, a_c2_af, a_c3_af, a_c4_af, a_c5_af, a_c6_af,
                        a_c7_af, a_c8_af, a_c9_af, a_c10_af, a_c11_af, a_c12_af))
bbbb <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
cccc <- cbind(aaaa, as.numeric(bbbb))
colnames(cccc) <- c("value", "chapter")

# frank
fff <- as.data.frame(c(f_l1_af, f_l2_af, f_l3_af, f_l4_af, f_c1_af, f_c2_af, f_c3_af, f_c4_af, f_c5_af, f_c6_af, f_c7_af, f_c8_af,
                       f_c9_af, f_c10_af, f_c11_af, f_c12_af, f_c13_af, f_c14_af, f_c15_af, f_c16_af, f_c17_af, f_c18_af, f_c19_af,
                       f_c20_af, f_c21_af, f_c22_af, f_c23_af, f_c24_af))
ggg <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)
hhh <- cbind(fff, ggg)
colnames(hhh) <- c("value", "chapter")


## Plot graph showing progression of sentiment through the novel 
#alice
eee <- ggplot(cccc, aes(x = chapter, y = value, fill = value < 0, ))+
  geom_bar( stat = "identity", show.legend = FALSE)+
  labs(x = "Chapter", y = "Sentiment", title = "Sentiment by Chapter Alice (AFINN)") +
  geom_hline(yintercept=0, color="blue")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12))
eee

#frank
kkk <-ggplot(hhh, aes(x = chapter, y = value, fill = value < 0, ))+
  geom_bar( stat = "identity", show.legend = FALSE)+
  labs(x = "Chapter", y = "Sentiment", title = "Sentiment by Chapter Frankenstein (AFINN)") +
  geom_hline(yintercept=0, color="blue")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28))
kkk
#plot in same figure
ggarrange(eee,kkk)

