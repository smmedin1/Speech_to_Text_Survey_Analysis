library(textreadr)
library(textshape)
library(dplyr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(scales)
library(wordcloud)
library(igraph)
library(ggraph)
library(reshape2)
library(topicmodels)
library(quanteda)
library(tm)
library(ROCR)
library(caret)
library(stringr)
library(readr)
library(knitr)
library(lattice)
library(quanteda.textmodels)
library(shiny)

# PreProcessing -----------------------------------------------------------
par(mar=c(0,0,0,0))

#s<- read_document(file="/Users/sarahmedina/Documents/R/Text Analytics/Survey.docx")
s<- read_document(file="/Users/sarahmedina/Documents/R/Text Analytics/team 3/Survey.docx")
a <- 36 #count of people
b <- 6  #number of questions

my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- s[i*b+z-b]
  }#closing z loop
}#closing i loop

# Creating ID col to separate respondents
my_df <- my_df %>% 
  mutate(ID = seq(1:nrow(my_df)))

# Recoding of success - the person got covid (1)
# failure - the person has not had covid (0)
my_df[grep('yes|Yes',  my_df$V6), 'V6'] <- 1
my_df[grep('no|No',  my_df$V6), 'V6'] <- 0
my_df %>% melt(., 'ID') %>% View()

# Transforming DF to long format
melt(my_df,id.vars = c('ID','V6')) %>% 
  View('melted')


my_df <- melt(my_df,id.vars = c('ID','V6'))

# Changing column names - ID, final question (covid Y/N), question number, answer text
colnames(my_df) <- c('ID', 'Single', 'Question', 'Answer')

# Labelling the questions Q1, Q2 etc...
my_df$Question <- gsub('V','Q', my_df$Question)

my_df$Question <- gsub('Q1','How did you spend valentines day last year?',my_df$Question)
my_df$Question <- gsub('Q2','How do you wish to spend valentines day this year?',my_df$Question)
my_df$Question <- gsub('Q3','What is your horoscope and does it describe you?',my_df$Question)
my_df$Question <- gsub('Q4','Describe your ideal Saturday night with unlimited budget.',my_df$Question)
my_df$Question <- gsub('Q5','What is your spirit animal and why?',my_df$Question)

my_df$Question

################
#horiscope data frame
horiscope<- my_df %>%
  filter(Question=='What is your horoscope and does it describe you?')

horiscope<- as.data.frame(horiscope)
horiscope <- mutate_all(horiscope,funs(tolower))

horiscope<- horiscope%>%
  dplyr::mutate(Aries= as.integer(grepl('aries', Answer)))%>%
  dplyr::mutate(Taurus= as.integer(grepl('taurus', Answer)))%>%
  dplyr::mutate(Gemini= as.integer(grepl('gemini', Answer)))%>%
  dplyr::mutate(Cancer= as.integer(grepl('cancer', Answer)))%>%
  dplyr::mutate(Leo= as.integer(grepl('leo', Answer)))%>%
  dplyr::mutate(Virgo= as.integer(grepl('virgo', Answer)))%>%
  dplyr::mutate(Libra= as.integer(grepl('libra', Answer)))%>%
  dplyr::mutate(Scorpio= as.integer(grepl('scorpio', Answer)))%>%
  dplyr::mutate(Sagittarius= as.integer(grepl('sagittarius', Answer)))%>%
  dplyr::mutate(Capricorn= as.integer(grepl('capricorn', Answer)))%>%
  dplyr::mutate(Aquarius= as.integer(grepl('aquarius', Answer)))%>%
  dplyr::mutate(Pisces= as.integer(grepl('pisces', Answer)))%>%
  dplyr::mutate(Dont_know= as.integer(grepl("diary| want to see| impossible", Answer)))


horiscope<- select(horiscope, -Question)
horiscope<- select(horiscope, -Answer)

horiscope_plot <- horiscope %>% 
  group_by(Single) %>% 
  summarise(Total = n(),
            Aries = sum(Aries == 1),
            Taurus = sum(Taurus == 1),
            Gemini = sum(Gemini == 1),
            Cancer = sum(Cancer == 1),
            Leo = sum(Leo == 1),
            Virgo = sum(Virgo == 1),
            Libra = sum(Libra == 1),
            Scorpio = sum(Scorpio == 1),
            Sagittarius = sum(Sagittarius == 1),
            Capricorn = sum(Capricorn == 1),
            Aquarius = sum(Aquarius == 1),
            Pisces = sum(Pisces == 1),
            Dont_know = sum(Dont_know == 1)) %>% 
  gather( "category", "counts", -Single)


ggplot(horiscope_plot, aes(Single, counts)) + 
  geom_col(aes(fill = category), position = "dodge")

############
cust_stop<- data_frame(word= c("day","animal","night","valentine's","spirit",
                               "spend","horoscope","saturday","budget","idea",
                               "unlimited","ideal","align","describe","yeah",
                               "horoscopes", "aligns", "gonna", "spent", "limited",
                               "aligned", "remember"),
                       lexicon = rep("cust", each =22))

#single
my_df_single<- my_df %>%
  filter(Single==1) %>% # SINGLE!
  unnest_tokens(word, Answer) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) %>%
  count(word, sort = TRUE)

singlesent <-my_df_single %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

single_wc<-singlesent %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Single sentiment", x=NULL)+
  labs(x=NULL, y="")+
  coord_flip()

#not single
my_df_not<- my_df %>%
  filter(Single==0) %>% # SINGLE!
  unnest_tokens(word, Answer) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop) %>%
  count(word, sort = TRUE)

not_singlesent <-my_df_not %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

notsingle_wc<- not_singlesent %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Single sentiment", x=NULL)+
  labs(x=NULL, y="")+
  coord_flip()

s_wordcloud = list(single = list(my_df_single, single_wc ),
                   notSingle = list(my_df_not, notsingle_wc))

choices_data_wordcloud <- names(s_wordcloud)

###########################
bigram_data <- my_df %>%
  unnest_tokens(bigram, Answer, token = "ngrams", n= 2)

bigram_data %>%
  count(bigram, sort = TRUE) 

bigrams_separated <- bigram_data %>%
  separate(bigram, c("word1", "word2"), sep = " ") # split them into word1 and word2

# filter stop words in both words - should not be stop word (not in stop words)
## -----------------------------------------------------------------------------
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word)%>%
  filter(!word2 %in% cust_stop$word)%>%
  filter(!word1 == "na|NA") %>%
  filter(!word2 == "na|NA")

# bigram, "no-stop-words" for single respondents
## -----------------------------------------------------------------------------
bigram_positive <- bigrams_filtered %>%
  filter(Single == "1") %>%
  count(word1, word2, sort = TRUE)

single_bigram<- ggraph(bigram_positive, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)+
  ggtitle("Token Network for Respondents who are SINGLE")

# Bigram, "no-stop-words" for non single respondents
## -----------------------------------------------------------------------------
bigram_negative <- bigrams_filtered %>%
  filter(Single == "0") %>%
  count(word1, word2, sort = TRUE)

notSingle_bigram <- ggraph(bigram_negative, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)+
  ggtitle("Token Network for Respondents who are NOT SINGLE")

s_bigram = list(single = list(bigram_positive, single_bigram),
                notSingle = list(bigram_negative, notSingle_bigram))

# Channel TFIDF  ----------------------------------------------------------------

# Data frame for section 

all_merged<- my_df %>% 
  unnest_tokens(word, Answer) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, Single, Question, sort = TRUE) %>% 
  bind_tf_idf(word, Question, n) %>% 
  arrange(desc(tf_idf)) %>%
  top_n(10) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=Single))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~Single, ncol=2, scales="free")+
  labs(x=NULL, y="tf-idf")+
  coord_flip()


# Single _____________

# Filter single people
all_merged_single <- all_merged %>% 
  filter(Single == 1)

# TFIDF Creation
all_merged_single <- all_merged_single %>%
  bind_tf_idf(word, Question, n) 


# GGPLOT of Single people
tfidf_single_plot <- all_merged_single %>%
  arrange(desc(tf_idf)) %>%
  top_n(10) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(Single) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=Single))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~Single, ncol=2, scales="free")+
  labs(x=NULL, y="tf-idf")+
  coord_flip()

print(tfidf_single_plot)
# Not Single ____________

# Filter not single people 
all_merged_nsingle <- all_merged %>% 
  filter(Single == 0)

# Create TFIDF
all_merged_nsingle <- all_merged_nsingle %>%
  bind_tf_idf(word, Question, n) 


# GGPLOT not single people

tfidf_nsingle_plot<- all_merged_nsingle %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(Single) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=Single))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~Single, ncol=2, scales="free")+
  labs(x=NULL, y="tf-idf")+
  coord_flip()
print(tfidf_nsingle_plot)


l_tfidf = list(single = list(all_merged_single, tfidf_single_plot),
               notSingle = list(all_merged_nsingle, tfidf_nsingle_plot))

###########################
# Naive Bayes

# Tokenize and remove stopwords
naive_df <- my_df %>% 
  unnest_tokens(word, Answer) %>% 
  anti_join(stop_words) 



# Random the order before put it in the model
# Set seed
set.seed(212)

# create corpus
opinions = Corpus(VectorSource(naive_df$word))


msg.dfm <- dfm(corpus(opinions), tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 1, min_docfreq = 1)

msg.dfm <- dfm_weight(msg.dfm)


NB_classifier <- textmodel_nb(msg.dfm, naive_df[,'Single'])
summary(NB_classifier)

str(NB_classifier)



#Shiny Widgets ------------------------------------------------------

library(shinyWidgets)
library(shinydashboard)
shinyWidgetsGallery()


#Server---------------------------------------------------------------
library(shiny) # loading shiny
library(shinythemes)

library(plotly)
library(markdown)

ui <- fluidPage(
  titlePanel("Valentines Day"),
  h4("Team 9 Text Analytics"),
  br(), br(),
  theme = shinythemes::shinytheme('cerulean'),
  navbarPage("",
             tabPanel("Cloud",
                      sidebarLayout(
                        sidebarPanel(
                          pickerInput(
                            inputId = "picker",
                            label = "Select Dataset", 
                            choices = c("Single" = 1, "Taken" = 0),
                            width = "auto")
                        ),
                        
                        mainPanel(
                          plotOutput("bigram")
                        )
                      )
             ), 
             
             tabPanel("Ngram TF-IDF",
                      sidebarLayout(
                        sidebarPanel(
                          pickerInput(
                            inputId = "picker_tf",
                            label = "Select Dataset", 
                            choices = c("Single" = 1, "Taken" = 0),
                            width = "auto"),
                          
                          sliderInput("range", "TF_IDF Range:",
                                      min = 0.01, max = 0.10,
                                      step = 0.001,
                                      value = c(0.03,0.5)
                          ),
                          
                          selectInput("question", "Choose a Question", choices = unique(my_df$Question))
                          
                        ),
                        
                        mainPanel(
                          plotOutput("bzodiac")
                        )
                      )
             ),
             
             tabPanel("Bigram TF-IDF",
                      sidebarLayout(
                        sidebarPanel(
                          pickerInput(
                            inputId = "picker_bi",
                            label = "Select Dataset", 
                            choices = c("Single" = 1, "Taken" = 0),
                            width = "auto"),
                          
                          sliderInput("range_bi", "TF_IDF Range:",
                                      min = 0.08, max = 0.32,
                                      step = 0.001,
                                      value = c(0.03,0.5)
                          ),
                          
                          selectInput("question_bi", "Choose a Question", choices = unique(my_df$Question))
                          
                        ),
                        
                        mainPanel(
                          plotOutput("bigram_tf")
                        )
                      )
             ),
             
             tabPanel("Zodiac",
                      sidebarLayout(
                        sidebarPanel(
                          pickerInput(
                            inputId = "picker_zodiac",
                            label = "Select Dataset", 
                            choices = c("Single" = 1, "Taken" = 0),
                            width = "auto")
                          
                        ),
                        
                        mainPanel(
                          plotOutput("zodiac")
                        )
                      )
             ),
             
             tabPanel("Naive Bayes",
                      verbatimTextOutput("naive")
             )
  )
) # Closing the UI




server <- function(input, output, session){
  
  
  output$bigram <- renderPlot({
    
    
    bigrams_filtered %>%
      filter(Single == input$picker) %>%
      count(word1, word2, sort = TRUE) %>% 
      ggraph( layout = "fr") +
      geom_edge_link()+
      geom_node_point()+
      geom_node_text(aes(label=name), vjust =1, hjust=1)
    
    
  })
  
  output$zodiac <- renderPlot({
    horiscope_plot <- horiscope %>% 
      filter(Single == input$picker_zodiac) %>% 
      summarise(Total = n(),
                Aries = sum(Aries == 1),
                Taurus = sum(Taurus == 1),
                Gemini = sum(Gemini == 1),
                Cancer = sum(Cancer == 1),
                Leo = sum(Leo == 1),
                Virgo = sum(Virgo == 1),
                Libra = sum(Libra == 1),
                Scorpio = sum(Scorpio == 1),
                Sagittarius = sum(Sagittarius == 1),
                Capricorn = sum(Capricorn == 1),
                Aquarius = sum(Aquarius == 1),
                Pisces = sum(Pisces == 1),
                Dont_know = sum(Dont_know == 1)) %>% 
      gather( "category", "counts", -Total)
    
    ggplot(horiscope_plot, aes( category, counts)) + 
      geom_col(aes(fill = category), position = "dodge") + 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  })
  
  output$bzodiac <- renderPlot({
    my_df %>% 
      unnest_tokens(word, Answer) %>%
      anti_join(stop_words) %>%
      anti_join(cust_stop) %>% #here's where we remove tokens
      count(word, Single, Question, sort = TRUE) %>% 
      filter(Single == input$picker_tf) %>% 
      bind_tf_idf(word, Question, n) %>% 
      arrange(desc(tf_idf)) %>%
      filter(tf_idf > input$range[1]) %>%
      filter(tf_idf < input$range[2]) %>%
      mutate(word=factor(word, levels=rev(unique(word)))) %>%
      group_by(Question) %>%
      filter(Question == input$question) %>%
      ungroup %>%
      ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Question))+
      geom_col(show.legend=FALSE)+
      facet_wrap(~Question, ncol=2, scales="free")+
      labs(x=NULL, y="tf-idf")+
      coord_flip()
    
    
  })
  
  output$naive <- renderPrint({
    summary(NB_classifier)
    
  })
  
  
  output$bigram_tf <- renderPlot({
    
    bigrams_filtered %>%
      filter(Single == input$picker_bi) %>%
      unite(bigram, word1, word2, sep = " ") %>% 
      count(Question, bigram) %>%
      bind_tf_idf(bigram, Question, n) %>%
      arrange(desc(tf_idf)) %>% 
      filter(tf_idf > input$range_bi[1]) %>%
      filter(tf_idf < input$range_bi[2]) %>%
      group_by(Question) %>%
      filter(Question == input$question_bi) %>%
      slice_max(tf_idf, n = 4) %>%
      ungroup() %>%
      ggplot(aes(tf_idf, fct_reorder(bigram, tf_idf), fill = Question)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~Question, ncol = 2, scales = "free") +
      labs(x = "tf-idf", y = NULL)
    
  })
  
  
} # define a custom function to create the server


# load every data frame that is called in the server
# run this last line of code
shinyApp(ui = ui, server = server)







