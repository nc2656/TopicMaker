#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Automatic Study Topic Maker"),

      wellPanel(p("Please upload a text file."),
                p("When finished, please download or copy your result.")),
      fluidRow(
        # Upload business description
        column(fileInput("uploaded_business_description",
                         label = h5("Upload your data")),
               width =  6),
      ),
      
    fluidRow(
      column(
        downloadButton("download_key_sentences",
                       "Download result of key sentences"),
        width =  6),
    ),
    
    
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {

  # Import the library 
  library(dplyr)
  library(readr)
  library(NLP)
  library(tm)
  library(Matrix)
  library(SnowballC)
  
  
  ### Defined Functions ###
  # Pre-processing data with defined function
  corpus_preprocessing = function(corpus){
    # Replace special symbols with space 
    toSpace <- content_transformer(function (x , pattern) gsub(pattern, " ", x))
    # Normalization
    corpus <- tm_map(corpus, toSpace, "/")
    corpus <- tm_map(corpus,toSpace,"@")
    corpus <- tm_map(corpus,toSpace,"\\｜")
    corpus <- tm_map(corpus,toSpace,"#")
    corpus <- tm_map(corpus, toSpace, "®")
    # Casing (upper case & lower case), convert the text to lower case
    corpus <- tm_map(corpus, content_transformer(tolower))
    # Remove punctuation 
    corpus <- tm_map(corpus, removePunctuation)
    # Remove extra white space 
    corpus <- tm_map(corpus, stripWhitespace)
    # Remove Stop words
    corpus <- tm_map(corpus,removeWords,stopwords("english"))
    corpus <- tm_map(corpus,removeWords,c("the","and","The","And","A","An","a","an","e","d"))
    # Stemming (e.g. -ing vs original)
    corpus <- tm_map(corpus,stemDocument, language ="english")
    return(corpus)
  }
  
  # Calculate cosine similarity with TF-IDF
  cos_sim = function(matrix){
    numerator = matrix %*% t(matrix)
    A = sqrt(apply(matrix^2, 1, sum))
    denumerator = A %*% t(A)
    return(numerator / denumerator)
  }
  
  file1<- reactive({
    inFile1 <-input$uploaded_business_description
    if(is.null(inFile1))
      #inFile1 <- input$pasted_text
      return(NULL)
    
  

  readfile <- read_file(inFile1$datapath)

  c1 <- subset(readfile, Chapter == "1")
  c2 <- subset(readfile, Chapter == "2")
  c3 <- subset(readfile, Chapter == "3")
  c4 <- subset(readfile, Chapter == "4")
  c5 <- subset(readfile, Chapter == "5")
  c6 <- subset(readfile, Chapter == "6")
  c7 <- subset(readfile, Chapter == "7")
  c8 <- subset(readfile, Chapter == "8")
  c9 <- subset(readfile, Chapter == "9")
  c10 <- subset(readfile, Chapter == "10")
  c11 <- subset(readfile, Chapter == "11")
  c12 <- subset(readfile, Chapter == "12")
  c13 <- subset(readfile, Chapter == "13")
  c14 <- subset(readfile, Chapter == "14")
  c15 <- subset(readfile, Chapter == "15")
  c16 <- subset(readfile, Chapter == "16")
  c17 <- subset(readfile, Chapter == "17")
  c18 <- subset(readfile, Chapter == "18")
  c19 <- subset(readfile, Chapter == "19")
  c20 <- subset(readfile, Chapter == "20")
  c21 <- subset(readfile, Chapter == "21")
  
  c1<- c1[,-2]
  c2<- c2[,-2]
  c3<- c3[,-2]
  c4<- c4[,-2]
  c5<- c5[,-2]
  c6<- c6[,-2]
  c7<- c7[,-2]
  c8<- c8[,-2]
  c9<- c9[,-2]
  c10<- c10[,-2]
  c11<- c11[,-2]
  c12<- c12[,-2]
  c13<- c13[,-2]
  c14<- c14[,-2]
  c15<- c15[,-2]
  c16<- c16[,-2]
  c17<- c17[,-2]
  c18<- c18[,-2]
  c19<- c19[,-2]
  c20<- c20[,-2]
  c21<- c21[,-2]
  
  # Change column name of both combined documents
  names(c1)[1] = "doc_id"
  names(c1)[2] = "text"
  names(c2)[1] = "doc_id"
  names(c2)[2] = "text"
  names(c3)[1] = "doc_id"
  names(c3)[2] = "text"
  names(c4)[1] = "doc_id"
  names(c4)[2] = "text"
  names(c5)[1] = "doc_id"
  names(c5)[2] = "text"
  names(c6)[1] = "doc_id"
  names(c6)[2] = "text"
  names(c7)[1] = "doc_id"
  names(c7)[2] = "text"
  names(c8)[1] = "doc_id"
  names(c8)[2] = "text"
  names(c9)[1] = "doc_id"
  names(c9)[2] = "text"
  names(c10)[1] = "doc_id"
  names(c10)[2] = "text"
  names(c11)[1] = "doc_id"
  names(c11)[2] = "text"
  names(c12)[1] = "doc_id"
  names(c12)[2] = "text"
  names(c13)[1] = "doc_id"
  names(c13)[2] = "text"
  names(c14)[1] = "doc_id"
  names(c14)[2] = "text"
  names(c15)[1] = "doc_id"
  names(c15)[2] = "text"
  names(c16)[1] = "doc_id"
  names(c16)[2] = "text"
  names(c17)[1] = "doc_id"
  names(c17)[2] = "text"
  names(c18)[1] = "doc_id"
  names(c18)[2] = "text"
  names(c19)[1] = "doc_id"
  names(c19)[2] = "text"
  names(c20)[1] = "doc_id"
  names(c20)[2] = "text"
  names(c21)[1] = "doc_id"
  names(c21)[2] = "text"
  
  # Transfer the data into corpus 
  doc_corpus_1 = VCorpus(DataframeSource(c1))
  doc_corpus_2 = VCorpus(DataframeSource(c2))
  doc_corpus_3 = VCorpus(DataframeSource(c3))
  doc_corpus_4 = VCorpus(DataframeSource(c4))
  doc_corpus_5 = VCorpus(DataframeSource(c5))
  doc_corpus_6 = VCorpus(DataframeSource(c6))
  doc_corpus_7 = VCorpus(DataframeSource(c7))
  doc_corpus_8 = VCorpus(DataframeSource(c8))
  doc_corpus_9 = VCorpus(DataframeSource(c9))
  doc_corpus_10 = VCorpus(DataframeSource(c10))
  doc_corpus_11 = VCorpus(DataframeSource(c11))
  doc_corpus_12 = VCorpus(DataframeSource(c12))
  doc_corpus_13 = VCorpus(DataframeSource(c13))
  doc_corpus_14 = VCorpus(DataframeSource(c14))
  doc_corpus_15 = VCorpus(DataframeSource(c15))
  doc_corpus_16 = VCorpus(DataframeSource(c16))
  doc_corpus_17 = VCorpus(DataframeSource(c17))
  doc_corpus_18 = VCorpus(DataframeSource(c18))
  doc_corpus_19 = VCorpus(DataframeSource(c19))
  doc_corpus_20 = VCorpus(DataframeSource(c20))
  doc_corpus_21 = VCorpus(DataframeSource(c21))
  
  # Apply data pre-processing to each corpus
  corpus_cleaned1 <- corpus_preprocessing(doc_corpus_1)
  corpus_cleaned2 <- corpus_preprocessing(doc_corpus_2)
  corpus_cleaned3 <- corpus_preprocessing(doc_corpus_3)
  corpus_cleaned4 <- corpus_preprocessing(doc_corpus_4)
  corpus_cleaned5 <- corpus_preprocessing(doc_corpus_5)
  corpus_cleaned6 <- corpus_preprocessing(doc_corpus_6)
  corpus_cleaned7 <- corpus_preprocessing(doc_corpus_7)
  corpus_cleaned8 <- corpus_preprocessing(doc_corpus_8)
  corpus_cleaned9 <- corpus_preprocessing(doc_corpus_9)
  corpus_cleaned10 <- corpus_preprocessing(doc_corpus_10)
  corpus_cleaned11 <- corpus_preprocessing(doc_corpus_11)
  corpus_cleaned12 <- corpus_preprocessing(doc_corpus_12)
  corpus_cleaned13 <- corpus_preprocessing(doc_corpus_13)
  corpus_cleaned14 <- corpus_preprocessing(doc_corpus_14)
  corpus_cleaned15 <- corpus_preprocessing(doc_corpus_15)
  corpus_cleaned16 <- corpus_preprocessing(doc_corpus_16)
  corpus_cleaned17 <- corpus_preprocessing(doc_corpus_17)
  corpus_cleaned18 <- corpus_preprocessing(doc_corpus_18)
  corpus_cleaned19 <- corpus_preprocessing(doc_corpus_19)
  corpus_cleaned20 <- corpus_preprocessing(doc_corpus_20)
  corpus_cleaned21 <- corpus_preprocessing(doc_corpus_21)
  
  #Create a Document Term matrix, which containing the frequency of the words
  #each row represents a document/text message 
  #each column represents a distinct text/name
  #each cell is a count of the token for a document/text message
  
  doc_dtm_1 <- DocumentTermMatrix(corpus_cleaned_1)
  dtm_m_1 <- as.matrix(doc_dtm_1)
  doc_dtm_2 <- DocumentTermMatrix(corpus_cleaned_2)
  dtm_m_2 <- as.matrix(doc_dtm_2)
  doc_dtm_3 <- DocumentTermMatrix(corpus_cleaned_3)
  dtm_m_3 <- as.matrix(doc_dtm_3)
  doc_dtm_4 <- DocumentTermMatrix(corpus_cleaned_4)
  dtm_m_4 <- as.matrix(doc_dtm_4)
  doc_dtm_5 <- DocumentTermMatrix(corpus_cleaned_5)
  dtm_m_5 <- as.matrix(doc_dtm_5)
  doc_dtm_6 <- DocumentTermMatrix(corpus_cleaned_6)
  dtm_m_6 <- as.matrix(doc_dtm_6)
  doc_dtm_7 <- DocumentTermMatrix(corpus_cleaned_7)
  dtm_m_7 <- as.matrix(doc_dtm_7)
  doc_dtm_8 <- DocumentTermMatrix(corpus_cleaned_8)
  dtm_m_8 <- as.matrix(doc_dtm_8)
  doc_dtm_9 <- DocumentTermMatrix(corpus_cleaned_9)
  dtm_m_9 <- as.matrix(doc_dtm_9)
  doc_dtm_10 <- DocumentTermMatrix(corpus_cleaned_10)
  dtm_m_10 <- as.matrix(doc_dtm_10)
  doc_dtm_11 <- DocumentTermMatrix(corpus_cleaned_11)
  dtm_m_11 <- as.matrix(doc_dtm_11)
  doc_dtm_12 <- DocumentTermMatrix(corpus_cleaned_12)
  dtm_m_12 <- as.matrix(doc_dtm_12)
  doc_dtm_13 <- DocumentTermMatrix(corpus_cleaned_13)
  dtm_m_13 <- as.matrix(doc_dtm_13)
  doc_dtm_14 <- DocumentTermMatrix(corpus_cleaned_14)
  dtm_m_14 <- as.matrix(doc_dtm_14)
  doc_dtm_15 <- DocumentTermMatrix(corpus_cleaned_15)
  dtm_m_15 <- as.matrix(doc_dtm_15)
  doc_dtm_16 <- DocumentTermMatrix(corpus_cleaned_16)
  dtm_m_16 <- as.matrix(doc_dtm_16)
  doc_dtm_17 <- DocumentTermMatrix(corpus_cleaned_17)
  dtm_m_17 <- as.matrix(doc_dtm_17)
  doc_dtm_18 <- DocumentTermMatrix(corpus_cleaned_18)
  dtm_m_18 <- as.matrix(doc_dtm_18)
  doc_dtm_19 <- DocumentTermMatrix(corpus_cleaned_19)
  dtm_m_19 <- as.matrix(doc_dtm_19)
  doc_dtm_20 <- DocumentTermMatrix(corpus_cleaned_20)
  dtm_m_20 <- as.matrix(doc_dtm_20)
  doc_dtm_21 <- DocumentTermMatrix(corpus_cleaned_21)
  dtm_m_21 <- as.matrix(doc_dtm_21)
  
  # Apply TF-IDF Weighting 
  tfidf_1 <- DocumentTermMatrix(doc_corpus_1,control = list(weighting = weightTfIdf))
  tfidf_m_1 = as.matrix(tfidf_1)
  tfidf_2 <- DocumentTermMatrix(doc_corpus_2,control = list(weighting = weightTfIdf))
  tfidf_m_2 = as.matrix(tfidf_2)
  tfidf_3 <- DocumentTermMatrix(doc_corpus_3,control = list(weighting = weightTfIdf))
  tfidf_m_3 = as.matrix(tfidf_3)
  tfidf_4 <- DocumentTermMatrix(doc_corpus_4,control = list(weighting = weightTfIdf))
  tfidf_m_4 = as.matrix(tfidf_4)
  tfidf_5 <- DocumentTermMatrix(doc_corpus_5,control = list(weighting = weightTfIdf))
  tfidf_m_5 = as.matrix(tfidf_5)
  tfidf_6 <- DocumentTermMatrix(doc_corpus_6,control = list(weighting = weightTfIdf))
  tfidf_m_6 = as.matrix(tfidf_6)
  tfidf_7 <- DocumentTermMatrix(doc_corpus_7,control = list(weighting = weightTfIdf))
  tfidf_m_7 = as.matrix(tfidf_7)
  tfidf_8 <- DocumentTermMatrix(doc_corpus_8,control = list(weighting = weightTfIdf))
  tfidf_m_8 = as.matrix(tfidf_8)
  tfidf_9 <- DocumentTermMatrix(doc_corpus_9,control = list(weighting = weightTfIdf))
  tfidf_m_9 = as.matrix(tfidf_9)
  tfidf_10 <- DocumentTermMatrix(doc_corpus_10,control = list(weighting = weightTfIdf))
  tfidf_m_10 = as.matrix(tfidf_10)
  tfidf_11 <- DocumentTermMatrix(doc_corpus_11,control = list(weighting = weightTfIdf))
  tfidf_m_11 = as.matrix(tfidf_11)
  tfidf_12 <- DocumentTermMatrix(doc_corpus_12,control = list(weighting = weightTfIdf))
  tfidf_m_12 = as.matrix(tfidf_12)
  tfidf_13 <- DocumentTermMatrix(doc_corpus_13,control = list(weighting = weightTfIdf))
  tfidf_m_13 = as.matrix(tfidf_13)
  tfidf_14 <- DocumentTermMatrix(doc_corpus_14,control = list(weighting = weightTfIdf))
  tfidf_m_14 = as.matrix(tfidf_14)
  tfidf_15 <- DocumentTermMatrix(doc_corpus_15,control = list(weighting = weightTfIdf))
  tfidf_m_15 = as.matrix(tfidf_15)
  tfidf_16 <- DocumentTermMatrix(doc_corpus_16,control = list(weighting = weightTfIdf))
  tfidf_m_16 = as.matrix(tfidf_16)
  tfidf_17 <- DocumentTermMatrix(doc_corpus_17,control = list(weighting = weightTfIdf))
  tfidf_m_17 = as.matrix(tfidf_17)
  tfidf_18 <- DocumentTermMatrix(doc_corpus_18,control = list(weighting = weightTfIdf))
  tfidf_m_18 = as.matrix(tfidf_18)
  tfidf_19 <- DocumentTermMatrix(doc_corpus_19,control = list(weighting = weightTfIdf))
  tfidf_m_19 = as.matrix(tfidf_19)
  tfidf_20 <- DocumentTermMatrix(doc_corpus_20,control = list(weighting = weightTfIdf))
  tfidf_m_20 = as.matrix(tfidf_20)
  tfidf_21 <- DocumentTermMatrix(doc_corpus_21,control = list(weighting = weightTfIdf))
  tfidf_m_21 = as.matrix(tfidf_21)
  
  # Calculate Cosine Similarity for each digit 
  tfidf_cos_sim_1 = cos_sim(tfidf_m_1)
  tfidf_cos_sim_2 = cos_sim(tfidf_m_2)
  tfidf_cos_sim_3 = cos_sim(tfidf_m_3)
  tfidf_cos_sim_4 = cos_sim(tfidf_m_4)
  tfidf_cos_sim_5 = cos_sim(tfidf_m_5)
  tfidf_cos_sim_6 = cos_sim(tfidf_m_6)
  tfidf_cos_sim_7 = cos_sim(tfidf_m_7)
  tfidf_cos_sim_8 = cos_sim(tfidf_m_8)
  tfidf_cos_sim_9 = cos_sim(tfidf_m_9)
  tfidf_cos_sim_10 = cos_sim(tfidf_m_10)
  tfidf_cos_sim_11 = cos_sim(tfidf_m_11)
  tfidf_cos_sim_12 = cos_sim(tfidf_m_12)
  tfidf_cos_sim_13 = cos_sim(tfidf_m_13)
  tfidf_cos_sim_14 = cos_sim(tfidf_m_14)
  tfidf_cos_sim_15 = cos_sim(tfidf_m_15)
  tfidf_cos_sim_16 = cos_sim(tfidf_m_16)
  tfidf_cos_sim_17 = cos_sim(tfidf_m_17)
  tfidf_cos_sim_18 = cos_sim(tfidf_m_18)
  tfidf_cos_sim_19 = cos_sim(tfidf_m_19)
  tfidf_cos_sim_20 = cos_sim(tfidf_m_20)
  tfidf_cos_sim_21 = cos_sim(tfidf_m_21)
  
  # Create columns for similarity_score and corresponding code 
  c1["similarity_score"] = tfidf_cos_sim_1[1:ncol(tfidf_cos_sim_1)]
  c2["similarity_score"] = tfidf_cos_sim_2[1:ncol(tfidf_cos_sim_2)]
  c3["similarity_score"] = tfidf_cos_sim_3[1:ncol(tfidf_cos_sim_3)]
  c4["similarity_score"] = tfidf_cos_sim_4[1:ncol(tfidf_cos_sim_4)]
  c5["similarity_score"] = tfidf_cos_sim_5[1:ncol(tfidf_cos_sim_5)]
  c6["similarity_score"] = tfidf_cos_sim_6[1:ncol(tfidf_cos_sim_6)]
  c7["similarity_score"] = tfidf_cos_sim_7[1:ncol(tfidf_cos_sim_7)]
  c8["similarity_score"] = tfidf_cos_sim_8[1:ncol(tfidf_cos_sim_8)]
  c9["similarity_score"] = tfidf_cos_sim_9[1:ncol(tfidf_cos_sim_9)]
  c10["similarity_score"] = tfidf_cos_sim_10[1:ncol(tfidf_cos_sim_10)]
  c11["similarity_score"] = tfidf_cos_sim_11[1:ncol(tfidf_cos_sim_11)]
  c12["similarity_score"] = tfidf_cos_sim_12[1:ncol(tfidf_cos_sim_12)]
  c13["similarity_score"] = tfidf_cos_sim_13[1:ncol(tfidf_cos_sim_13)]
  c14["similarity_score"] = tfidf_cos_sim_14[1:ncol(tfidf_cos_sim_14)]
  c15["similarity_score"] = tfidf_cos_sim_15[1:ncol(tfidf_cos_sim_15)]
  c16["similarity_score"] = tfidf_cos_sim_16[1:ncol(tfidf_cos_sim_16)]
  c17["similarity_score"] = tfidf_cos_sim_17[1:ncol(tfidf_cos_sim_17)]
  c18["similarity_score"] = tfidf_cos_sim_18[1:ncol(tfidf_cos_sim_18)]
  c19["similarity_score"] = tfidf_cos_sim_19[1:ncol(tfidf_cos_sim_19)]
  c20["similarity_score"] = tfidf_cos_sim_20[1:ncol(tfidf_cos_sim_20)]
  c21["similarity_score"] = tfidf_cos_sim_21[1:ncol(tfidf_cos_sim_21)]
  
  # Sort the data frame by similarity score
  similarity_1 = c1[order(-c1$similarity_score),]
  similarity_2 = c2[order(-c2$similarity_score),]
  similarity_3 = c3[order(-c3$similarity_score),]
  similarity_4 = c4[order(-c4$similarity_score),]
  similarity_5 = c5[order(-c5$similarity_score),]
  similarity_6 = c6[order(-c6$similarity_score),]
  similarity_7 = c7[order(-c7$similarity_score),]
  similarity_8 = c8[order(-c8$similarity_score),]
  similarity_9 = c9[order(-c9$similarity_score),]
  similarity_10 = c10[order(-c10$similarity_score),]
  similarity_11 = c11[order(-c11$similarity_score),]
  similarity_12 = c12[order(-c12$similarity_score),]
  similarity_13 = c13[order(-c13$similarity_score),]
  similarity_14 = c14[order(-c14$similarity_score),]
  similarity_15 = c15[order(-c15$similarity_score),]
  similarity_16 = c16[order(-c16$similarity_score),]
  similarity_17 = c17[order(-c17$similarity_score),]
  similarity_18 = c18[order(-c18$similarity_score),]
  similarity_19 = c19[order(-c19$similarity_score),]
  similarity_20 = c20[order(-c20$similarity_score),]
  similarity_21 = c21[order(-c21$similarity_score),]
  
  similarity_1 <- similarity_1[-1,]
  similarity_2 <- similarity_2[-1,]
  similarity_3 <- similarity_3[-1,]
  similarity_4 <- similarity_4[-1,]
  similarity_5 <- similarity_5[-1,]
  similarity_6 <- similarity_6[-1,]
  similarity_7 <- similarity_7[-1,]
  similarity_8 <- similarity_8[-1,]
  similarity_9 <- similarity_9[-1,]
  similarity_10 <- similarity_10[-1,]
  similarity_11 <- similarity_11[-1,]
  similarity_12 <- similarity_12[-1,]
  similarity_13 <- similarity_13[-1,]
  similarity_14 <- similarity_14[-1,]
  similarity_15 <- similarity_15[-1,]
  similarity_16 <- similarity_16[-1,]
  similarity_17 <- similarity_17[-1,]
  similarity_18 <- similarity_18[-1,]
  similarity_19 <- similarity_19[-1,]
  similarity_20 <- similarity_20[-1,]
  similarity_21 <- similarity_21[-1,]
  # Display the top ten 
  top_10_c1 <- similarity_1 %>% top_n(10)
  top_10_c2 <- similarity_2 %>% top_n(10)
  top_10_c3 <- similarity_3 %>% top_n(10)
  top_10_c4 <- similarity_4 %>% top_n(10)
  top_10_c5 <- similarity_5 %>% top_n(10)
  top_10_c6 <- similarity_6 %>% top_n(10)
  top_10_c7 <- similarity_7 %>% top_n(10)
  top_10_c8 <- similarity_8 %>% top_n(10)
  top_10_c9 <- similarity_9 %>% top_n(10)
  top_10_c10 <- similarity_10 %>% top_n(10)
  top_10_c11 <- similarity_11 %>% top_n(10)
  top_10_c12 <- similarity_12 %>% top_n(10)
  top_10_c13 <- similarity_13 %>% top_n(10)
  top_10_c14 <- similarity_14 %>% top_n(10)
  top_10_c15 <- similarity_15 %>% top_n(10)
  top_10_c16 <- similarity_16 %>% top_n(10)
  top_10_c17 <- similarity_17 %>% top_n(10)
  top_10_c18 <- similarity_18 %>% top_n(10)
  top_10_c19 <- similarity_19 %>% top_n(10)
  top_10_c20 <- similarity_20 %>% top_n(10)
  top_10_c21 <- similarity_21 %>% top_n(10)
  
  output_file <- rbind(top_10_c1,top_10_c2,top_10_c3,top_10_c4,top_10_c5,
                       top_10_c6,top_10_c7,top_10_c8,top_10_c9,top_10_c10,
                       top_10_c11,top_10_c12,top_10_c13,top_10_c14,top_10_c15,
                       top_10_c16,top_10_c17,top_10_c18,top_10_c19,top_10_c20,
                       top_10_c21)
  
  names(output_file)[1] = "Chapter"
  names(output_file)[2] = "Sentence"
  
  write.csv(output_file,"output.csv")
  
  #Download the output still the full code 

  })  
  
  output$download_key_sentences <- downloadHandler(
    filename = "key-sentences.csv",
    content = function(file){
      file.copy("data/output.csv",file)
    })
  }


# Run the application 
shinyApp(ui = ui, server = server)
