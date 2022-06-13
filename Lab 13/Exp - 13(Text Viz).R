#Step 1: Create a text file
#In the following examples, I'll process some random text. You can use any text you want :
#Copy and paste the text in a plain text file (e.g : ml.txt)
#Save the file



#Step 2 : Install and load the required packages
# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")



#Step 3 : Text mining
#load the text
#The text is loaded using Corpus() function from text mining (tm) package. Corpus is a list of a document (in our case, we only have one document).
text <- readLines(file.choose()) #import the file saved locally in your computer
# Load the data as a corpus (a collection of written or spoken language, which can be used for a variety of reasons, from helping to compile dictionaries, to providing insight into how language is actually used.)
docs <- Corpus(VectorSource(text)) #VectorSource() function creates a corpus of character vectors
inspect(docs) #Inspect the content of the document



#Text transformation
#Transformation is performed using tm_map() function to replace, for example, special characters from the text.
#Replacing "/", "@" and "|" with space:
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "%")
docs <- tm_map(docs, toSpace, "\\|")



#Cleaning the text
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)



#Step 4 : Build a term-document matrix
#Document matrix is a table containing the frequency of the words. Column names are words and row names are documents. The function TermDocumentMatrix() from text mining package can be used as follow :
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)



#Step 5 : Generate the Word cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#Arguments of the word cloud generator function :
#words : the words to be plotted
#freq : their frequencies
#min.freq : words with frequency below min.freq will not be plotted
#max.words : maximum number of words to be plotted
#random.order : plot words in random order. If false, they will be plotted in decreasing frequency
#rot.per : proportion words with 90 degree rotation (vertical text)
#colors : color words from least to most frequent. Use, for example, colors ="black" for single color.



#Explore frequent terms and their associations
#In the example below we want to find words that occur at least two times :
findFreqTerms(dtm, lowfreq = 5)
findAssocs(dtm, terms = c("chocolate", "chips", "hello"), corlimit = 0.1)



#The frequency table of words
head(d, 10)



#Plot word frequencies (The frequency of the first 10 frequent words are plotted :)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

