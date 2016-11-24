# Define the packages we want to load:
packs = c(
  "tm",                         # Text mining
  "tm.plugin.webmining",        # Web-source plugin for text mining
  "SnowballC",                  # Stemmer
  "RColorBrewer",               # Colors for visualisation
  "ggplot2",                    # Plotting
  "wordcloud",                  # Draw wordclouds
  "openNLP",                     # Split text into sentences.
  "gtools"
)


sapply(packs, require, character.only=TRUE)   # Load the packages.

ToSentences = function(text, language="en") {
  # Splits text into sentences using an Apache OpenNLP sentence detector.
  
  # Arguments:
  # "text" the text to be processed (character)
  # "lang" ISO-639 code of the language of the text (character)
  
  # Returns:
  # sentences of the text (character vector)
  if(length(text) ==0)      {return("")}
  if(nchar(text) == 0)   {return("")}   # Cover special case 0-character text.
  
  # Convert text to String object; allows for splitting by index.
  text = as.String(text)
  
  # Discover the sentence markers in the text (specify NLP as
  # source of annotate because there is also an annotate function in ggplot2)
  markers = NLP::annotate(
    text,
    Maxent_Sent_Token_Annotator(language=language)   # Annotator from OpenNLP
  )
  
  # Return sentences by splitting the text at the boundaries.
  text[markers]
}
CorpusToSentences = function(corpus) {
  # Split every document in the corpus into sentences and return a new corpus
  # with all the sentences as individual documents.
  
  # Extract the text from each document in the corpus.
  #text = lapply(corpus, "[[", "content")
  
  # Basically convert the text
  docs = lapply(corpus, ToSentences)
  
  docs = as.vector(unlist(docs))
  
  # Return a corpus with sentences as documents.
  Corpus(VectorSource(docs))
}
BreakDownCorpus = function(corpora) {
  # Create a new corpus which merges existing corpora after splitting them
  # into sentences.
  corpus = Reduce(c, lapply(corpora, CorpusToSentences))
  
  # Process the corpora contents.
  corpus = tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, stripWhitespace)
  
  corpus
}
PosCloud = function() {
  wordcloud(
    pos.terms,
    colSums(as.matrix(dtm[ , pos.terms])),
    min.freq=1,
    scale=c(4,0.7),
    color=brewer.pal(n=9, "Blues")[6:9]
  )
}
NegCloud = function() {
  wordcloud(
    neg.terms,
    colSums(as.matrix(dtm[ , neg.terms])),
    min.freq=1,
    scale=c(4,0.7),
    color=brewer.pal(n=9, "Reds")[6:9]
  )
}

height = 9
width = 9

################################################################
# ACQUIRING AND PROCESSING THE LEXICON.
################################################################
setwd("~/Desktop/dev/trump_speeches/")
lex = read.csv("inquirerbasic.csv", stringsAsFactors=FALSE)
lex$Entry = gsub("#1", "", lex$Entry)
lex = lex[!grepl("#", lex$Entry), ]
neg.lex = tolower(lex$Entry[lex$Negativ != ""])
pos.lex = tolower(lex$Entry[lex$Positiv != ""])

neg.lex = setdiff(neg.lex, c("need"))
pos.lex = setdiff(pos.lex, c("applause"))
################################################################

setwd("~/Desktop/dev/trump_speeches/trump_data/")

trump_txt_files = list.files(pattern = 'speech_')
trump_txt_files = mixedsort(txt_files)
corpora = lapply(trump_txt_files, readLines)
corpus = BreakDownCorpus(corpora)
dtm = DocumentTermMatrix(corpus)
dtm = dtm[ , colSums(as.matrix(dtm)) > 0]

terms = colnames(dtm)

# Find the positive and negative terms using the lexicons.
neg.terms = terms[terms %in% neg.lex]
pos.terms = terms[terms %in% pos.lex]

# Specify positive terms which may be questionable.
neg.terms.adj = setdiff(neg.terms, c("need"))

# Calculate the negative and positive sentence scores ("document scores").
neg.scores = rowSums(as.matrix(dtm[ , neg.terms]))
pos.scores = rowSums(as.matrix(dtm[ , pos.terms]))

document.scores = pos.scores - neg.scores

# Calulate the document signs ("sentence signs").
document.signs = sign(document.scores)

# Calculate the sentiment score
trump.sentiment.score = sum(document.signs == 1) / sum(document.signs !=0)

setwd("~/Desktop/dev/trump_speeches/charts/")

pdf("TrumpPositiveCloud.pdf",width=width,height=height)
PosCloud()
dev.off()

pdf("TrumpNegativeCloud.pdf",width=width,height=height)
NegCloud()
dev.off()


## Inter-Speech Analysis
setwd("~/Desktop/dev/trump_speeches/data")
speech_corpora <- length(txt_files)
speech_corpi <- length(txt_files)

for (i in 1:length(txt_files)) {
  speech_corpora[i] = readLines(con = txt_files[i])[2]
}

document.scores.each <- length(txt_files)
document.signs.each <- length(txt_files)
sentiment.score.each <- length(txt_files)

for (i in 1:length(txt_files)) {
  
  speech_corpi_temp = BreakDownCorpus(speech_corpora[i])
  temp_dtm = DocumentTermMatrix(speech_corpi_temp)
  temp_dtm = temp_dtm[ , colSums(as.matrix(temp_dtm)) > 0]
  
  terms = colnames(temp_dtm)
  
  neg.terms.each = terms[terms %in% neg.lex]
  pos.terms.each = terms[terms %in% pos.lex]
  
  neg.scores.each = rowSums(as.matrix(temp_dtm[ , neg.terms.each]))
  pos.scores.each = rowSums(as.matrix(temp_dtm[ , pos.terms.each]))
  
  # Specify positive terms which may be quiestionable.
  #pos.terms.adj = setdiff(pos.terms, c("equity", "share", "consensus"))
  
  document.scores.each = pos.scores.each - neg.scores.each
  document.signs.each = sign(document.scores.each)
  sentiment.score.each[i] = sum(document.signs.each == 1) / sum(document.signs.each !=0)
}

d <- data.frame(x = unlist(sentiment.score.each), 
                grp = rep(1:length(sentiment.score.each),
                          times = sapply(sentiment.score.each,length)))

pdf("SentimentPlot.pdf",width=9,height=6)
ggplot(d, aes(x = grp, y = x)) + geom_line()
dev.off()



####################################################################
# Hillary
####################################################################

## Inter-Speech Analysis
setwd("~/Desktop/dev/trump_speeches/data")
speech_corpora <- length(txt_files)
speech_corpi <- length(txt_files)

for (i in 1:length(txt_files)) {
  speech_corpora[i] = readLines(con = txt_files[i])[2]
}

document.scores.each <- length(txt_files)
document.signs.each <- length(txt_files)
sentiment.score.each <- length(txt_files)

for (i in 1:length(txt_files)) {
  
  speech_corpi_temp = BreakDownCorpus(speech_corpora[i])
  temp_dtm = DocumentTermMatrix(speech_corpi_temp)
  temp_dtm = temp_dtm[ , colSums(as.matrix(temp_dtm)) > 0]
  
  terms = colnames(temp_dtm)
  
  neg.terms.each = terms[terms %in% neg.lex]
  pos.terms.each = terms[terms %in% pos.lex]
  
  neg.scores.each = rowSums(as.matrix(temp_dtm[ , neg.terms.each]))
  pos.scores.each = rowSums(as.matrix(temp_dtm[ , pos.terms.each]))
  
  # Specify positive terms which may be quiestionable.
  #pos.terms.adj = setdiff(pos.terms, c("equity", "share", "consensus"))
  
  document.scores.each = pos.scores.each - neg.scores.each
  document.signs.each = sign(document.scores.each)
  sentiment.score.each[i] = sum(document.signs.each == 1) / sum(document.signs.each !=0)
}

d <- data.frame(x = unlist(sentiment.score.each), 
                grp = rep(1:length(sentiment.score.each),
                          times = sapply(sentiment.score.each,length)))

pdf("SentimentPlot.pdf",width=9,height=6)
ggplot(d, aes(x = grp, y = x)) + geom_line()
dev.off()














