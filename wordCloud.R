### Esto es >95% código de Gastón Sanchez. El código original y mucho más lo
### pueden encontrar en https://sites.google.com/site/miningtwitter/ Un súper
### sitio si apenas estan empezando a minar y visualizar texto con R.

library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Primero hay que logearse a la API de twitter. Por motivos de seguridad omito
# esa parta.

# get the twits
mach_tweets <- searchTwitter("@FelipeCantuR", n = 1000, lang="es")

mach_text <- sapply(mach_tweets, function(x) x$getText())

# create a corpus
mach_corpus <- Corpus(VectorSource(mach_text),
                      readerControl = list(language = "spanish"))

# escape unicode. Remember to source the unicode_escape file first.
mach_corpus <- tm_map(mach_corpus,
                     content_transformer(function(x) unicode_escape(as.character(x))))


# remove URLs
mach_corpus <- tm_map(mach_corpus,
                      content_transformer(function(x) gsub("http.*",".",x,ignore.case=TRUE)))

# create document term matrix applying some transformations
tdm = TermDocumentMatrix(mach_corpus,
                         control = list(language = "spanish", removePunctuation = TRUE,
                                        stopwords = c(stopwords("spanish"),
                                                      "felipe","cantu",
                                                      "felipecantur"),
                                        removeNumbers = TRUE, tolower = TRUE))
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# save the image in png format
png("felipeCloud.png", width=12, height=8, units="in", res=300)
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
