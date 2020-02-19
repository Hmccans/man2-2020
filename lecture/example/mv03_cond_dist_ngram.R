# This program illustrates how to analyze text by modelling the relationship
# between words. The distribution for ngram is nothing but the joint distribution
# of two words (X and Y)

# This program is adapted from Text Mining with R (Silge and Robinson)
# available here at https://www.tidytextmining.com/ngrams.html

library(dplyr)
library(tidytext)
library(janeaustenr)
library(ggplot2)
library(tidyr)


# We will examine pairs of two consecutive words, called "bigrams"
# tokenize the "text" variable with bigram, and save this information
# in a variable called "bigram"

  austen_bigrams <- austen_books() %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  
  austen_bigrams

# Let's examine what are the most common bigrams

  austen_bigrams %>%
    count(bigram, sort = TRUE)

# Now let's treat each word as X and Y
# The first word as X and the second as Y

  
  bigrams_separated <- austen_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")

# Let's get rid of all the stop words
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

# new bigram counts:
  bigram_counts <- bigrams_filtered %>% 
    count(word1, word2, sort = TRUE)
  
  bigram_counts

# names (whether first and last or with a salutation) 
# are the most common pairs in Jane Austen books.

# Note that you can reverse the action of splitting words
# with unite()

  bigrams_united <- bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " ")
  
  bigrams_united
  
  # Let's calculate the frequency for each pair of words
  
  joint.dist<-bigrams_united %>% 
    count(bigram, sort = TRUE) %>% 
      mutate(
        freq = n/sum(n)
      )
  
  # We can visualize the joint distribution (just treating two words as one compound word!) as we did last semester
  
  ggplot(joint.dist[joint.dist$freq>=0.001,], mapping = aes(x=reorder(bigram,freq),y=freq)) +
    geom_bar(stat="identity") +
    coord_flip()
  
  # thousand pounds refer to money about a property that they were trying to sell.
  
  
# Let's calculate the probabiity of the first sentence
# View(austen_book())
  
# The family of Dashwood had long been settled in Sussex.  
  
# Let's examine which word is actually a stop word?
  
  sentence <- "The family of Dashwood had long been settled in Sussex"
  
  
  "the" %in% stop_words$word
  "family" %in% stop_words$word
  "of" %in% stop_words$word
  "dashwood" %in% stop_words$word
  "had" %in% stop_words$word
  "been" %in% stop_words$word
  "settled" %in% stop_words$word
  "long" %in% stop_words$word
  "sussex" %in% stop_words$word
  
  # a non-trivial sentence: family dashwood settled sussex
  
  # A non-repetitive solution
  
    # set every word to lower case first
    s.lower <- tolower(sentence)
    
    # split the sentence
    s.split <- strsplit(s.lower, " ")
  
    # unlist it so that it is a vector
    s.split <- unlist(s.split)
  
    # Write a function that repeats the evaluation for us
    # and apply it to every word in the vector
    select<-lapply(s.split, function(word){
        
        word %in% stop_words$word
      
        }
    )
  
    # unlist the list so that it is a logical vector
    select <- unlist(select)
  
    # select out the words
    s.split[select!="TRUE"]

    # The following code won't work    
    bigram_counts %>%
      filter(word1 == "family") %>%
      count(word2, sort = TRUE) %>%
      mutate( 
        freq = n/sum(n)
      )    
    
  
    # note uwing the code, the count variable
    # is renamed to nn
    bigram_counts %>%
      filter(word1 == "family") %>%
      count(word2, sort = TRUE)
        
    bigram_counts %>%
      filter(word1 == "family") %>%
      count(word2, sort = TRUE) %>%
      mutate( 
        freq = nn/sum(nn)
      )    %>%
      filter(word2 == "dashwood")
  
  
    # Correct way
    bigrams_filtered %>%
      filter(word1 == "family") %>%
      count(word2, sort = TRUE) %>%
      mutate( 
        freq = n/sum(n)
      )    %>%
      filter(word2 == "dashwood")  
  
# Suppose that you are interested in predicting a word coming after sir
# You just need to use Bayes classifier with the next word being Y
# Below is a toy novel writer starting with "sir"!
    
  
  bigrams_filtered %>%
    filter(word1 == "sir") %>%
    count(word2, sort = TRUE) %>%
    mutate( 
      freq = n/sum(n)
    ) 
  
  
  bigrams_filtered %>%
    filter(word1 == "thomas") %>%
    count(word2, sort = TRUE) %>%
    mutate( 
      freq = n/sum(n)
    )    
  
  
  bigrams_filtered %>%
    filter(word1 == "bertram") %>%
    count(word2, sort = TRUE) %>%
    mutate( 
      freq = n/sum(n)
    )    
  
  
  bigrams_filtered %>%
    filter(word1 == "agreed") %>%
    count(word2, sort = TRUE) %>%
    mutate( 
      freq = n/sum(n)
    )     
  
# You can certainly create tri-gram data as well
  
  austen_books() %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word,
           !word3 %in% stop_words$word) %>%
    count(word1, word2, word3, sort = TRUE)  
  