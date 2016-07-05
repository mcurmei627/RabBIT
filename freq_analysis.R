library(data.table) # fast read csv file
setwd("/Users/admin/src/RabBIT/");
load("big_wiki.RData");

function()
wiki_word_freq = fread("/Users/admin/src/BitFunnel/src/Index/UnitTest/Data/big_wiki.csv", header = T, sep = ',')

# add a column to the frequency table for the rank of the posting
wiki_word_freq$rank = length(wiki_word_freq$doc_count) + 1 - frank(wiki_word_freq$doc_count, ties.method = "random")
save.image(file = "big_wiki.RData");
plot(wiki_word_freq$rank, wiki_word_freq$doc_count, pch='.')

# remove the postings that only appear in one document
non_hapax_freq = wiki_word_freq[wiki_word_freq$doc_count > 1,];

# change the rank column to reflect the change
non_hapax_freq$rank = length(non_hapax_freq$doc_count) + 1 - frank(non_hapax_freq$doc_count, ties.method = "random")

plot(log(non_hapax_freq$rank), log(non_hapax_freq$doc_count), pch='.')

# divide by n-grams
one_freq = non_hapax_freq[non_hapax_freq$ngram_size==1];


