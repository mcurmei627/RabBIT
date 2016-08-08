# RabBIT

This repository contains the R script mainly for running simulations over the corpus using some of the functionalities provided by BitFunnel Ingestor: https://github.com/BitFunnel

## Creating manifest files

The chunk files can be found here, there are 4 folders {AA, AB, AC, AD} containing each 100 chunck filea, except the last one which has 96. 
https://www.dropbox.com/s/ysava18au2q7t67/wiki-chunks.tar.gz?dl=0
Each chunk file contains around 500 documents.

A manifest file is a text file that contains on each line fully specified system paths to each chunk file

## Obtaining a test corpus

Clone experiment branch https://github.com/BitFunnel/BitFunnel/tree/experiment and run:
```
make &&./StatisticsBuilder <manifest filepath>
```
The result are two $ separated csv files (they are not comma separated because some terms contain commas in the string of a posting, confusing the reader).

