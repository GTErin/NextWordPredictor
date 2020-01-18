library(quanteda)
library(stringr)

data_dir <- "C:/Users/Erin Kennedy/Documents/Data Analytics Certificate/10 Capstone/Week 1/final/en_US"

unigrams <- readRDS(sprintf("%s/%s.RDS", data_dir, "uniFreq"))
bigrams <- readRDS(sprintf("%s/%s.rds", data_dir, "biFreq"))
trigrams <- readRDS(sprintf("%s/%s.rds", data_dir, "triFreq"))
tetragrams <- readRDS(sprintf("%s/%s.rds", data_dir, "tetraFreq"))
pentagrams <- readRDS(sprintf("%s/%s.rds", data_dir, "pentaFreq"))

unigramProcessed <- readRDS("uniDFM.RDS")
bigramProcessed <- readRDS("biDFM.RDS")
trigramProcessed <- readRDS("triDFM.RDS")
tetragramProcessed <- readRDS("tetraDFM.RDS")
pentagramProcessed <- readRDS("pentaDFM.RDS")
ngrams_names <- c("unigrams", "bigrams", "trigrams", "tetragrams", "pentagrams")

# Remove singletons
bigramProcessed <- bigramProcessed[frequency != 1]
trigramProcessed <- trigramProcessed[frequency != 1]
tetragramProcessed <- tetragramProcessed[frequency != 1]
pentagramProcessed <- tetragramProcessed[frequency != 1]

PredictNextWord <- function(input){
  input <- tolower(input)
  input <- unlist(strsplit(as.character(input), ' '))
  n <- length(input)
  
  if(n == 0) return(" ")
  
  if(n >= 4 & nrow(pentagramProcessed[base == paste(input[n-3], input[n-2], input[n-1], input[n], sep = " "),]) > 0){
    new <- pentagramProcessed[.(paste(input[n-3], input[n-2], input[n-1], input[n], sep = " ")), head(.SD, 3), on = "base"]
    return(new[, predict])
  } else if(nrow(tetragramProcessed[base == paste(input[n-2], input[n-1], input[n], sep = " "),]) > 0) {
    new <- tetragramProcessed[.(paste(input[n-2], input[n-1], input[n], sep = " ")), head(.SD, 3), on = "base"]
    return(new[, predict])
  } else if(nrow(trigramProcessed[base == paste(input[n-1], input[n], sep = " "),]) > 0){
    new <- trigramProcessed[.(paste(input[n-1], input[n], sep = " ")), head(.SD, 3), on = "base"]
    return(new[, predict])
  } else if(nrow(bigramProcessed[base == paste(input[n], sep = ""),]) > 0){
    new <- bigramProcessed[.(paste(input[n], sep = "")), head(.SD, 3), on = "base"]
    return(new[, predict])
  } else if(n == 3 & nrow(tetragramProcessed[base == paste(input[n-2], input[n-1], input[n], sep = " "),]) > 0){
    new <- tetragramProcessed[.(paste(input[n-2], input[n-1], input[n], sep = " ")), head(.SD, 3), on = "base"]
    return(new[, predict])
  } else if(nrow(trigramProcessed[base == paste(input[n-1], input[n], sep = " "),]) > 0) {
    new <- trigramProcessed[.(paste(input[n-1], input[n], sep = " ")), head(.SD, 3), on = "base"]
    return(new[, predict])
  } else if(nrow(bigramProcessed[base == paste(input[n], sep = ""),]) > 0){
    new <- bigramProcessed[.(paste(input[n], sep = "")), head(.SD, 3), on = "base"]
    return(new[, predict])
  } else if(n == 2 & nrow(trigramProcessed[base == paste(input[n-1], input[n], sep = " "),]) > 0){
    new <- trigramProcessed[.(paste(input[n-1], input[n], sep = " ")), head(.SD, 3), on = "base"]
    return(new[, predict])
  } else if(nrow(bigramProcessed[base == paste(input[n], sep = ""),]) > 0) {
    new <- bigramProcessed[.(paste(input[n], sep = "")), head(.SD, 3), on = "base"]
    return(new[, predict])
  } else if(n == 1 & nrow(bigramProcessed[base == paste(input[n], sep = " "),]) > 0){
    new <- bigramProcessed[.(paste(input[n], sep = " ")), head(.SD, 3), on = "base"]
    return(new[, predict])
  } else{
    return("Unknown")
  }
}
