library(data.table)
library(here)

#Read in test dataset (see src_BASH for more details)
ds <- fread(here("data", "out.csv"), fill=TRUE, header = TRUE)

#List of target software
software <- c('Winclada',
              'NONA' ,
              'TNT',
              'PAUP', 
              'Raxml',
              'Iqtree', 
              'iq-tree',
              'POPTREE2', 
              'RAPDistance')

#Extract a subset of the rows
testds <- ds[1:5,]

#Generate a presence absence matrix based on matches
result_df <- sapply(software, function(x) {
  as.integer(grepl(x, testds$fullText, ignore.case = TRUE))
})

#Merge the DOI
result_df <- cbind.data.frame(DOI = testds$doi, result_df)

#Export the dataset
write.csv(here("data", "PAM.csv"))





