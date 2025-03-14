library(data.table)
library(here)
library(pbapply)

#Read in test dataset (see src_BASH for more details)
ds <- fread(here("data", "out.csv"), fill = TRUE, header = TRUE)

ExtractContext <- function(text, words) {
  cleaned <- gsub("\\n", "", text, fixed = TRUE)
  context <- lapply(words, function(z) {
    do.call(rbind.data.frame, lapply(strsplit(t(cleaned), "\\. "), function(x) {
      data.frame(x[unlist(lapply(z, function(y)
        grep(y, x)))])
    }))
  })
  names(context) <- words
  context2 <- rbindlist(context, idcol = "term")
  return(context2)
}


#List of target software
software <- c(
  'Winclada',
  'NONA' ,
  'TNT',
  'PAUP',
  'Raxml',
  'Iqtree',
  'iq-tree',
  'POPTREE2',
  'RAPDistance'
)

testds <- ds[1:50, ]

extractedWords <- lapply(1:nrow(testds), function(x){
  exW <- ExtractContext(text = testds$fullText[x], words = software)
  if( nrow(exW) > 0 ){
    colnames(exW) <- c("Term", "Sentence")
    cbind(exW, doi = testds$doi[x])
  }
})

extractedWords <- rbindlist(extractedWords)

write.csv(extractedWords, here("data/Context_test.csv"))


