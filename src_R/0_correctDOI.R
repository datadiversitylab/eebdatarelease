library(dplyr)
library(data.table)
library(here)
library(pbapply)

ds <- fread(here("data", "out.csv"), fill = TRUE, header = TRUE)
testds <- ds[1:100, ]

trim_to_ark <- function(row) {
  doi_index <- which(startsWith(as.character(row), "ark"))
  if (length(doi_index) > 0) {
    return(row[c(doi_index[1], doi_index[1]-1) ])
  }else{
    return(c(NA, NA)) #Skip those where parsing is incorrect
  }
}

trimmed <- t(apply(testds, 1, trim_to_ark))
trimmed_df <- as.data.frame(trimmed, stringsAsFactors = FALSE)
colnames(trimmed_df) <- c("identifier", "fullText")
trimmed_df <- na.omit(trimmed_df)
all(startsWith(trimmed_df$identifier , "ark")) #Check that all identifier starts with ark
fwrite(trimmed_df, here("data", "out.test.corrected.csv"))

##On the full dataset
trimmed <- t(pbapply(ds, 1, trim_to_ark))
trimmed_df <- as.data.frame(trimmed, stringsAsFactors = FALSE)
colnames(trimmed_df) <- c("identifier", "fullText")
all(startsWith(trimmed_df$identifier, "ark")) #Check that all DOI starts with 10.
fwrite(trimmed_df, here("data", "out.corrected.csv"))

