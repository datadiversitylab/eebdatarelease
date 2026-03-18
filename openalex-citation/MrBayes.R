# # # # # # # # # 
# Example of how to fetch all the papers that cite MrBayes
# Start with MrBayes version (https://doi.org/10.1093/sysbio/sys029)
# install.packages("openalexR")
library(openalexR)
library(tidyverse)
library(dplyr)
library(readr)
library(jsonlite)
packageVersion("openalexR")
file.edit("~/.Renviron") # add openalexR.mailto = sebratt@arizona.edu


# # # # Fetch all DOIs that cite Mr Bayes 3.2. As of 3/18/2026 
# 30,088 citations according to Google Scholar
# # # # Select only 'language' and 'doi' of the citing works?
# 
# Note: I used ChatGPT to optimize the code, since 
# MrBayes is > 10k citations (pagination)


# -----------------------------------------------------------
# 1. Define the DOI of the target paper
# This will be a list of DOIs for a bunch of EEB software, but let's start here. 
# -----------------------------------------------------------

doi <- "10.1093/sysbio/sys029" # MrBayes DOI
#doi <- "10.1155/2021/6671300"

url <- paste0(
  "https://api.openalex.org/works/https://doi.org/",
  doi)

# -----------------------------------------------------------
# 2. Retrieve the OpenAlex record for the target paper
# -----------------------------------------------------------
work <- fromJSON(url)
work$language

# Extract OpenAlex ID of the target work
target_openalex_id <- work$id

cat("Target OpenAlex ID:", target_openalex_id, "\n")

# -----------------------------------------------------------
# 3. Retrieve ALL works that cite this paper
#    (uses automatic pagination)
# -----------------------------------------------------------
# Define the openalex work id 
target_openalex_id = 'W2148698435'


citing_works <- oa_fetch(
  entity = "works",
  publication_year=2012,
  cites = target_openalex_id,
  options=list(select = c("doi", "language","title","abstract_inverted_index")),
  per_page = 200,      # max allowed
  #cursor = "*"         # enables cursor pagination
  paging="cursor"
  )

# next steps. 
# 1. For each DOI:
# 1.1  Write a loop to get cites for that DOI for "publication_year".
# 1.3 Store the citing 

# Tutorial on paging from Jeff Oliver: ipynb 

# ---------------------------------------------------
# 
# Next: Detect the language of the title and abstract
# because the language of some papers are inaccurately
# classified. 
# 
# ---------------------------------------------------


