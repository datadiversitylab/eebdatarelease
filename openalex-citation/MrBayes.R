# # # # 
# Try fetching MrBayes version (https://doi.org/10.1093/sysbio/sys029)
#install.packages("openalexR")
library(openalexR)
library(tidyverse)
library(dplyr)
library(readr)
library(jsonlite)
packageVersion("openalexR")
file.edit("~/.Renviron") # add openalexR.mailto = your@email.com


# # # # Fetch all DOIs that cite Mr Bayes 3.2. As of 3/5/2026 
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
# MrBayes
year = 1992
citing_works <- oa_fetch(
  entity = "works",
  publication_year=year,
  cites = target_openalex_id,
  options=list(select = c("doi", "language","title","abstract_inverted_index","authorships")),
  per_page = 200,      # max allowed
  paging="cursor" # enables cursor pagination
)



# next steps. 
# 1. For each DOI:
# 1.1  Write a loop to get cites for that DOI for "publication_year".
# 1.3 Store the citing work in a cvs somehwere.
getwd()
dois <- read.csv2("C:/Users/sebratt/Downloads/eeb_dois188.csv", header=TRUE, sep = ",")

for (i in 9:nrow(dois))
{
  #i = 1
  doi <- dois[i,]
  
  url <- paste0(
    "https://api.openalex.org/works/https://doi.org/",
    doi)
  
  # -----------------------------------------------------------
  # 2. Retrieve the OpenAlex record for the target paper
  # -----------------------------------------------------------
  work <- fromJSON(url)
  # Extract OpenAlex ID of the target work
  target_openalex_id <- work$id
  # Extract start year of work 
  startyear <- work$publication_year
# Jeff Halp plz:  
 # when no records (for a given year) are found can't rbind.
# when abstract not returned have to add column for abstract with NAs.
year = 2005
  for (year in startyear:2026)
  {
    print(year)
    citing_works <- oa_fetch(
      entity = "works",
      publication_year=year,
      cites = target_openalex_id,
      options=list(select = c("doi", "language","title","abstract_inverted_index","authorships")),
      per_page = 200,      # max allowed
      paging="cursor" # enables cursor pagination
    
      )
    
        citing_works$year <- year
        citing_works$original_doi <- doi 
        citing_works$original_workid <- target_openalex_id
   
          if (!is.null(ncol(citing_works)) && ncol(citing_works) == 8) 
          {
            print("citing works has exactly 8 columns")
          } else {
            citing_works$abstract <- NA 
            # Additional actions can be placed here
          }
     
  #final_df <- citing_works
        if (!is.null(ncol(citing_works)) && ncol(citing_works) == 8) 
        {
          final_df <- rbind(final_df, citing_works)
        } else {
          print("no records for this year or columns not rbind length correct")
        } 

        
}

  
  
  
  # what happens if there not cited in a year? does it throw an error? 
  # write results to csv 

} 

f_df <- final_df[,]

# Tutorial on paging from Jeff Oliver: ipynb 
# https://github.com/ourresearch/openalex-api-tutorials/blob/main/notebooks/getting-started/paging.ipynb

# ---------------------------------------------------
# 
# Next: 
# - Language: Detect the language of the title and abstract
# because the language of some papers are inaccurately classified. 
# - extract authors' institutional affiliation data (country-level?)
# - Use nationalize and genderize to classify author gender. $70/month.
# 
# ---------------------------------------------------




# ---------------------------------------------------
# 
# Then: Analysis. 
# - Linguistic diffusion and language diversification or homogenization over time. t1 - tn 
# - Co-citation: which papers cite several software? how many? linguistic coupling accompnying citation coupling?
# - Geographic diffusion (using institutional affiliation). 
# 
# - Gender analysis. How many of the authors are identified as male/female/Unknown?
# - longitudinal /temporal and structural citation and collaboration network analysis 
# - visualization of collaboration and citation network  
# - Scholarly impact measures (citations) 
# 
# ---------------------------------------------------
