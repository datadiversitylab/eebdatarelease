library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(here)

df <- read.csv(here("data", "software_doi.csv"))
df <- df[!df$language == "",]
df <- df[!df$Term == "",]

min_year_span = 3

# Unique papers dataset
papers_unique <- df %>%
  select(id, language, Term, publication_year) %>%
  distinct()

summary_stats <- papers_unique %>%
  group_by(publication_year, Term, language) %>%
  summarise(paper_count = n(), .groups = 'drop') %>%
  arrange(publication_year, Term, language)

summary_stats

# Check if we have enough years for trend analysis
year_range <- range(papers_unique$publication_year)
year_span <- year_range[2] - year_range[1] + 1

if (year_span < min_year_span) {
  print(paste("\nWarning: Only", year_span, "year(s) of data available."))
  print("Trend analysis works best with at least 3+ years of data.")
}

# Create proportion data by year and term
yearly_proportions <- papers_unique %>%
  group_by(publication_year, Term) %>%
  mutate(total_papers_term_year = n()) %>%
  group_by(publication_year, Term, language) %>%
  summarise(
    papers_in_language = n(),
    total_papers = first(total_papers_term_year),
    proportion = papers_in_language / total_papers,
    .groups = 'drop'
  )

# Statistical Tests for Each Term
terms <- unique(papers_unique$Term)
test_results <- list()

for (term in terms) {
  cat("\n--- ANALYSIS FOR TERM:", term, "---\n")
  
  term_data <- papers_unique %>% filter(Term == term)
  
  # Check if we have multiple years and languages for this term
  n_years <- length(unique(term_data$publication_year))
  n_languages <- length(unique(term_data$language))
  
  if (n_years < 2) {
    cat("Insufficient years for trend analysis (only", n_years, "year)\n")
    next
  }
  
  if (n_languages < 2) {
    cat("Insufficient languages for comparison (only", n_languages, "language)\n")
  }
  
  # Create contingency table: Year × Language
  cont_table <- table(term_data$publication_year, term_data$language)
  print("Contingency table (Year × Language):")
  print(cont_table)
  
  # Chi-square test for independence
  if (min(cont_table) >= 5 && nrow(cont_table) > 1 && ncol(cont_table) > 1) {
    chi_test <- chisq.test(cont_table)
    cat("\nChi-square test results:\n")
    cat("Chi-square statistic:", round(chi_test$statistic, 4), "\n")
    cat("p-value:", round(chi_test$p.value, 4), "\n")
    cat("Degrees of freedom:", chi_test$parameter, "\n")
    
    if (chi_test$p.value < 0.05) {
      cat("Result: Language use SIGNIFICANTLY changed over years for", term, "(p < 0.05)\n")
    } else {
      cat("Result: No significant change in language use over years for", term, "(p >= 0.05)\n")
    }
    
    # Effect size (Cramér's V)
    n_total <- sum(cont_table)
    r <- nrow(cont_table)
    c <- ncol(cont_table)
    cramers_v <- sqrt(chi_test$statistic / (n_total * (min(r, c) - 1)))
    cat("Cramér's V (effect size):", round(cramers_v, 3), "\n")
    
    cat("Using Fisher's exact test (small sample size):\n")
    fisher_test <- fisher.test(cont_table, simulate.p.value = TRUE, B = 10000)
    cat("Fisher's exact test p-value:", round(fisher_test$p.value, 4), "\n")
    
    if (fisher_test$p.value < 0.05) {
      cat("Result: Language use SIGNIFICANTLY changed over years for", term, "(p < 0.05)\n")
    } else {
      cat("Result: No significant change in language use over years for", term, "(p >= 0.05)\n")
    }
    
    # Trend analysis for each language within this term
    
    languages <- unique(term_data$language)
    for (lang in languages) {
      lang_yearly <- term_data %>%
        filter(language == lang) %>%
        group_by(publication_year) %>%
        summarise(count = n(), .groups = 'drop')
      
      # Get total papers per year for this term
      total_yearly <- term_data %>%
        group_by(publication_year) %>%
        summarise(total = n(), .groups = 'drop')
      
      # Merge and calculate proportions
      trend_data <- merge(lang_yearly, total_yearly, by = "publication_year", all.y = TRUE)
      trend_data$count[is.na(trend_data$count)] <- 0
      trend_data$proportion <- trend_data$count / trend_data$total
      
      if (nrow(trend_data) >= 3) {
        # Linear regression: proportion ~ year
        lm_model <- lm(proportion ~ publication_year, data = trend_data)
        lm_summary <- summary(lm_model)
        
        cat("Language:", lang, "\n")
        cat("Trend slope:", round(coef(lm_model)[2], 6), "per year\n")
        cat("R-squared:", round(lm_summary$r.squared, 3), "\n")
        cat("p-value for trend:", round(lm_summary$coefficients[2, 4], 4), "\n")
        
        if (lm_summary$coefficients[2, 4] < 0.05) {
          trend_direction <- ifelse(coef(lm_model)[2] > 0, "increasing", "decreasing")
          cat("Result: SIGNIFICANT", trend_direction, "trend over time\n")
        } else {
          cat("Result: No significant trend over time\n")
        }
      }
    }
    
    # Store results
    test_results[[term]] <- list(
      contingency_table = cont_table,
      n_years = n_years,
      n_languages = n_languages
    )
  }
}

# Overall test across all terms

# Create a combined variable for interaction testing
papers_unique$year_term <- paste(papers_unique$publication_year, papers_unique$Term, sep = "_")

# Test if language distribution differs across year-term combinations
overall_table <- table(papers_unique$year_term, papers_unique$language)

overall_chi <- chisq.test(overall_table)
cat("\nOverall Chi-square test results:\n")
cat("Chi-square statistic:", round(overall_chi$statistic, 4), "\n")
cat("p-value:", round(overall_chi$p.value, 4), "\n")

if (overall_chi$p.value < 0.05) {
  cat("Result: Language use patterns SIGNIFICANTLY differ across year-term combinations\n")
} else {
  cat("Result: No significant differences in language patterns across year-term combinations\n")
}


# Language proportions over time by term
p1 <- yearly_proportions %>%
  ggplot(aes(x = publication_year, y = proportion, color = language)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~Term, scales = "free") +
  labs(
    title = "Language Use Trends Over Time by Software",
    subtitle = "Proportion of papers in each language by year and software",
    x = "Publication Year",
    y = "Proportion of Papers",
    color = "Language"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0,1)

print(p1)

# Absolute counts
count_data <- papers_unique %>%
  group_by(publication_year, Term, language) %>%
  summarise(count = n(), .groups = 'drop')

p2 <- count_data %>%
  ggplot(aes(x = publication_year, y = count, fill = language)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Term, scales = "free") +
  labs(
    title = "Language Use Over Time by Term (Absolute Counts)",
    subtitle = "Number of papers by language, year, and term",
    x = "Publication Year",
    y = "Number of Papers",
    fill = "Language"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

