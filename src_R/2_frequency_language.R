library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

df <- read.csv(here("data", "software_doi.csv"))
df <- df[!df$language == "",]
df <- df[!df$Term == "",]


# Step 1: Create a unique paper-level dataset
# Since your data has multiple rows per paper (due to multiple authors),
# we need to get unique papers first
papers_unique <- df %>%
  select(id, language, Term, publication_year) %>%
  distinct()

# Step 2: Create contingency table
contingency_table <- table(papers_unique$Term, papers_unique$language)
print(contingency_table)

# Step 3: Calculate proportions
prop_table <- prop.table(contingency_table, margin = 1) # Row proportions
print(round(prop_table, 3))

# Step 4: Statistical Tests
chi_test <- chisq.test(contingency_table)
print("\n=== CHI-SQUARE TEST ===")
print(paste("Chi-square statistic:", round(chi_test$statistic, 4)))
print(paste("p-value:", round(chi_test$p.value, 4)))
print(paste("Degrees of freedom:", chi_test$parameter))

if(chi_test$p.value < 0.05) {
  print("Result: Language frequency SIGNIFICANTLY differs between terms (p < 0.05)")
} else {
 print("Result: No significant difference in language frequency between terms (p >= 0.05)")
}

# Step 5: Effect size (Cramér's V for association strength)
cramers_v <- function(chi_stat, n, r, c) {
  sqrt(chi_stat / (n * (min(r, c) - 1)))
}

if(exists("chi_test")) {
  n_total <- sum(contingency_table)
  r <- nrow(contingency_table)
  c <- ncol(contingency_table)
  cv <- cramers_v(chi_test$statistic, n_total, r, c)

  print(paste("\nCramér's V (effect size):", round(cv, 3)))
  if(cv < 0.1) print("Effect size: Negligible; The relationship exists but is so small it's practically meaningless")
  else if(cv < 0.3) print("Effect size: Small")
  else if(cv < 0.5) print("Effect size: Medium")
  else print("Effect size: Large")
}


# Plot
ggplot(papers_unique, aes(x = Term, fill = language)) +
  geom_bar(position = "fill") +
  labs(title = "Language Distribution by Software",
       subtitle = "Proportion of languages within each software",
       x = "Software",
       y = "Proportion",
       fill = "Language") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette="Set3")
