# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(igraph)
library(networkD3)
library(stringr)
library(corrplot)
library(survival)
library(broom)
library(wordcloud)
library(RColorBrewer)

df <- read.csv(here("data", "software_doi.csv"))
df <- df[!df$language == "",]
df <- df[!df$Term == "",]
df <- df[!df$institution_country_code == "",]

# =============================================================================
# RESEARCH QUESTION 1: INTERNATIONAL COLLABORATION PATTERNS
# =============================================================================


  # Papers with authors from multiple countries
  collaboration_data <- df %>%
    group_by(id, Term, publication_year) %>%
    summarise(
      unique_countries = n_distinct(institution_country_code),
      countries = paste(unique(institution_country_code), collapse = ", "),
      author_count = first(author_count),
      .groups = 'drop'
    ) %>%
    mutate(
      collaboration_type = case_when(
        unique_countries == 1 ~ "Domestic",
        unique_countries == 2 ~ "Bilateral",
        unique_countries >= 3 ~ "Multilateral"
      )
    )

  # 1. Collaboration trends over time
  collab_trends <- collaboration_data %>%
    group_by(publication_year, collaboration_type) %>%
    summarise(papers = n(), .groups = 'drop') %>%
    group_by(publication_year) %>%
    mutate(proportion = papers / sum(papers))

  print("Collaboration trends over time:")
  print(collab_trends)

  # Statistical test: Has international collaboration increased over time?
  international_papers <- collaboration_data %>%
    mutate(is_international = unique_countries > 1)

  # Logistic regression: international ~ year
  logit_model <- glm(is_international ~ publication_year,
                     data = international_papers,
                     family = binomial)

  cat("\nLogistic regression: International collaboration ~ Year\n")
  cat("Coefficient (log-odds):", round(coef(logit_model)[2], 4), "\n")
  cat("P-value:", round(summary(logit_model)$coefficients[2, 4], 4), "\n")

  # 2. Collaboration by research term
  collab_by_term <- collaboration_data %>%
    group_by(Term, collaboration_type) %>%
    summarise(papers = n(), .groups = 'drop') %>%
    group_by(Term) %>%
    mutate(proportion = papers / sum(papers))

  print("\nCollaboration patterns by research term:")
  print(collab_by_term)

  # 3. Visualization
  p1 <- ggplot(collab_trends, aes(x = publication_year, y = proportion,
                                  color = collaboration_type)) +
    geom_line(size = 1) + geom_point(size = 2) +
    labs(title = "International Collaboration Trends Over Time",
         x = "Year", y = "Proportion of Papers") +
    theme_minimal()

  print(p1)

# =============================================================================
# RESEARCH QUESTION 2: COUNTRY DOMINANCE AND RESEARCH LEADERSHIP
# =============================================================================

  # 1. First author country analysis (research leadership)
  first_author_countries <- df %>%
    filter(author_position == "first") %>%
    group_by(institution_country_code, Term, publication_year) %>%
    summarise(papers_led = n(), .groups = 'drop')

  # Overall leadership by country
  country_leadership <- first_author_countries %>%
    group_by(institution_country_code) %>%
    summarise(total_papers_led = sum(papers_led)) %>%
    arrange(desc(total_papers_led))

  print("Research leadership by country (first author papers):")
  print(head(country_leadership, 10))

  # 2. Country participation vs leadership
  participation <- df %>%
    group_by(institution_country_code) %>%
    summarise(total_participation = n_distinct(id)) %>%
    arrange(desc(total_participation))

  leadership <- first_author_countries %>%
    group_by(institution_country_code) %>%
    summarise(total_leadership = sum(papers_led))

  country_comparison <- merge(participation, leadership,
                              by = "institution_country_code", all = TRUE)
  country_comparison[is.na(country_comparison)] <- 0
  country_comparison$leadership_ratio <- country_comparison$total_leadership /
    country_comparison$total_participation

  print("\nCountry participation vs leadership:")
  print(head(country_comparison[order(-country_comparison$leadership_ratio),], 10))

  # 3. Test: Do certain countries dominate specific research terms?
  country_term_matrix <- df %>%
    filter(author_position == "first") %>%
    group_by(institution_country_code, Term) %>%
    summarise(papers = n(), .groups = 'drop') %>%
    pivot_wider(names_from = Term, values_from = papers, values_fill = 0)


    chi_matrix <- as.matrix(country_term_matrix[,-1])
    rownames(chi_matrix) <- country_term_matrix[[1]]


      chi_test <- chisq.test(chi_matrix)
      cat("\nChi-square test: Country specialization in research terms\n")
      cat("P-value:", round(chi_test$p.value, 4), "\n")

      if (chi_test$p.value < 0.05) {
        cat("Result: Countries show SIGNIFICANT specialization in different terms\n")
      } else {
        cat("Result: No significant country specialization by term\n")
      }

country_comparison ##Need to map this


# =============================================================================
# RESEARCH QUESTION 3: TEAM SIZE AND COLLABORATION EFFECTIVENESS
# =============================================================================

  # 1. Team size trends over time
  team_size_data <- df %>%
    select(id, author_count, publication_year, Term) %>%
    distinct()

  # Correlation between year and team size
  cor_test <- cor.test(team_size_data$publication_year, team_size_data$author_count)
  cat("Correlation between year and team size:\n")
  cat("Correlation coefficient:", round(cor_test$estimate, 3), "\n")
  cat("P-value:", round(cor_test$p.value, 4), "\n")

  # 2. Team size by research term
  team_by_term <- team_size_data %>%
    group_by(Term) %>%
    summarise(
      mean_team_size = mean(author_count),
      median_team_size = median(author_count),
      sd_team_size = sd(author_count),
      papers = n()
    )

  print("Team size statistics by research term:")
  print(team_by_term)

  # ANOVA: Does team size differ significantly across terms?

    anova_result <- aov(author_count ~ Term, data = team_size_data)
    cat("\nANOVA: Team size differences across terms\n")
    cat("P-value:", round(summary(anova_result)[[1]][["Pr(>F)"]][1], 4), "\n")

    if (summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
      cat("Result: SIGNIFICANT differences in team size across terms\n")

      # Post-hoc test
      tukey_result <- TukeyHSD(anova_result)
      print("Tukey post-hoc comparisons:")
      print(tukey_result)
    }

  # 3. Team composition analysis
  team_composition <- df %>%
    group_by(id, Term) %>%
    summarise(
      team_size = first(author_count),
      countries_involved = n_distinct(institution_country_code),
      diversity_ratio = countries_involved / team_size,
      .groups = 'drop'
    )

  # Correlation: team size vs international diversity
  diversity_cor <- cor.test(team_composition$team_size,
                            team_composition$diversity_ratio)
  cat("\nCorrelation: Team size vs international diversity\n")
  cat("Correlation:", round(diversity_cor$estimate, 3), "\n")
  cat("P-value:", round(diversity_cor$p.value, 4), "\n")


# =============================================================================
# RESEARCH QUESTION 4: AUTHOR POSITION AND CAREER PATTERNS
# =============================================================================


  # 1. Author position distribution
  position_dist <- df %>%
    group_by(author_position) %>%
    summarise(count = n(), proportion = n() / nrow(df))

  print("Author position distribution:")
  print(position_dist)

  # 2. Country differences in authorship patterns
  country_positions <- df %>%
    group_by(institution_country_code, author_position) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(institution_country_code) %>%
    mutate(proportion = count / sum(count)) %>%
    filter(author_position %in% c("first", "last"))  # Focus on key positions

  # Test: Do countries differ in first/last authorship patterns?
  position_matrix <- country_positions %>%
    select(institution_country_code, author_position, count) %>%
    pivot_wider(names_from = author_position, values_from = count, values_fill = 0)

  if ("first" %in% names(position_matrix) && "last" %in% names(position_matrix)) {
    # Chi-square test
    chi_matrix <- as.matrix(position_matrix[, c("first", "last")])
    rownames(chi_matrix) <- position_matrix$institution_country_code

    if (min(chi_matrix) >= 5 && nrow(chi_matrix) > 1) {
      chi_test <- chisq.test(chi_matrix)
      cat("Chi-square test: Country differences in first/last authorship\n")
      cat("P-value:", round(chi_test$p.value, 4), "\n")
    }
  }

  # 3. Career progression analysis (if we have individual author data)
  author_progression <- df %>%
    group_by(au_id, au_display_name) %>%
    summarise(
      papers = n(),
      years_active = max(publication_year) - min(publication_year) + 1,
      first_papers = sum(author_position == "first"),
      last_papers = sum(author_position == "last"),
      middle_papers = sum(author_position == "middle"),
      .groups = 'drop'
    ) %>%
    filter(papers >= 2) %>%  # Authors with multiple papers
    mutate(
      first_author_rate = first_papers / papers,
      last_author_rate = last_papers / papers,
      career_stage = case_when(
        first_author_rate > 0.7 ~ "Early Career",
        last_author_rate > 0.5 ~ "Senior/PI",
        TRUE ~ "Mid-Career"
      )
    )

  print("Author career stage distribution:")
  print(table(author_progression$career_stage))

# =============================================================================
# RESEARCH QUESTION 5: TEMPORAL RESEARCH PATTERNS AND EVOLUTION
# =============================================================================

  # 1. Research term evolution over time
  term_evolution <- df %>%
    group_by(publication_year, Term) %>%
    summarise(papers = n_distinct(id), .groups = 'drop') %>%
    group_by(publication_year) %>%
    mutate(proportion = papers / sum(papers))

  # Test for trend in each term
  terms <- unique(term_evolution$Term)
  for (term in terms) {
    term_data <- term_evolution %>% filter(Term == term)

    if (nrow(term_data) >= 3) {
      trend_test <- cor.test(term_data$publication_year, term_data$proportion)
      cat("Term:", term, "- Trend correlation:", round(trend_test$estimate, 3),
          "P-value:", round(trend_test$p.value, 4), "\n")
    }
  }

  # 2. Research productivity cycles
  yearly_productivity <- df %>%
    group_by(publication_year) %>%
    summarise(
      total_papers = n_distinct(id),
      total_authors = n_distinct(au_id),
      papers_per_author = total_papers / total_authors
    )

  print("Research productivity over time:")
  print(yearly_productivity)

  # 3. Emerging vs declining research areas
  # Calculate growth rates for each term
  term_growth <- term_evolution %>%
    group_by(Term) %>%
    arrange(publication_year) %>%
    mutate(
      papers_growth = (papers - lag(papers)) / lag(papers),
      proportion_growth = (proportion - lag(proportion)) / lag(proportion)
    ) %>%
    summarise(
      avg_growth_rate = mean(papers_growth, na.rm = TRUE),
      avg_proportion_growth = mean(proportion_growth, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(avg_growth_rate))

  print("Research term growth rates:")
  print(term_growth)


# =============================================================================
# RESEARCH QUESTION 6: NETWORK ANALYSIS - COLLABORATION NETWORKS
# =============================================================================

  # 1. Author collaboration network
  author_pairs <- df %>%
    select(id, au_id, au_display_name) %>%
    distinct() %>%
    group_by(id) %>%
    do({
      authors <- .$au_id
      if (length(authors) > 1) {
        combinations <- combn(authors, 2)
        data.frame(
          author1 = combinations[1, ],
          author2 = combinations[2, ],
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(author1 = character(0), author2 = character(0))
      }
    }) %>%
    ungroup()

  if (nrow(author_pairs) > 0) {
    # Count collaborations
    author_network <- author_pairs %>%
      group_by(author1, author2) %>%
      summarise(collaborations = n(), .groups = 'drop')

    # Create network graph
    if (nrow(author_network) > 0) {
      g <- graph_from_data_frame(author_network, directed = FALSE)

      cat("Network statistics:\n")
      cat("Number of authors:", vcount(g), "\n")
      cat("Number of collaborations:", ecount(g), "\n")
      cat("Network density:", round(edge_density(g), 4), "\n")
      cat("Average clustering coefficient:", round(transitivity(g), 4), "\n")

      # Find most connected authors
      degrees <- degree(g)
      top_authors <- head(sort(degrees, decreasing = TRUE), 5)
      cat("Most connected authors:\n")
      print(top_authors)
    }
  }

  # 2. Country collaboration network
  country_pairs <- df %>%
    select(id, institution_country_code) %>%
    distinct() %>%
    group_by(id) %>%
    filter(n_distinct(institution_country_code) > 1) %>%  # Only international papers
    do({
      countries <- unique(.$institution_country_code)
      if (length(countries) > 1) {
        combinations <- combn(countries, 2)
        data.frame(
          country1 = combinations[1, ],
          country2 = combinations[2, ],
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(country1 = character(0), country2 = character(0))
      }
    }) %>%
    ungroup()

  if (nrow(country_pairs) > 0) {
    country_network <- country_pairs %>%
      group_by(country1, country2) %>%
      summarise(collaborations = n(), .groups = 'drop')

    print("Top country collaborations:")
    print(head(country_network[order(-country_network$collaborations),], 10))
  }


# =============================================================================
# RESEARCH QUESTION 7: GEOGRAPHIC RESEARCH PATTERNS
# =============================================================================

  # 1. Research concentration by region
  # You might want to add a region mapping
  region_mapping <- data.frame(
    institution_country_code = c("US", "KR", "CN", "JP", "DE", "UK", "FR", "CA"),
    region = c("North America", "East Asia", "East Asia", "East Asia",
               "Europe", "Europe", "Europe", "North America"),
    stringsAsFactors = FALSE
  )

  geographic_data <- df %>%
    left_join(region_mapping, by = "institution_country_code") %>%
    mutate(region = ifelse(is.na(region), "Other", region))

  # Regional research output
  regional_output <- geographic_data %>%
    group_by(region, Term) %>%
    summarise(papers = n_distinct(id), .groups = 'drop') %>%
    group_by(region) %>%
    mutate(proportion = papers / sum(papers))

  print("Regional research focus:")
  print(regional_output)

  # 2. Research diversity by country
  country_diversity <- df %>%
    group_by(institution_country_code) %>%
    summarise(
      total_papers = n_distinct(id),
      research_terms = n_distinct(Term),
      diversity_index = research_terms / total_papers,
      .groups = 'drop'
    ) %>%
    filter(total_papers >= 3) %>%  # Countries with at least 3 papers
    arrange(desc(diversity_index))

  print("Research diversity by country (countries with 3+ papers):")
  print(head(country_diversity, 10))



