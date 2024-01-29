source("dependencies.R")

labels = list(
  "audit_type_condensed" = "Audit Type"
)

palette = "Set2"
pattern_theme = function(field) {
  list(
    ggpattern::scale_pattern_discrete(
      name=labels[[field]],
      choices=c('stripe', 'circle', 'crosshatch')
    ),
    shades::lightness(ggpattern::scale_pattern_fill_brewer(name=labels[[field]], palette=palette), scalefac(0.8)),
    shades::lightness(ggpattern::scale_pattern_colour_brewer(name=labels[[field]], palette=palette), scalefac(0.8)),
    ggpattern::scale_pattern_angle_discrete(name=labels[[field]], range=c(5, 85)),
    scale_colour_brewer(name=labels[[field]], palette=palette),
    shades::lightness(scale_fill_brewer(name=labels[[field]], palette=palette), scalefac(1.1)),
    theme_bw()
  )
}


plot_bar = function(df, x_col) {
  ggplot(df, aes(
    x=!!sym(x_col), fill=audit_type_condensed,
    pattern=audit_type_condensed,
    pattern_fill=audit_type_condensed,
    pattern_colour=audit_type_condensed,
    pattern_angle=audit_type_condensed
  )) +
    ggpattern::geom_bar_pattern(
      color="white",
      pattern_density=0.02,
      pattern_spacing=0.05
    ) +
    coord_flip() +
    scale_y_continuous(expand=c(0, 0)) +
    pattern_theme("audit_type_condensed") +
    ylab("# audit studies") +
    guides(
      pattern=guide_legend(ncol=2)
    ) +
    theme_classic() +
    theme(
      legend.position = "top",
      axis.text.y = element_markdown()
    )
}

get_words = function(column) {
  academic %>%
    unnest_tokens(output=word, input=!!sym(column)) %>%
    anti_join(stop_words) %>%
    filter(nchar(word) > 2)
}
get_2grams = function(column) {
  academic %>%
    unnest_tokens(output=word, input=!!sym(column), token="ngrams", n=2, stopwords=stop_words$word) %>%
    anti_join(stop_words) %>%
    filter(nchar(word) > 2)
}
count_words = function(words) {
  words %>%
    mutate(
      n_papers_total = n_distinct(paper_id)
    ) %>%
    group_by(word, .add=T) %>%
    summarise(
      n = n(),
      n_papers = n_distinct(paper_id),
      n_papers_total = first(n_papers_total),
      .groups="drop_last"
    ) %>%
    mutate(
      p_papers = n_papers / n_papers_total
    )
}

kwds_motivations_harms = c(
  # general audit motivation
  "bias", "biases", "biased", "fairness", "fair", "accountability", "accountable", "evaluate", "evaluation",
  "evaluations", "evaluating", "audit", "audits", "auditing", "ethics", "ethical", "impact", "impacts", "privacy", "transparency",
  "impact assessment", "impact assessments", "discrimination", "discriminatory", "justice", "governance", "explainability", "explanations",
  "explanation", "xai", "harm", "harms", "risk", "interpretability", "misinformation", "security", "surveillance", "trust", "accessibility",
  "disparate impact", "disparity", "disparities", "compliance", "mitigation", "mitigate", "parity", "power", "radicalization", "responsible",
  "robustness", "safety", "stereotypes", "violence", "hate speech", "gender bias", "data protection", "age bias", "racial bias"
)
kwds_groups = c(
  # types of harms --- social categories
  "gender", "race", "racial", "age", "black", "demographic", "demographics", "disability", "ethnicity"
)
kwds_targets = c(
  # audit targets
  "computer vision", "language processing", "natural language", "social media", "social networks", "facebook", "twitter", "youtube", "facial recognition",
  "face recognition", "multimodal", "platform", "platforms", "generative", "affective",
  "genotyping", "search engines", "google", "data mining", "speaker recognition", "speaker verification",
  "speech recognition", "voice assistant",
  # target domains
  "health", "healthcare", "medical", "advertising", "ads", "credit", "social credit", "criminal justice", "education", "government",
  "hiring", "employment", "policing", "police", "welfare", "child welfare", "elections", "forensic", "news",
  "automated decision", "risk assessment"
)
kwds_methods = c(
  # methods
  "participatory", "participatory design", "hci", "sociotechnical", "socio technical", "qualitative", "ethnography", "interviews", "community", "communities", "interdisciplinary",
  "advocacy", "facct", "benchmark", "benchmarks", "ecosystem", "human centered"
)

count_words_selected = function(words, filter_list) {
  if (missing(filter_list)) {
    filter_list = c(kwds_motivations_harms, kwds_targets, kwds_methods, kwds_groups)
  }
  words %>%
    mutate(
      n_papers_total = n_distinct(paper_id),
      term = fct_collapse(
        as.factor(word),
        "bias/biases/biased" = c("bias", "biases", "biased"),
        "fairness/fair" = c("fairness", "fair"),
        "accountability/accountable" = c("accountability", "accountable"),
        "evaluate/evaluation/evaluations/evaluating" = c("evaluate", "evaluation", "evaluations", "evaluating"),
        "audit/audits/auditing" = c( "audit", "audits", "auditing"),
        "ethics/ethical" = c("ethics", "ethical"),
        "impact/impacts" = c("impact", "impacts"),
        "impact assessment(s)" = c("impact assessment", "impact assessments"),
        "discrimination/discriminatory" = c("discrimination", "discriminatory"),
        "explainability/explanations/explanation/xai" = c("explainability", "explanations", "explanation", "xai"),
        "harm/harms" = c("harm", "harms"),
        "disparate impact / disparity / disparities" = c("disparate impact", "disparity", "disparities"),
        "race/racial/black" = c("race", "racial", "black"),
        "demographic/demographics" = c("demographic", "demographics"),
        "natural language / language processing" = c("natural language", "language processing"),
        "social media / social networks / facebook / twitter / youtube" = c("social media", "social networks", "facebook", "twitter", "youtube"),
        "facial recognition / face recognition" = c("facial recognition", "face recognition"),
        "platform/platforms" = c("platform", "platforms"),
        "speech recognition / speaker recognition / speaker verification" = c("speaker recognition", "speaker verification", "speech recognition"),
        "health/healthcare/medical" = c("health", "healthcare", "medical"),
        "advertising/ads" = c("advertising", "ads"),
        "hiring/employment" = c("hiring", "employment"),
        "policing/police" = c("policing", "police"),
        "sociotechnical" = c("sociotechnical", "socio technical"),
        "community/communities" = c("community", "communities"),
        "qualitative/ethnography/interviews/interviewed/workshop(s)" = c("qualitative", "ethnography", "interviews", "interviewed", "workshops", "workshop"),
        "benchmark/benchmarks" = c("benchmark", "benchmarks")
      )
    ) %>%
    filter(
      word %in% filter_list
    ) %>%
    group_by(term, .add=T) %>%
    summarise(
      n = n(),
      n_papers = n_distinct(paper_id),
      n_papers_total = first(n_papers_total),
      .groups="drop_last"
    ) %>%
    mutate(
      p_papers = n_papers / n_papers_total,
      p_papers_formatted = sprintf("%0.1f", p_papers*100),
      n_p_papers = str_c(n_papers, " (", p_papers_formatted, "\\%)"),
    )
}

print_table = function(counts) {
  counts %>%
    select(term, n, n_p_papers) %>% xtable() %>% print(type="latex", include.rownames=F)
}

print_table_pivot = function(counts, filter_max_t) {
  counts %>%
    mutate(
      p_papers_formatted = ifelse(
        p_papers > filter_max_t,
        sprintf("\\textbf{%0.1f}", p_papers*100),
        sprintf("%0.1f", p_papers*100)
      ),
      n_p_papers = str_c(n_papers, " (", p_papers_formatted, "\\%)"),
    ) %>%
    filter_max(filter_max_t) %>%
    # slice_max(order_by=n_papers, n=10) %>%
    arrange(desc(n)) %>%
    pivot_wider(id_cols=term, names_from=audit_type_condensed, values_from=n_p_papers, values_fill="0") %>%
    # drop_na() %>%
    mutate(
      `Ecosystem Audit` = if("Ecosystem Audit" %in% names(.)) `Ecosystem Audit` else NA,
      `Data Audit` = if("Data Audit" %in% names(.)) `Data Audit` else NA
    ) %>%
    relocate(`Ecosystem Audit`) %>% relocate(`Product/Model/Algo. Audit`) %>% relocate(`Data Audit`) %>% relocate(term) %>%
    xtable() %>% 
    print(type="latex", include.rownames=F, sanitize.text.function=identity)
}
filter_max = function(counts, t) {
  counts  %>%
    group_by(term) %>%
    mutate(
      p_papers_max = max(p_papers)
    ) %>%
    filter(p_papers_max > t)
}