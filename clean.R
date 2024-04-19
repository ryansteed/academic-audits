source("dependencies.R")

factors = c(
  "source_sheet",
  "audit_type"
)

# load Google Sheets export (all papers, including non-academic)
papers = read.csv("papers.csv", na.strings=c(" ", "", "NA", "NaN")) %>%
  clean_names()
print("Rows:")
print(nrow(papers))
print("Dupes:")
print(nrow(papers %>% filter(audit_type == "Duplicate")))

papers = papers %>%
  filter(audit_type != "Duplicate") %>%
  rename(
    urls = "ur_ls"
  ) %>%
  mutate_at(factors, as.factor) %>%
  mutate(
    is_academic = is_academic == "Y",
    source_detailed = as.factor(ifelse(
      grepl("aclanthology", urls) | grepl("ACL", journal_series_institution),
      "[URL Match | Institution Match] ACL",
      # ifelse(
      #   journal_series_institution == "CSCW",
      #   "[Institution Match] CSCW",
        as.character(source_sheet)
      # )
    )),
    source = fct_recode(source_detailed, 
      `ACL` = "[URL Match | Institution Match] ACL",
      FAccT = "ACMDL - FAccT",
      AIES = "ACMDL - AIES",
      Markup = "ACMDL - Markup",
      Propublica = "ACMDL - Propublica"
    ),
    source = fct_collapse(source,
      `Other non-ACM` = c(levels(source)[startsWith(levels(source), "OAT")])
    ),
    audit_type_condensed = fct_collapse(
      audit_type, `Meta-Commentary` = c("Method", "Tool", "Meta-Commentary", "Critique")
    ),
    journal_series_institution = ifelse(source == "Markup", "Markup", journal_series_institution),
    journal_series_institution = ifelse(source == "Propublica", "Propublica", journal_series_institution)
  ) %>%
  tibble::rowid_to_column("paper_id")

all = papers %>%
  select(
    source, audit_type_condensed, authors, title, urls,
    journal_series_institution, year, keywords, abstract, audit_type
  ) %>%
  arrange(source, year)
all %>%
  write.csv(., file = "all-audit-studies.csv")
print("Wrote cleaned audit studies to `all-audit-studies.csv`, length:")
print(nrow(all))

academic = papers %>%
  # only papers resulting from principled search
  filter(is_academic, year > 2017, year < 2023) %>%
  select(
    source, audit_type_condensed, authors, title, urls,
    journal_series_institution, year, keywords, abstract, source_detailed, audit_type, paper_id
  ) %>%
  arrange(source, year)

academic %>%
  write.csv(., file = "academic-audit-studies.csv")
print("Wrote cleaned academic audit studies to `academic-audit-studies.csv`, length:")
print(nrow(academic))


