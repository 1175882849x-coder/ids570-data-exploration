
### Set up ----
# Extract the archive into the texts folder
unzip("Te_t_Files_export.zip", exdir = "texts")

# List all .txt files with their paths
text_files <- list.files(
  path = "texts",
  pattern = "\\.txt$",
  full.names = TRUE
)

# Check the number of files
length(text_files)

# Display the file paths
text_files

# Load packages
library(readr)
library(dplyr)

# Read each file into one character string
text_contents <- vapply(
  text_files,
  read_file,
  FUN.VALUE = character(1)
)

# Create a table with one row per document
texts_df <- tibble(
  document = basename(text_files),
  text = unname(text_contents)
)

# Check the table dimensions
dim(texts_df)

# Preview document names and the first 100 characters
texts_df %>%
  transmute(
    document,
    preview = substr(text, 1, 100)
  )

### Prepossing ----

# Load string-processing functions
library(stringr)

# Count long s characters in each original document
texts_df <- texts_df %>%
  mutate(
    long_s_before = str_count(text, fixed("\u017f"))
  )

# Display documents containing long s
texts_df %>%
  filter(long_s_before > 0) %>%
  select(document, long_s_before)

# Create a normalized copy while preserving the original text
texts_df <- texts_df %>%
  mutate(
    text_normalized = str_replace_all(text, fixed("\u017f"), "s"),
    long_s_after = str_count(text_normalized, fixed("\u017f"))
  )

# Check total counts before and after normalization
texts_df %>%
  summarise(
    total_before = sum(long_s_before),
    total_after = sum(long_s_after)
  )

# Count black-dot markers in each document
texts_df <- texts_df %>%
  mutate(
    damaged_markers = str_count(text_normalized, fixed("\u25cf"))
  )

# Display documents containing these markers
texts_df %>%
  filter(damaged_markers > 0) %>%
  select(document, damaged_markers)

# Inspect up to five affected strings per document
damage_examples <- texts_df %>%
  filter(damaged_markers > 0) %>%
  transmute(
    document,
    examples = vapply(
      str_extract_all(text_normalized, "\\S*\u25cf\\S*"),
      function(x) paste(head(unique(x), 5), collapse = ", "),
      FUN.VALUE = character(1)
    )
  )

print(damage_examples, width = Inf)

### Tokenization ----
# Load the text-analysis package
library(quanteda)

# Build a corpus from the normalized text
text_corpus <- corpus(
  texts_df,
  text_field = "text_normalized",
  docid_field = "document"
)

# Split each document into tokens
text_tokens <- tokens(text_corpus)

# Inspect the first 30 tokens in the first document
head(text_tokens[[1]], 30)

# Check the number of documents
ndoc(text_tokens)

# Tokenize again, excluding punctuation and symbols
lexical_tokens <- tokens(
  text_corpus,
  remove_punct = TRUE,
  remove_symbols = TRUE
)

# Convert tokens to lowercase
lexical_tokens <- tokens_tolower(lexical_tokens)

# Inspect the first 30 tokens
head(lexical_tokens[[1]], 30)

### Building DFM ----

# Build a document-feature matrix of token counts
dfm_counts <- dfm(
  lexical_tokens,
  tolower = FALSE
)

# Check the number of documents and features
ndoc(dfm_counts)
nfeat(dfm_counts)

# Inspect the first 5 documents and first 10 features
as.matrix(dfm_counts[1:5, 1:10])

# Inspect the English stopword list
head(stopwords("en"), 20)

# Remove stopwords from the count matrix
dfm_content <- dfm_remove(
  dfm_counts,
  pattern = stopwords("en"),
  valuetype = "fixed"
)

# Compare the number of features
c(
  before = nfeat(dfm_counts),
  after = nfeat(dfm_content)
)

### TI - IDF ----
# Calculate TF-IDF weights after removing stopwords
dfm_tfidf_weights <- dfm_tfidf(
  dfm_content,
  scheme_tf = "count",
  scheme_df = "inverse",
  base = 10
)

# Inspect the top 15 TF-IDF terms in the first document
topfeatures(
  dfm_tfidf_weights[1, ],
  n = 15
)

# Extract the top 15 TF-IDF terms from every document
tfidf_top15 <- bind_rows(
  lapply(seq_len(ndoc(dfm_tfidf_weights)), function(i) {
    
    scores <- topfeatures(
      dfm_tfidf_weights[i, ],
      n = 15
    )
    
    tibble(
      document = docnames(dfm_tfidf_weights)[i],
      rank = seq_along(scores),
      term = names(scores),
      tf_idf = as.numeric(scores)
    )
  })
)

# Check the dimensions and preview the results
dim(tfidf_top15)
print(tfidf_top15, n = 30)

# Inspect selected terms in their original token context
term_context <- kwic(
  text_tokens[1:2],
  pattern = c("p", "d", "s", "ll", "ss", "½", "0"),
  valuetype = "fixed",
  case_insensitive = TRUE,
  window = 6
)

# Show up to three examples for each matched term
term_context %>%
  as_tibble() %>%
  group_by(keyword) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  select(docname, pre, keyword, post) %>%
  print(n = Inf, width = Inf)

# Save the top 15 TF-IDF terms for all documents
write_csv(tfidf_top15, "tfidf_top15.csv")

### Pearson ----

# Retain terms occurring at least five times across the corpus
dfm_pearson <- dfm_trim(
  dfm_content,
  min_termfreq = 5
)

# Compare the number of features
c(
  before = nfeat(dfm_content),
  after = nfeat(dfm_pearson)
)

# Convert the count DFM to a regular matrix
count_matrix <- as.matrix(dfm_pearson)

# Calculate Pearson correlations between documents
r_mat <- cor(
  t(count_matrix),
  method = "pearson"
)

# Check the dimensions
dim(r_mat)

# Inspect the first five documents
round(r_mat[1:5, 1:5], 3)

# Load plotting and reshaping packages
library(ggplot2)
library(tidyr)

# Convert the correlation matrix to long format
heat_df <- as.data.frame(r_mat) %>%
  tibble::rownames_to_column("doc_i") %>%
  pivot_longer(
    cols = -doc_i,
    names_to = "doc_j",
    values_to = "r"
  )

# Inspect the reshaped data
head(heat_df)

# Create a similarity heatmap
ggplot(heat_df, aes(x = doc_j, y = doc_i, fill = r)) +
  geom_tile() +
  coord_fixed() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Pearson Correlation Between Documents",
    x = NULL,
    y = NULL,
    fill = "Pearson r"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# Locate unique document pairs, excluding self-comparisons
pair_indices <- which(
  upper.tri(r_mat),
  arr.ind = TRUE
)

# Create a table of document pairs and correlations
pair_results <- tibble(
  document_1 = rownames(r_mat)[pair_indices[, 1]],
  document_2 = colnames(r_mat)[pair_indices[, 2]],
  r = r_mat[pair_indices]
)

# Check the number of unique pairs
nrow(pair_results)

# Find the two most similar pairs
pair_results %>%
  arrange(desc(r)) %>%
  slice_head(n = 2)

# Find the two least similar pairs
pair_results %>%
  arrange(r) %>%
  slice_head(n = 2)

# Save the two most similar pairs
most_similar <- pair_results %>%
  arrange(desc(r)) %>%
  slice_head(n = 2)

# Save the two least similar pairs
least_similar <- pair_results %>%
  arrange(r) %>%
  slice_head(n = 2)

# Display more decimal places without changing the stored values
bind_rows(
  most_similar %>% mutate(comparison = "Most similar"),
  least_similar %>% mutate(comparison = "Least similar")
) %>%
  mutate(r_display = sprintf("%.6f", r)) %>%
  select(comparison, document_1, document_2, r_display)

# Compare distinctive vocabulary in the least similar pair
tfidf_top15 %>%
  filter(
    document %in% c(
      "A93819.txt",
      "B14801__Free_Trade.txt"
    )
  ) %>%
  arrange(document, rank) %>%
  print(n = 30)

# Inspect potentially editorial terms in Free Trade
free_trade_context <- kwic(
  text_tokens["B14801__Free_Trade.txt"],
  pattern = c("non-latin", "alphabet"),
  valuetype = "fixed",
  case_insensitive = TRUE,
  window = 8
)

# Display the first six matches
free_trade_context %>%
  as_tibble() %>%
  select(docname, pre, keyword, post) %>%
  slice_head(n = 6) %>%
  print(width = Inf)

# Compare distinctive vocabulary in the most similar pair
tfidf_top15 %>%
  filter(
    document %in% c("A32827.txt", "A32833.txt")
  ) %>%
  arrange(document, rank) %>%
  print(n = 30)

# Compare the opening passages
texts_df %>%
  filter(
    document %in% c("A32827.txt", "A32833.txt")
  ) %>%
  transmute(
    document,
    opening = substr(text_normalized, 1, 500)
  ) %>%
  print(width = Inf)

# Print both opening passages without tibble truncation
for (id in c("A32827.txt", "A32833.txt")) {
  
  passage <- texts_df$text_normalized[
    texts_df$document == id
  ]
  
  cat(
    "\nDocument:", id, "\n",
    substr(passage, 1, 500),
    "\n"
  )
}

# Extract the two complete normalized texts
text_a <- texts_df$text_normalized[
  texts_df$document == "A32827.txt"
]

text_b <- texts_df$text_normalized[
  texts_df$document == "A32833.txt"
]

# Compare exact text and then ignore capitalization
c(
  exactly_identical = identical(text_a, text_b),
  identical_ignoring_case = identical(
    str_to_lower(text_a),
    str_to_lower(text_b)
  )
)

# Compare distinctive vocabulary in the second most similar pair
tfidf_top15 %>%
  filter(
    document %in% c("A32836.txt", "A50763.txt")
  ) %>%
  arrange(document, rank) %>%
  print(n = 30)

# Compare distinctive vocabulary in the second least similar pair
tfidf_top15 %>%
  filter(
    document %in% c("A06791.txt", "A93819.txt")
  ) %>%
  arrange(document, rank) %>%
  print(n = 30)

# Select two documents for syntactic analysis
syntax_texts <- texts_df %>%
  filter(
    document %in% c("A06791.txt", "A93819.txt")
  ) %>%
  select(
    document,
    text = text_normalized
  )

# Check the selected documents and their character counts
syntax_texts %>%
  transmute(
    document,
    characters = nchar(text)
  )

### Syntactic complexity ----
# Load the parsing package
library(udpipe)

# Define the model location within the project
dir.create("models", showWarnings = FALSE)

model_path <- file.path(
  "models",
  "english-ewt-ud-2.5-191206.udpipe"
)

# Download the model only if it is missing
if (!file.exists(model_path)) {
  
  model_info <- udpipe_download_model(
    language = "english-ewt",
    model_dir = "models"
  )
  
  if (isTRUE(model_info$download_failed)) {
    stop(
      "Model download failed. Place the English EWT model ",
      "in the models folder, then run the script again."
    )
  }
}

# Load the model
ud_model <- udpipe_load_model(model_path)

# Annotate both documents with the loaded model
annotations <- udpipe_annotate(
  object = ud_model,
  x = syntax_texts$text,
  doc_id = syntax_texts$document
)

# Convert annotations to a data frame
anno_df <- as.data.frame(annotations)

# Save the annotations for reuse
saveRDS(anno_df, "syntax_annotations.rds")

# Inspect the first 15 tokens and their annotations
anno_df %>%
  select(
    doc_id,
    paragraph_id,
    sentence_id,
    token_id,
    token,
    lemma,
    upos,
    feats,
    head_token_id,
    dep_rel
  ) %>%
  head(15) %>%
  as_tibble() %>%
  print(width = Inf)

# Flag syntactic features using the Week 05 definitions
syntax_df <- anno_df %>%
  mutate(
    is_word = !is.na(upos) & upos != "PUNCT",
    
    is_clause =
      upos %in% c("VERB", "AUX") &
      str_detect(coalesce(feats, ""), "VerbForm=Fin"),
    
    is_dep_clause = dep_rel %in% c(
      "advcl", "ccomp", "xcomp", "acl", "acl:relcl"
    ),
    
    is_coord = dep_rel %in% c(
      "conj", "cc"
    ),
    
    is_complex_nominal = dep_rel %in% c(
      "amod", "nmod", "compound", "appos"
    )
  )

# Inspect the flags for the first 15 tokens
syntax_df %>%
  select(
    token,
    is_word,
    is_clause,
    is_dep_clause,
    is_coord,
    is_complex_nominal
  ) %>%
  head(15) %>%
  as_tibble() %>%
  print(width = Inf)

# Summarise syntactic features for each sentence
sentence_df <- syntax_df %>%
  filter(is_word) %>%
  group_by(doc_id, paragraph_id, sentence_id) %>%
  summarise(
    sentence = first(sentence),
    words = n(),
    clauses = sum(is_clause),
    dep_clauses = sum(is_dep_clause),
    coordination = sum(is_coord),
    complex_nominals = sum(is_complex_nominal),
    .groups = "drop"
  )

# Inspect the first 10 sentences
sentence_df %>%
  select(-sentence) %>%
  print(n = 10, width = Inf)

# Calculate sentence length and clause density by document
basic_syntax <- sentence_df %>%
  group_by(doc_id) %>%
  summarise(
    sentences = n(),
    total_words = sum(words),
    total_clauses = sum(clauses),
    MLS = mean(words),
    C_per_S = sum(clauses) / n(),
    .groups = "drop"
  )

# Display the results
print(basic_syntax, width = Inf)

# Calculate subordination measures by document
subordination <- sentence_df %>%
  group_by(doc_id) %>%
  summarise(
    sentences = n(),
    total_clauses = sum(clauses),
    total_dep_clauses = sum(dep_clauses),
    .groups = "drop"
  ) %>%
  mutate(
    DC_per_C = total_dep_clauses / total_clauses,
    DC_per_S = total_dep_clauses / sentences
  )

# Display the results
print(subordination, width = Inf)

# Calculate coordination measures by document
coordination_summary <- sentence_df %>%
  group_by(doc_id) %>%
  summarise(
    sentences = n(),
    total_clauses = sum(clauses),
    total_coordination = sum(coordination),
    .groups = "drop"
  ) %>%
  mutate(
    Coord_per_C = total_coordination / total_clauses,
    Coord_per_S = total_coordination / sentences
  )

# Display the results
print(coordination_summary, width = Inf)

# Calculate nominal complexity measures by document
nominal_summary <- sentence_df %>%
  group_by(doc_id) %>%
  summarise(
    sentences = n(),
    total_clauses = sum(clauses),
    total_complex_nominals = sum(complex_nominals),
    .groups = "drop"
  ) %>%
  mutate(
    CN_per_C = total_complex_nominals / total_clauses,
    CN_per_S = total_complex_nominals / sentences
  )

# Display the results
print(nominal_summary, width = Inf)

# Combine all eight syntactic measures
syntax_summary <- basic_syntax %>%
  select(doc_id, MLS, C_per_S) %>%
  left_join(
    subordination %>%
      select(doc_id, DC_per_C, DC_per_S),
    by = "doc_id"
  ) %>%
  left_join(
    coordination_summary %>%
      select(doc_id, Coord_per_C, Coord_per_S),
    by = "doc_id"
  ) %>%
  left_join(
    nominal_summary %>%
      select(doc_id, CN_per_C, CN_per_S),
    by = "doc_id"
  )

# Display rounded values
syntax_summary %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  print(width = Inf)

# Save the full-precision results
write_csv(syntax_summary, "syntax_summary.csv")

# Find candidate sentences with substantial subordination
examples_a <- sentence_df %>%
  filter(
    doc_id == "A06791.txt",
    words >= 25,
    words <= 65
  ) %>%
  arrange(desc(dep_clauses)) %>%
  slice_head(n = 3)

# Find candidate sentences with substantial nominal modification
examples_b <- sentence_df %>%
  filter(
    doc_id == "A93819.txt",
    words >= 25,
    words <= 65
  ) %>%
  arrange(desc(complex_nominals)) %>%
  slice_head(n = 3)

# Combine the candidates
example_candidates <- bind_rows(examples_a, examples_b)

# Print the complete sentences and their counts
for (i in seq_len(nrow(example_candidates))) {
  cat(
    "\nDocument:", example_candidates$doc_id[i],
    "| Paragraph:", example_candidates$paragraph_id[i],
    "| Sentence:", example_candidates$sentence_id[i],
    "\nWords:", example_candidates$words[i],
    "| Dependent relations:", example_candidates$dep_clauses[i],
    "| Coordination:", example_candidates$coordination[i],
    "| Nominal relations:", example_candidates$complex_nominals[i],
    "\n", example_candidates$sentence[i], "\n"
  )
}

# Select the two illustrative sentences
selected_examples <- sentence_df %>%
  filter(
    (doc_id == "A06791.txt" &
       paragraph_id == 96 &
       sentence_id == 380) |
      (doc_id == "A93819.txt" &
         paragraph_id == 2 &
         sentence_id == 2)
  )

# Verify that two sentences were selected
nrow(selected_examples)

# Save the sentences and their counts
write_csv(selected_examples, "syntax_examples.csv")

# Print the selected sentences for the report
for (i in seq_len(nrow(selected_examples))) {
  cat(
    "\nDocument:", selected_examples$doc_id[i],
    "| Paragraph:", selected_examples$paragraph_id[i],
    "| Sentence:", selected_examples$sentence_id[i],
    "\n", selected_examples$sentence[i], "\n"
  )
}