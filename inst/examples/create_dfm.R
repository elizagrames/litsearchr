head(as.matrix(
  create_dfm(
    BBWO_data$text[1:10],
    type = "keywords",
    keywords = extract_terms(
      text = BBWO_data$text[1:10],
      method = "fakerake",
      min_freq = 2,
      ngrams = TRUE,
      n = 2
    )
  )
))
