litsearchr::create_network(
  search_dfm = create_dfm(
    BBWO_data$abstract[1:10],
    features = extract_terms(
      text = BBWO_data$abstract[1:10],
      method = "fakerake",
      min_freq = 2,
      ngrams = TRUE,
      n = 2
    )
  )
)
