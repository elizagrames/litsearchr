terms <- c("Picoides arcticus [Breeding season /  / ] [Nests /  / ]",
"[Forest and and woodland / Coniferous forest / ]",
"[Fire / Extreme wildfire / ] and [California / Sierra Nevada Mountains / ].")

clean_keywords(terms)

extract_terms(keywords=terms, method="tagged", min_freq = 1, ngrams = FALSE)
