## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
list.files("../inst/extdata/")


## ------------------------------------------------------------------------
BBWO_data <- import_naive("../inst/extdata/", remove_duplicates = TRUE, clean_dataset = TRUE)


## ------------------------------------------------------------------------
raked_keywords <- extract_terms(BBWO_data, new_stopwords = NULL, min_freq = 2, title = TRUE, abstract = TRUE)

# When we check the results, some of these are definitely not important keywords
# For example, "cocaine exposure cause parrot foot necrosis" doesn't seem particularly relevant to BBWO
# But, not to worry, these sorts of keywords will disappear once we check their frequency in the entire dataset later
head(raked_keywords, 20)

## ------------------------------------------------------------------------
real_keywords <- select_actual_terms(BBWO_data)

# At first glance, these keywords seem more relevant
# But something tagged with "biology" really isn't any better than "cocaine exposure cause parrot foot necrosis"
# This is why a combined approach is best to really capture the full range of possible keywords
head(real_keywords, 20)


## ------------------------------------------------------------------------
BBWO_dict <- make_dictionary(actual_terms = real_keywords, likely_terms = raked_keywords)
BBWO_corpus <- make_corpus(BBWO_data)

BBWO_dfm <- create_dfm(BBWO_corpus, my_dic=BBWO_dict)

# If you want to see the most frequently occurring terms, call the topfeatures() function from the quanteda package. 
quanteda::topfeatures(BBWO_dfm, 20)


## ------------------------------------------------------------------------
BBWO_graph <- create_network(BBWO_dfm, min_studies=3, min_occurrences = 3)
# plot_full_network(BBWO_graph)

## ------------------------------------------------------------------------
plot(density(igraph::strength(BBWO_graph)), main="Density of node strengths")
hist(igraph::strength(BBWO_graph), 100, main="Histogram of node strengths", xlab="Node strength")
plot(sort(igraph::strength(BBWO_graph)), ylab="Node strength", main="Ranked node strengths", xlab="Rank")

## ------------------------------------------------------------------------
cutoffs_spline <- find_cutoff(BBWO_graph, method = "spline", degrees = 2, knot_num = 4, diagnostics = TRUE)

cutoffs_cumulative <- find_cutoff(BBWO_graph, method = "cumulative", cum_pct = .8, diagnostics = TRUE)

## ------------------------------------------------------------------------
reduced_graph <- reduce_graph(BBWO_graph, cutoff_strength = cutoffs_spline[1], printplot = FALSE)

search_terms <- get_keywords(reduced_graph, savekeywords = TRUE, makewordle = FALSE)

# If we check the length of search_terms, it is a pretty long list to consider manually (930)
length(search_terms)

# How relevant are these terms? Not very
head(sort(search_terms), 20)

# We have two options about how to procede here: use a more stringent cutoff for node strength (for example cutoffs_spline[2]), or split out network into n-grams which is the next example


