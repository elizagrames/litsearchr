## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
BIOSIS <- import_naive("BBWO-BIOSIS-062118.csv", database = "BIOSIS")
ZooRec <- import_naive("BBWO-ZooRec-062118.csv", database = "ZooRec")
Scopus <- import_naive("BBWO-Scopus-062118.csv", database = "Scopus")

BBWO_data <- rbind(BIOSIS, ZooRec, Scopus) 


## ------------------------------------------------------------------------
BBWO_data <- clean_dataset(BBWO_data)

## ------------------------------------------------------------------------
unique_keywords <- get_unique(BBWO_data, singular=TRUE)

## ------------------------------------------------------------------------
BBWO_data <- create_subjects(BBWO_data, keys=TRUE, titles=TRUE, abstracts=TRUE, models=FALSE)

BBWO_data <- get_descriptors(BBWO_data, unique_keywords, stemming=FALSE, exact=FALSE)

## ------------------------------------------------------------------------
occurrence_dataset <- make_dataset(BBWO_data, keys=unique_keywords, onlykeywords = FALSE)

trimmed_dataset <- trim_dataset(occurrence_dataset, studies=2)

BBWO_graph <- make_graph(trimmed_dataset, printplot=TRUE)

## ------------------------------------------------------------------------
plot(density(igraph::strength(BBWO_graph)), main="Density of node strengths")
hist(igraph::strength(BBWO_graph), 100, main="Histogram of node strengths", xlab="Node strength")
plot(sort(igraph::strength(BBWO_graph)), ylab="Node strength", main="Ranked node strengths", xlab="Rank")

## ------------------------------------------------------------------------
cutoffs_spline <- find_cutoff(BBWO_graph, method = "spline", degrees = 2, knot_num = 3, diagnostics = TRUE)

cutoffs_cumulative <- find_cutoff(BBWO_graph, method = "cumulative", cum_pct = .8, diagnostics = TRUE)

## ------------------------------------------------------------------------
reduced_graph <- reduce_graph(BBWO_graph, cutoff_strength = cutoffs_spline[1])

search_terms <- get_keywords(reduced_graph, savekeywords = TRUE, makewordle = TRUE)


## ------------------------------------------------------------------------
search_terms
process_group <- c("behaviour",
                   "breeding",
                   "breeding habitat",
                   "clutch size",
                   "diet",
                   "dispersal",
                   "establishment",
                   "fecundity",
                   "food",
                   "hatching",
                   "migration",
                   "mortality",
                   "nest failure",
                   "nest predation",
                   "nest predator",
                   "nest success",
                   "nest survival",
                   "nesting",
                   "nesting habitat",
                   "predation",
                   "predator",           
                   "prey",
                   "recruitment",
                   "reproduction",
                   "reproductive behaviour",
                   "reproductive rate",
                   "reproductive success",
                   "survival",
                   "survival rate",
                   "birth",
                   "death",
                   "mortality", 
                   "immigration", 
                   "emigration", 
                   "mechanism", 
                   "cause")

fire_group <- c("anthropogenic disturbance",
                "burn",
                "burned forest",
                "disturbance",
                "disturbance regime",
                "fire",
                "fire regime",
                "fire suppression",
                "forest fire",
                "prescribed fire",
                "snag",
                "wildfire")

bird_group <- c("bird",
                "breeding bird",
                "woodpecker",
                "sapsucker")

occupancy_group <- c("abundance",
                     "habitat suitability",
                     "habitat use",
                     "occurrence",
                     "population decline",
                     "population dynamic",
                     "species abundance",
                     "species composition",
                     "species distribution",
                     "species habitat",
                     "occupancy")

my_search_terms <- list(process_group, fire_group, bird_group, occupancy_group)


