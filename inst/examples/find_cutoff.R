dfm <- create_dfm(
  elements = c(
    "Cross-scale occupancy dynamics of a postfire specialist
    in response to variation across a fire regime",
    "Variation in home-range size of Black-backed Woodpeckers",
    "Black-backed woodpecker occupancy in burned and beetle-killed forests"
  ),
  features = c("occupancy", "variation", "black-backed woodpecker", "burn")
)

my_network <- create_network(search_dfm = as.matrix(dfm),
                             min_studies = 1,
                             min_occ = 1)

find_cutoff(my_network,
            method = "cumulative",
            percent = .9)
