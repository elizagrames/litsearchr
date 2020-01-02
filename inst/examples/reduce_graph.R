my_graph <- create_network(
  search_dfm = create_dfm(
    c(
      "Cross-scale occupancy dynamics of a postfire specialist
    in response to variation across a fire regime",
      "Variation in home-range size of Black-backed Woodpeckers",
      "Black-backed woodpecker occupancy in burned and beetle-killed forests"
    ),
    features = c("occupancy", "variation", "woodpecker"),
    closure = "none"
  ),
  min_studies = 1,
  min_occurrences = 1
)
create_network(
  search_dfm = create_dfm(
    c(
      "Cross-scale occupancy dynamics of a postfire specialist
    in response to variation across a fire regime",
      "Variation in home-range size of Black-backed Woodpeckers",
      "Black-backed woodpecker occupancy in burned and beetle-killed forests"
    ),
    features = c("occupancy", "variation", "woodpecker"),
    closure = "none"
  ),
  min_studies = 1,
  min_occurrences = 1
)

reduce_graph(my_graph, cutoff_strength = 1)
