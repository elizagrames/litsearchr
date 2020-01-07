my_graph <- create_network(
  search_dfm = create_dfm(
    c(
      "Cross-scale occupancy dynamics of a postfire specialist
    in response to variation across a fire regime",
      "Variation in home-range size of Black-backed Woodpeckers",
      "Black-backed woodpecker occupancy in burned and beetle-killed forests"
    ),
    features = c("occupancy", "variation", "woodpecker", "burn"),
    closure = "none"
  ),
  min_studies = 1,
  min_occ = 1
)

make_importance(my_graph, imp_method = "strength")
