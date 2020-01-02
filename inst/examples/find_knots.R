my_graph <- create_network(
  search_dfm = create_dfm(
    c(
      "Cross-scale occupancy dynamics of a postfire specialist
    in response to variation across a fire regime",
      "Variation in home-range size of Black-backed Woodpeckers"
    ),
    features = c("occupancy", "variation", "woodpecker"),
    closure = "none"
  ),
  min_studies = 1,
  min_occurrences = 1
)


find_knots(
  importance_data = make_importance(graph=my_graph, importance_method = "strength"),
  degrees = 2,
  knot_num = 1
)
