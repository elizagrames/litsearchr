my_df <- data.frame(title=c("Picoides", "picoides", "Seiurus"), id=c("01", "02", "03"))
remove_duplicates(my_df, "title", "exact")
