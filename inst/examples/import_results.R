file_location <- system.file(
  "extdata",
  "avian_ecology_bibliography.ris",
  package = "revtools")
naive_results <- import_results(file=file_location)
