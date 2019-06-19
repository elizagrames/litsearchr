write.csv(litsearchr::scopus_example,
          paste(tempdir(), "/scopus.csv", sep = ""))
naive_results <- import_results(paste(tempdir(), "/", sep = ""))
