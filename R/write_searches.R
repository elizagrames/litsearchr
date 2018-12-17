#' Select non-English languages to search in
#' @description This function chooses the best non-English languages to conduct searches in based on the topic of the review. The topics query a database of non-English language journals compiled from Ulrich; currently only STEM fields are supported.
#' @param key_topics a character vector of topics related to the topic of the search
#' @return a data frame of languages used by journals tagged with the key topics and a count of how many journals use that language.
get_language_data <- function(key_topics){
  subset_these <- c()

  for (i in 1:length(key_topics)){
    subset_these <- append(subset_these, which(stringr::str_detect(litsearchr::ulrich$SubjectCodes, key_topics[i])==TRUE))
  }

  my_subject <- litsearchr::ulrich[subset_these,]
  langs <- my_subject$Language

  combined_langs <- c()
  for (i in 1:length(langs)){
    combined_langs <- paste(combined_langs, langs[i])
  }

  split_langs <- strsplit(combined_langs, "\ | ")
  all_langs <- split_langs[[1]]

  remove_these <- c(which(all_langs=="null"),
                    which(all_langs=="Yes"),
                    which(all_langs=="English"),
                    which(all_langs=="Multiple"),
                    which(all_langs=="languages"),
                    which(all_langs==""),
                    which(all_langs==","),
                    which(all_langs=="in"),
                    which(all_langs=="|" ))

  good_langs <- all_langs[-remove_these]

  label <- key_topics[1]
  if (length(key_topics) > 1){
    for (i in 2:length(key_topics)){
      label <- paste(label, "or", key_topics[i])
    }}

  lang_table <- sort(table(good_langs), decreasing = TRUE)

  lang_data <- as.data.frame(matrix(c(""), nrow=length(lang_table), ncol=2))
  colnames(lang_data) <- c("language", "count")
  lang_data$language <- names(lang_table)
  lang_data$count <- as.numeric(lang_table)

  return(lang_data)
}

#' Select search languages
#' @description Checks which languages returned from get_language_data() are possible to use and returns a character vector of languages to use. Called from inside write_search().
#' @param lang_data a table of language data exported from get_language_data()
choose_languages <- function(lang_data=get_language_data(key_topics = "biology")){

  language_output <- lang_data[,1:2]
  language_output <- language_output[which((language_output$language %in% litsearchr::possible_langs$Language)==TRUE),1]

  return(language_output)

}

#' Generates a graph of languages to search
#' @description Creates a bubble graph showing the number of journals in each language from the languages used by journals in given topics.
#' @param lang_data a table of language data returned from choose_languages()
#' @param no_return the maximum number of languages to include in the graph
#' @param key_topics a character vector of the same key topics used in choose_languages()
#' @return a bubble plot of non-English languages used by journals in a discipline sized by count
language_graphs <- function(lang_data=get_language_data(key_topics=c("biology", "conservation", "ecology")), no_return=15, key_topics=c("biology", "conservation", "ecology")){
  lang_data$x <- as.numeric(row.names(lang_data))^.1
  lang_data$y <- sample(1:100, length(lang_data$language))*(as.numeric((lang_data$count)))*as.numeric(rownames(lang_data))

  label <- key_topics[1]
  if (length(key_topics) > 1){
    for (i in 2:length(key_topics)){
      label <- paste(label, "or", key_topics[i])
    }}

  if (length(lang_data$x) <= no_return){
    symbols(lang_data$x, lang_data$y,
            circles=lang_data$count,
            bg=rainbow(length(lang_data$x)),
            axes=FALSE, xlab="", ylab="",
            main=paste("Non-English languages for", label, "journals", sep=" "))
    text(lang_data$x, lang_data$y, lang_data$language, cex=.75)
    legend("topright",
           legend=c(paste(lang_data$language, lang_data$count, sep=", ")),
           pch=20,
           col=rainbow(length(lang_data$x)))
  }

  if (length(lang_data$x) > no_return){
    symbols(lang_data$x, lang_data$y,
            circles=lang_data$count,
            bg=rainbow(length(lang_data$x[1:no_return])),
            axes=FALSE, xlab="", ylab="",
            main=paste("Non-English languages for", label, "journals", sep=" "))
    text(lang_data$x, lang_data$y, lang_data$language, cex=.75)
    legend("topright",
           legend=c(paste(head(lang_data$language, no_return),
                          head(lang_data$count, no_return), sep=", ")),
           pch=20,
           col=head(rainbow(length(lang_data$x[1:no_return])),no_return))

  }

}

#' Translate search terms
#' @param search_terms a character vector of search terms
#' @param target_language a character vector of the language(s) to translate the search to
#' @param source_language a character vector of the language the search terms are currently in
#' @param API_key an API key for Google Translate (not available through litsearchr)
#' @description Takes groups of search terms and translates them into target language using the Google Translate API. This function is intended for use inside write_search(), not as a standalone function.
translate_search <- function(search_terms, target_language, source_language="en", API_key=API_key){
  words <- search_terms

  termlist <- words

  this_one <- which(stringr::str_detect(litsearchr::possible_langs$Language, target_language)==TRUE)
  trans_lang <- as.character(litsearchr::possible_langs$Short[this_one])
  trans_encod <- as.character(litsearchr::possible_langs$Encoding[this_one])

  for (i in 1:length(words)){
    termlist[i] <- translate::translate(words[i], source=source_language, target=trans_lang, key = API_key)[[1]]
  }

  return(termlist)
}

#' Check whether a word is long enough to stem
#' @param word the word to check
#' @description Checks if the stemmed form of a word is longer than 3 characters. Not intended as a standalone function and is called from write_search().
should_stem <- function(word){
  wordcut <- SnowballC::wordStem(word, language="en")
  splitup <- strsplit(wordcut, " ")[[1]]
  stem_length <- nchar(splitup[length(splitup)])
  new_word <- word
  if (stem_length > 3){new_word <- SnowballC::wordStem(word, language="en")}
  return(new_word)
}

#' Write Boolean searches
#' @description Takes search terms grouped by concept group and writes Boolean searches in which terms within concept groups are separated by "OR" and concept groups are separated by "AND". Searches can be written in up to 53 languages, though the function defaults to only searching the top ten most used languages in a discipline using the choose_languages() function. The default for language options relies on searching a database of journals by discipline based on Ulrich's Periodicals Directory. Only scientific fields are included in this database. All supported languages can be seen with available_languages().
#' @param groupdata a list of character vectors, each of which is a concept group
#' @param API_key your Google Translate API key
#' @param languages a character vector of supported languages to write searches in.
#' @param exactphrase if set to \code{TRUE}, stemmed search terms with multiple words will be enclosed in quotes
#' @param directory the path to the directory where you want to save searches (defaults to current working directory)
#' @param stemming if TRUE, writes stemmed search (only when the current language is English)
#' @param verbose if TRUE, prints when each language is finished writing
#' @param writesearch if TRUE, saves the searches to .txt files in the specified directory
#' @return a list of search strings
write_search <- function(groupdata, API_key=NULL, languages=NULL, exactphrase=FALSE, directory="./", stemming=TRUE, verbose=TRUE, writesearch=FALSE){
  if(writesearch==TRUE){
    if(utils::menu(c("yes", "no"),
          title="This is going to write .txt files to your computer containing the search strings. Are you sure you want to write the files?")==2){
    writesearch <- FALSE
  }}

  no_groups <- length(groupdata)
  group_holder <- c()
  no_langs <- length(languages)

  if(exactphrase==FALSE){

    for (i in 1:no_langs){
      current_lang <- languages[i]
      translated_groups <- list()
      length(translated_groups) <- no_groups

      for (j in 1:no_groups){
        current_group <- groupdata[j]

        if (current_lang!="English"){
          translated_terms <- (litsearchr::translate_search(
            search_terms = current_group[[1]], target_language = current_lang, API_key = API_key))
          each_line <- paste("\\(", "\\(", translated_terms[1], "\\)")
        }

        if (current_lang=="English"){
          if(stemming==FALSE){
            translated_terms <- current_group[[1]]
          }

          if(stemming==TRUE){
            stemyes <- "stemmed-"
            for (m in 1:length(current_group[[1]])){
              prestar[m] <- litsearchr::should_stem(current_group[[1]][m])
              if(m==length(current_group[[1]])){prestar <- unique(prestar)}
            }

            for(n in 1:length(prestar)){
              if(n==1){redundant <- c()}
              if(stringr::str_detect(paste(prestar[-n], collapse=" "), prestar[n])){
                detections <- which(stringr::str_detect(prestar, prestar[n])==TRUE)
                redundant <- append(redundant, detections[-which(detections==n)])
              }
              if(n==length(prestar)){
                redundant <- unique(redundant)
                prestar <- prestar[-redundant]
              }
            }

            translated_terms <- unique(paste(prestar, "*", sep=""))



          }
          each_line <- paste("\\(", "\\(", translated_terms[1],  "*", "\\)")


        }

        for (k in 2:length(translated_terms)){
          each_line <- paste(each_line, " OR \\(", translated_terms[k],"\\)", "*",  sep="")
        }

        each_line <- paste(each_line, "\\)")

        translated_groups[[j]] <- each_line
      }

      total_search <- translated_groups[[1]]
      for (l in 2:length(translated_groups)){
        total_search <- paste(total_search, translated_groups[[l]], sep=" AND ")
      }
      total_search <- paste("\\(", total_search, "\\)")

      this_one <- which(stringr::str_detect(litsearchr::possible_langs$Language, current_lang)==TRUE)
      trans_encod <- as.character(litsearchr::possible_langs$Encoding[this_one])

      converted_search <- iconv(total_search, "UTF-8", trans_encod)
      converted_search <- gsub("\\\\", "\\", converted_search)

      if(writesearch==TRUE){
        filename <- paste(directory, "search-in-", current_lang, ".txt", sep="")
        writeLines(converted_search, filename)
        if(verbose==TRUE){
          print(paste(current_lang, "is written"))
        }
      }

      if(i==1){
        search_list <- list()
        length(search_list) <- length(i)
      }
      search_list[[i]] <- converted_search
      names(search_list)[[i]] <- current_lang

    }


  }

  if(exactphrase==TRUE){
    for (i in 1:no_langs){
      current_lang <- languages[i]
      translated_groups <- list()
      length(translated_groups) <- no_groups

      for (j in 1:no_groups){
        current_group <- groupdata[j]

        if (current_lang!="English"){
          translated_terms <- (translate_search(
            search_terms = current_group[[1]], target_language = current_lang))
          each_line <- paste("\\(", "\"", translated_terms[1], "\"", sep="")
        }

        if (current_lang=="English"){
          if(stemming==FALSE){
            translated_terms <- current_group[[1]]
          }
          if(stemming==TRUE){
            stemyes <- "stemmed-"

            for (m in 1:length(current_group[[1]])){
              prestar[m] <- litsearchr::should_stem(current_group[[1]][m])
              if(m==length(current_group[[1]])){prestar <- unique(prestar)}
            }

            for(n in 1:length(prestar)){
              if(n==1){redundant <- c()}
              if(stringr::str_detect(paste(prestar[-n], collapse=" "), prestar[n])){
                detections <- which(stringr::str_detect(prestar, prestar[n])==TRUE)
                redundant <- append(redundant, detections[-which(detections==n)])
              }
              if(n==length(prestar)){
                redundant <- unique(redundant)
                prestar <- prestar[-redundant]
              }
            }

            translated_terms <- unique(paste(prestar, "*", sep=""))

          }
          each_line <- paste("\\(", "\"", translated_terms[1], "\"", sep="")
        }

        for (k in 2:length(translated_terms)){
          each_line <- paste(each_line, " OR ", "\"", translated_terms[k], "\"", sep="")
        }

        each_line <- paste(each_line, "\\)")

        translated_groups[[j]] <- each_line
      }

      total_search <- translated_groups[[1]]
      for (l in 2:length(translated_groups)){
        total_search <- paste(total_search, translated_groups[[l]], sep=" AND ")
      }
      total_search <- paste("\\(", total_search, "\\)")

      this_one <- which(stringr::str_detect(litsearchr::possible_langs$Language, current_lang)==TRUE)
      trans_encod <- as.character(litsearchr::possible_langs$Encoding[this_one])

      converted_search <- iconv(total_search, "UTF-8", trans_encod)
      converted_search <- gsub("\\\\", "\\", converted_search)

      if(stemming==FALSE){filename <- paste("search-in-", current_lang, ".txt", sep="")}
      if(stemming==TRUE){
        if(current_lang!="English"){filename <- paste("search-in-", current_lang, ".txt", sep="")}
        if(current_lang=="English"){filename <- paste("search-in-stemmed-", current_lang, ".txt", sep="")}
      }

      if(writesearch==TRUE){
        writeLines(converted_search, filename)
        if(verbose==TRUE){
          print(paste(current_lang, "is written"))
          }
      }

      if(i==1){
        search_list <- list()
        length(search_list) <- length(i)
      }
      search_list[[i]] <- converted_search
      names(search_list)[[i]] <- current_lang


    }

  }

  return(search_list)
  }





#' Print possible search languages
#' @description Prints a list of languages that write_search() can write searches in.
available_languages <- function(){
  print(litsearchr::possible_langs$Language)
}


#' Write a search to check title recall
#' @description Given a set of titles, writes a Boolean search that can be used in database title fields to check whether a search strategy retrieves titles.
#' @param titles a character vector of titles
#' @return a text string
write_title_search <- function(titles){
  titlekeys <- quanteda::tokens_remove(
    quanteda::tokens(tolower(titles),
                     remove_numbers=TRUE, remove_hyphens=FALSE, remove_punct=TRUE), litsearchr::custom_stopwords)

  title <- c()
  for (i in 1:length(titlekeys)){
    temp <- paste(titlekeys[[i]], collapse=" ")
    title[i] <- paste("\\(", temp, "\\)")
  }

  title_search <- paste(title, collapse=" OR ")
  title_search <- gsub("\\\\", "", title_search)

  return(title_search)
}
