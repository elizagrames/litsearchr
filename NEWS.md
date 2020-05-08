# litsearchr 0.1.0

Initial version of litsearchr. 

# litsearchr 0.2.0

Changes made in the new version of litsearchr:
 -- reduced package dependencies to reduce recursive dependencies
 -- eliminated dependence on rJava with an alternative keyword extraction algorithm
 -- made the search writing function more efficient
 -- moved import and deduplicate functions to wrapping synthesisr
 -- incorporated comments and ideas from the Evidence Synthesis Hackathon

# litsearchr 0.2.1

This version includes an updated check_recall function that is more flexible:
-- uses reciprocal similarity to deal with short titles that match a subset of words in longer titles
-- makes removing stopwords optional for disciplines where stopwords are meaningful
-- allows non-English stopwords to be removed from titles

# litsearchr 0.3.0

This version of litsearchr reflects changes to its sister package synthesisr, which now handles most of the core text processing functions in litsearchr in addition to file import.

# litsearchr 0.4.0 

The main change in this version is that all core text processing has been moved back from synthesisr into litsearchr so that metaverse packages are more modular. litsearchr now only calls synthesisr for file import and deduplication.

Minor changes:
-- fixed the problem of NA string similarity in check_recall
-- hid some functions that are only called internally by other functions
