litsearchr
==========

The ``litsearchr`` package for R is designed to partially automate search term selection and writing search strategies for systematic reviews. It uses the Rapid Automatic Keyword Extraction algorithm (Rose et al. 2010) to identify potential keywords from a sample of titles and abstracts and combines them with author- and database-tagged keywords to create a pool of possible keywords relevant to a field of study. Important keywords in a field are identified from their importance in a keyword co-occurrence network. After keywords are grouped into concept groups, **litsearchr** writes Boolean searches in up to 53 languages, with stemming support for English. The searches are tested and work fully in at least 14 commonly used search databases with partial support in six additional databases.

Our intent in creating **litsearchr** is to make the process of designing a search strategy for systematic reviews easier for researchers in fields that lack standardized terms. By partially automating keyword selection, **litsearchr** reduces investigator bias in keyword selection and increases the repeatability of systematic reviews. It also reduces errors in creating database-specific searches by generating searches that work across a wide range of databases. Our hope is that **litsearchr** can be used to facilitate systematic reviews and contribute to automating evidence synthesis.

Comments, suggestions, bugs, and questions
------------------------------------------

**litsearchr** is a work in progress - any and all comments, suggestions, bugs, or questions are welcome! Please email eliza.grames@uconn.edu or open an issue at <https://github.com/elizagrames/litsearchr>.
