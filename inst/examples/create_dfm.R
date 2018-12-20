create_dfm(make_corpus(BBWO_data),
           make_dictionary(extract_terms(
             df=BBWO_data, type="RAKE", min_freq=3)))
