regex <- paste0("(§§?|Art\\.|Artikel)", # Section: Name
                " *",  # Whitespace
                "([0-9, ]+[a-z]?)", # Section: Numbering
                " *",  # Whitespace
                "((Abs\\.?|Absatz) *[0-9, ]+ *)?", # Absatz
                "((UAbs\\.?|Unterabsatz) *[0-9, ]+ *)?", # Unterabsatz
                "((S\\.?|Satz) *[0-9, ]+ *)?", # Satz
                "((Nr\\.?|Nummer) *[0-9, ]+ *)?", # Nummer
                "(([lL]it\\.?|litera) *[a-z, ]+ *)?", # Litera
                "d?e?s?", # Optional Genitiv
                " *", # Whitespace
                "[A-Z][a-zA-Z-]*(G|VO|V|gesetz|gesetzes|GB)" # Name of Law
                )



testcases <- readLines("law_testcases.txt")

grep(regex, testcases, value =T)

grep(regex, testcases, invert =T, value =T)

regmatches(testcases, gregexpr(regex, testcases))
