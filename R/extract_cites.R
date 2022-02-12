regex <- paste0("(§|§§|Art\\.|Artikel) *", # Section: Name
                "([0-9, ]+[a-z]?) *", # Section: Numbering
                "((Abs\\.?|Absatz) *[0-9, ]+ *)?", # Absatz
                "((UAbs\\.?|Unterabsatz) *[0-9, ]+ *)?", # Unterabsatz
                "((S\\.?|Satz) *[0-9, ]+ *)?", # Satz
                "((Nr\\.?|Nummer) *[0-9, ]+ *)?", # Nummer
                "(([lL]it\\.?|litera) *[a-z, ]+ *)?", # Litera
                "(des)?", # Optional Genitiv
                " *", # Whitespace
                "[A-Z][a-zA-Z-]*(G|VO|V|gesetz|gesetzes|VN-Charta)" # Name of Law
                )



testcases <- readLines("law_testcases.txt")

grep(regex, testcases, value =T)

grep(regex, testcases, invert =T, value =T)

regmatches(testcases, gregexpr(regex, testcases))

"([0-9]\\. Halbsatz *)?", # Halbsatz

