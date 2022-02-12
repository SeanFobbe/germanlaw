regex <- paste0("(§|§§|Art\\.|Artikel) *", # Section: Name
                "([0-9, \\.]+[a-z]?) *", # Section: Numbering
                "((Abs\\.?|Absatz|Absätze) *[0-9, ]+ *)?", # Absatz
                "((UAbs\\.?|Unterabsatz|Unterabsätze) *[0-9, ]+ *)?", # Unterabsatz
                "((S\\.?|Satz|Sätze) *[0-9, ]+ *)?", # Satz
                "((Nr\\.?|Nummern?) *[0-9, und]+ *)?", # Nummer
                "(([lL]it\\.?|litera) *[a-z, ]+ *)?", # Litera
                "(des)?(der)?", # Optional Genitiv
                " *", # Whitespace
                "[A-Z][a-zA-Z]*(G|B|-?VO|V|gesetz|gesetzes|-?[sS]atzung|-?[vV]erordnung)|gesetzbuch|-[oO]rdnung" # Name of Law
                )



testcases <- readLines("law_testcases.txt")

grep(regex, testcases, value =T)

grep(regex, testcases, invert =T, value =T)

regmatches(testcases, gregexpr(regex, testcases))



                "([0-9]\\. *Halbsatz *)?", # Halbsatz  # geht nicht?
