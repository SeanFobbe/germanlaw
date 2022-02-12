regex <- paste0("(§|§§|Art\\.|Artikel) *", # Section: Name
                "([0-9, \\.]+[a-z]?) *", # Section: Numbering
                "((Abs\\.?|Absatz) *[0-9, ]+ *)?", # Absatz
                "((UAbs\\.?|Unterabsatz) *[0-9, ]+ *)?", # Unterabsatz
                "((S\\.?|Satz) *[0-9, ]+ *)?", # Satz
                "((Nr\\.?|Nummer) *[0-9, und]+ *)?", # Nummer
                "(([lL]it\\.?|litera) *[a-z, ]+ *)?", # Litera
                "(des)?(der)?", # Optional Genitiv
                " *", # Whitespace
                "[A-Z][a-zA-Z]*(G|B|-?VO|V|gesetz|gesetzes|-?[sS]atzung|-?[vV]erordnung)|gesetzbuch" # Name of Law
                )



testcases <- readLines("law_testcases.txt")

grep(regex, testcases, value =T)

grep(regex, testcases, invert =T, value =T)

regmatches(testcases, gregexpr(regex, testcases))



                "([0-9]\\. *Halbsatz *)?", # Halbsatz  # geht nicht?
