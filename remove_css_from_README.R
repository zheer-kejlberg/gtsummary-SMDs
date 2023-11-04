tx  <- readLines("README.md")
tx <- paste(tx, collapse="\n")
tx  <- gsub(pattern = "<style>(\r\n|\r|\n|.)*?<[/]style>", replacement = "", x = tx)
writeLines(tx, con="README.md")