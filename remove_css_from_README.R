library(stringr)
tx  <- readLines("README.md")
tx2 <- paste(tx, collapse="\n")

str_count(tx2, "<style>(\r\n|\r|\n|.)*</style>")


tx3  <- gsub(pattern = "<style>(\r\n|\r|\n|.)*</style>", replacement = " ", x = tx2)



writeLines(tx3, con="README2.md")

