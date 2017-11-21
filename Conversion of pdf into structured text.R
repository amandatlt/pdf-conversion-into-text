library(pdftools)

#####################
### import in pdf ###
#####################

pdf_file <- file.path(getwd(), "my pdf file.pdf")
doc <- pdf_text(pdf_file)

#########################################
### convert two column pages into one ###
#########################################

# function to clean trailing and leading spaces
trim = function (x) gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x,  perl=TRUE)



# split the lines of each page into its own element within the page vector
doc <- sapply(doc, function(x) unlist(strsplit(x, split = "\r\n")))



#function to remove column formatting
unformat_columns <- function(x) {
  
  # x should be a vector containing a single page of information
  # each row of x should contain one line of the page
  
  # here you want to find the number of spaces we should start slicing the column at
  # since the number of spaces before the column begin will differ across each page
  start_spaces <- gregexpr("^\\s+", x)
  
  if (length(x) == 0) return("")
  
  start_length <- table(sapply(start_spaces, function(x) attr(x, "match.length")))
  start_nchar <- names(sort(start_length, decreasing = TRUE)[1])
  start_nchar <- as.integer(start_nchar)
  
  if(start_nchar == -1) start_nchar = 0
  
  start_nchar = start_nchar + 1
  
  #start slicing the first column - 64 is a hardcoded number
  p1 <- unlist(lapply(x, function(x) substr(x,start_nchar,start_nchar + 64)))
  p1 <- trim(p1)
  p1 <- p1[-which(p1 %in% "")]
  p1 <- paste(p1, collapse = " ")
  p1_final <- gsub("- ","", p1)
  
  #slice the second column
  p2 <- unlist(lapply(x, function(x) substring(x,start_nchar + 64) ))
  p2 <- trim(p2)
  p2 <- p2[-which(p2 %in% "")]
  p2 <- paste(p2, collapse = " ")
  p2_final <- gsub("- ", "", p2)
  
  #piece the two columns together
  full <- paste(p1_final, p2_final, sep = "<column break> ")
  
  return(full)
}



#function for easier reading of contents within a page
read_entirety <- function(x) {
  print(substr(x, 1, 500))
  print(substr(x, 501, 1000))
  print(substr(x, 1001, 1500))
  print(substr(x, 1501, 2000))
  print(substr(x, 2001, 2500))
  print(substr(x, 2501, 3000))
}



#convert all two-column pages into 1!
doc_singlecol <- lapply(doc, unformat_columns)



#check some pages (make sure information copied over correctly)
read_entirety(doc_singlecol[[65]])
read_entirety(doc_singlecol[[7]])
read_entirety(doc_singlecol[[9]])
read_entirety(doc_singlecol[[92]])
