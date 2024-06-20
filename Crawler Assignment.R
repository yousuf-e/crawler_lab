library(RCurl)
library(XML)
library(stringr)
library(stringi)
library(xml2)
library(httr)


# Helper function for parsing HTML
htmlToText <- function(input, ...) {
  ###---PACKAGES ---###
  require(RCurl)
  require(XML)
  
  
  
  ###--- LOCAL FUNCTIONS ---###
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {
    # if input is a .html file
    if (file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if (grepl("</html>", input, fixed = TRUE))
      return(input)
    
    # if input is a URL, probably should use a regex here instead?
    if (!grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if (!file.exists("cacert.perm"))
        download.file(url = "http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
  
  # convert HTML to plain text
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE)
    # text <- xml_find_all(
    #   doc,
    #   "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]",
    #   xmlValue
    # )
    text_nodes <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    text <- paste(text_nodes, collapse = " ")
    return(text)
  }
  
  # format text vector into one character string
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  ###--- MAIN ---###
  # lapply(input, print)
  
  # STEP 1: Evaluate input
  html.list <- lapply(input, evaluate_input)
  
  
  # STEP 2: Extract text from HTML
  text.list <- lapply(html.list, convert_html_to_text)
  
  # STEP 3: Return text
  text.vector <- sapply(text.list, collapse_text)
  return(text.vector)
}

################################################################################
#                                                                              #
#  Todo: 1. modify the repo to be directory, store the files in there?         #
#        2. add error handling and clean up the code a bit                     #
#                                                                              #
#                                                                              #
################################################################################

frontier <- c("https://www.nytimes.com/",
              "http://www.kdnuggets.com",
              "http://news.google.com",
              "https:reddit.com"
              )

topicwords <- c("technology",
                "web",
                "mining",
                "news"
                )

num <- 50 #total number of items to crawl
result <- c() # Page titles of the crawled items
j <- 0  #number of items in the repository
repo <- c("") # Repository of crawled pages

# Check if running in RStudio
if (rstudioapi::isAvailable()) {
  # Get the path of the currently active document (script)
  script_path <- rstudioapi::getActiveDocumentContext()$path
  
  # Get the directory of the script
  script_dir <- dirname(script_path)
  
  # Set the working directory to the script's directory
  setwd(script_dir)
  
  # Print the current working directory to confirm
  print(getwd())
  dir.create(file.path(script_dir, "repo"))
  
} else {
  message("rstudioapi is not available.")
}




while (j < num) {
  j <- j + 1
  
  # if our queue is empty, we are done
  if (length(frontier) < 1) {
    break
  }
  
  
  
  # grab the first item in the frontier and place in the "exploredlink" variable
  exploredlink <- frontier[1]
  frontier <- frontier[-1]
  print(exploredlink)
  
  
  
  # if the file is a jpg, ignore
  if (str_detect(exploredlink, "\\.jpg$"))
  {
    next
  }
  
  
  
  # 0. grab the html document and turn it into a string.
  doc <- tryCatch(
    getURL(exploredlink),
    error = function(cond) {
      return("")
    }
  )
  
  doc <- stri_enc_toutf8(doc)
  doc <- stri_replace_all_regex(
    doc,
    pattern = "[^\x09\x0A\x0D\x20-\x7E\xC2-\xF4][\x80-\xBF]*",
    replacement = "",
    vectorize_all = FALSE
  )
  
  
  
  # error check 1: only continue if the doc is more than 10 chars
  if (str_length(doc) < 10) {
    next
  }
  
  
  
  # error check 2: only continue if the website is a .com
  domain <- str_extract(exploredlink, pattern = ".*\\.com")
  if (is.na(domain)) {
    next
  }
  
  
  
  # 2. parse the doc to html, grab all the links on the page, add to the frontier
  doc <- htmlParse(doc)
  anchor <- getNodeSet(doc, "//a")
  anchor <- sapply(anchor, function(x) 
    xmlGetAttr(x, "href")
  )
  
  
  
  # error check 3. only add absolute urls by checking for ^http
  if (length(anchor) > 0) {
    temp <- c()
    for (i in 1:length(anchor)) {
      if (is.null(anchor[[i]])) {
        next
      }
      if (!str_detect(anchor[[i]][1], "^http")) {
        next
      }
      if (str_detect(anchor[[i]][1], domain)) {
        next
      }
      temp <- append(temp, str_trim(anchor[[i]][1]))
    }
    anchor <- temp
    rm(temp)
    
    frontier <- append(frontier, anchor)
    frontier <- unique(frontier)
    
  } else {
    print("no links found")
  }

  
  
  # 3. grab the text
  bodyText <- htmlToText(
    content(
      GET(exploredlink),
      type = "text/html",
      encoding = "UTF-8",
      as ="text"))

  
  
  # 3.1 split the text into a list for further analysis. 
  bodyText <- str_split(
    tolower(
      str_replace_all(
        (str_replace_all(
          bodyText,
          "(\\t|\\n|\\r)",
          " ")
         ),
        "\\s{2,}",
        " ")),
    " ")[[1]]
  
  if (length(bodyText) < 10) {
    next
  }
  
  
  
  # 3.2 add to repo if the page has any of the listed keywords
  if (any(topicwords %in% bodyText)) {
    print("Found a topic word!")
    # repo <- append(repo, bodyText[1:50])
    exploredlink <- gsub("/", "_", exploredlink)
    file_path <- file.path(script_dir, "repo", paste0(exploredlink, ".txt"))
    writeLines(bodyText, file_path)
    
  } else {
    print("no topic words found >:(  ")
  }
  
  
  
  
  # 4. update the vec w <Page Title><URL>
  title <- getNodeSet(doc, "//title")[1]
  title <- sapply(title, function(x) 
    xmlValue(x)
  )
  updated_entry <- paste0("<", title, "><", exploredlink, ">")
  result <- c(result, updated_entry)
  
}

print("done")
print(repo)
print(result)

