#Libraries
{
  if(!require(parallel)){install.packages("parallel", dependencies = TRUE);library(parallel)}
  if(!require(igraph)){install.packages("igraph", dependencies = TRUE);library(igraph)}
  if(!require(readr)){install.packages("readr", dependencies = TRUE);library(readr)}
  if(!require(dplyr)){install.packages("dplyr", dependencies = TRUE);library(dplyr)}
  if(!require(tidyverse)){install.packages("tidyverse", dependencies = TRUE);library(tidyverse)}
  if(!require(tidytext)){install.packages("tidytext", dependencies = TRUE);library(tidytext)}
  if(!require(tm)){install.packages("tm", dependencies = TRUE);library(tm)}
  if(!require(stringr)){install.packages("stringr", dependencies = TRUE);library(stringr)}
  if(!require(ggplot2)){install.packages("ggplot2", dependencies = TRUE);library(ggplot2)}
  if(!require(data.table)){install.packages("data.table", dependencies = TRUE);library(data.table)}
  if(!require(stringdist)){install.packages("stringdist", dependencies = TRUE);library(stringdist)}
  if(!require(lubridate)){install.packages("lubridate", dependencies = TRUE);library(lubridate)} #dates arithmatics
  
  
  if(!require(tm)){install.packages("tm", dependencies = TRUE);library(tm)}
  if(!require(corpus)){install.packages("corpus", dependencies = TRUE);library(corpus)}
  
  if(!require(rJava)){install.packages("rJava", dependencies = TRUE);library(rJava)}
  if(!require(qdap)){install.packages("qdap", dependencies = TRUE);library(qdap)}
  
  #geocoding
  if(!require(ggmap)){install.packages("ggmap", dependencies = TRUE);library(ggmap)}
  
  #sentiment analysis
  if(!require(httr)){install.packages("httr", dependencies = TRUE);library(httr)}
  if(!require(jsonlite)){install.packages("jsonlite", dependencies = TRUE);library(jsonlite)}
  
  
  if(!require(googleLanguageR)){install.packages("googleLanguageR", dependencies = TRUE);library(googleLanguageR)}
  
}

#change home directory accordingly
home.dir <- "/home/antonio/Desktop/New PC/Uni/phd/Computer Science for Heurmeneutics - Luxemburg/kickoff start/Lorella/tutorial"

setwd(home.dir)

#directories and global variables. Change accordingly
{
# directory containing the original corpus
dir.corpus <- "CI_newspaper_subcorpora"
# directory containing the cleaned corpus
dir.corpus.cleaned <- "CI_newspaper_subcorpora_cleaned"
#directory containing the files used for NER
dir.NER <- "NER_corpora"
# directory containing the output files of NER
dir.NER.tagged<- "NER/sequence_tagging/NER_corpora_tagged"

#directory containing the dataframes results of NER
dir.NER.dataframe <- "NER_dataframes"
#directory containing the dataframes results of NER after entity cleaning
dir.NER.dataframe.clean <- "NER_dataframes_clean"

#directory containing the results of Geocoding
dir.geocoding <- "geocoding"
#directory containing the results of Sentiment Analysis
dir.sentiment <- "NER_sentiment"
#directory containing the results the network file for Social Network Analysis
dir.SNA <- "SNA"

#directory containing the dataframes that will be used for the Shiny app
dir.shiny.datasets <- "shiny_datasets"


#key used access the google api for geocoding
#change name of the text file containing the key accordingly
geocoding_key_filename <- "geocoding_key"
API_KEY_GEOCODING <- readLines(file(geocoding_key_filename, open="r"))

#key used access the google api for sentiment analysis
#change name of the text file containing the key accordingly
sentiment_key_filename <- "sentiment_key"
API_KEY_SENTIMENT_C2DH <- readLines(file(sentiment_key_filename, open="r"))

}



read.corpus <- function(dir.corpus){
  
  {
  "
  This function reads the corpus
  
  Input
  @param dir.corpus The directory that contains the corpus
  this directory will contain a subdirectory for each newspaper title (named after the newspaper)
  each subdiretory will contain all issues of the said title
  
  Output:
  dataframe containing every issue
  each line will refer to a specidi issue
   'data.frame':	14 obs. of  4 variables:
   $ title: chr  'Cronaca_Sovversiva'  ...
   $ issue: chr  '2012271201_1903-06-06_ed-1_seq-1_ocr.txt' ...
   $ text : chr  'Ebdomadario anàrchico di propaganda rivoluzionaria'
   $ date : chr  '1903-06-06'  ...
  "
  }

  #read one subdirectory for each newspaper title
  conf.dirs <- list.dirs(path = dir.corpus, full.names = FALSE, recursive = FALSE)
  
  #create a dataframe for each title
  #each dataframe contains title, issue and text text
  newspapers <- lapply(conf.dirs, function(dir){
    list.files <- list.files(file.path(dir.corpus, dir))
    filenames <- file.path(dir.corpus, dir, list.files)
    files <- sapply(filenames, read_file)
    names(files) <- basename(filenames)
    #files
    
    df <- data.frame(title = rep(basename(dir), length(files)), issue = names(files), text = files, stringsAsFactors = FALSE)
    df
    })
  
  #combine dataframes in a single dataframe and add a date column
  newspapers <- bind_rows(newspapers)
  newspapers <- newspapers %>%
    mutate(date = str_extract(issue, "[0-9]{4}-[0-9]{2}-[0-9]{2}")) %>% #extract date issue
    arrange(issue)
  
  newspapers
}

#Utility functions used to read the different dataframes used
{
read.NER.dataframes <- function(dir.NER.dataframe){
  
  list.files <- list.files(file.path(dir.NER.dataframe))
  list.files <- list.files[!grepl("_grouped", list.files)]
  list.files <- list.files[!grepl("_title", list.files)]
  list.files <- list.files[!grepl("_clean", list.files)]
  list.files <- list.files[!grepl("_sentences", list.files)]
  
  filenames <- file.path(dir.NER.dataframe, list.files)
  
  NER.df <- NULL
  for(f in 1:length(filenames)){
    df <- read.csv(file = filenames[f], header = TRUE, sep = "\t", quote = "", fileEncoding = "UTF8", stringsAsFactors = F)
    NER.df[[length(NER.df) + 1]] <- df
  }
  
  names(NER.df) <- basename(list.files)
  
  NER.df
}
read.NER.dataframes.clean <- function(dir.NER.dataframe.clean){
  
  list.files <- list.files(file.path(dir.NER.dataframe.clean))
  list.files <- list.files[grepl("_clean", list.files)]
  list.files <- list.files[!grepl("grouped", list.files)]
  list.files <- list.files[!grepl("sentences", list.files)]
  
  
  filenames <- file.path(dir.NER.dataframe.clean, list.files)
  NER.df.clean <- NULL
  for(f in 1:length(filenames)){
    df <- read.csv(file = filenames[f], header = TRUE, sep = "\t", quote = "", fileEncoding = "UTF8", stringsAsFactors = F)
    NER.df.clean[[length(NER.df.clean) + 1]] <- df
  }
  names(NER.df.clean) <- gsub("_clean", "", basename(list.files))
  
  NER.df.clean
}
read.NER.dataframes.grouped <- function(dir.NER.dataframe.clean){
  
  list.files <- list.files(file.path(dir.NER.dataframe.clean))
  list.files <- list.files[grepl("_clean", list.files)]
  list.files <- list.files[grepl("grouped", list.files)]
  list.files <- list.files[!grepl("sentences", list.files)]
  
  
  filenames <- file.path(dir.NER.dataframe.clean, list.files)
  NER.df.clean <- NULL
  for(f in 1:length(filenames)){
    df <- read.csv(file = filenames[f], header = TRUE, sep = "\t", quote = "", fileEncoding = "UTF8", stringsAsFactors = F)
    NER.df.clean[[length(NER.df.clean) + 1]] <- df
  }
  names(NER.df.clean) <- basename(list.files)
  names(NER.df.clean) <- gsub("_clean_grouped", "", basename(list.files))
  
  NER.df.clean
}

read.sentence.dataframes <- function(dir.NER.dataframe){
  
  list.files <- list.files(file.path(dir.NER.dataframe))
  list.files <- list.files[grepl("_sentences", list.files)]
  list.files <- list.files[!grepl("_clean", list.files)]
  list.files <- list.files[!grepl("processed", list.files)]
  
  
  filenames <- file.path(dir.NER.dataframe, list.files)
  
  sentence.df <- NULL
  for(f in 1:length(filenames)){
    df <- read.csv(file = filenames[f], header = TRUE, sep = "\t", quote = "", fileEncoding = "UTF8", stringsAsFactors = F)
    sentence.df[[length(sentence.df) + 1]] <- df
  }
  
  names(sentence.df) <-  gsub("_sentences", "", basename(list.files))
  
  sentence.df
}
read.sentence.dataframes.clean <- function(dir.NER.dataframe.clean){
  
  list.files <- list.files(file.path(dir.NER.dataframe.clean))
  list.files <- list.files[grepl("_clean", list.files)]
  list.files <- list.files[grepl("sentences", list.files)]
  list.files <- list.files[!grepl("processed", list.files)]
  
  
  filenames <- file.path(dir.NER.dataframe.clean, list.files)
  sentence.df.clean <- NULL
  for(f in 1:length(filenames)){
    df <- read.csv(file = filenames[f], header = TRUE, sep = "\t", quote = "", fileEncoding = "UTF8", stringsAsFactors = F)
    sentence.df.clean[[length(sentence.df.clean) + 1]] <- df
  }
  names(sentence.df.clean) <-  gsub("_sentences_clean", "", basename(list.files))
  
  sentence.df.clean
}

read.sentence.dataframes.clean.selected <- function(dir.sentiment){
  
  list.files <- list.files(file.path(dir.sentiment))
  list.files <- list.files[grepl("sentences", list.files)]
  list.files <- list.files[grepl("selected", list.files)]
  list.files <- list.files[!grepl("processed", list.files)]
  
  
  filenames <- file.path(dir.sentiment, list.files)
  sentence.df.clean <- NULL
  for(f in 1:length(filenames)){
    df <- read.csv(file = filenames[f], header = TRUE, sep = "\t", quote = "", fileEncoding = "UTF8", stringsAsFactors = F)
    sentence.df.clean[[length(sentence.df.clean) + 1]] <- df
  }
  names(sentence.df.clean) <- basename(list.files)
  
  sentence.df.clean
}
read.sentence.dataframes.clean.processed <- function(dir.sentiment){
  
  list.files <- list.files(file.path(dir.sentiment))
  list.files <- list.files[grepl("processed", list.files)]
  
  filenames <- file.path(dir.sentiment, list.files)
  sentence.df.clean <- NULL
  for(f in 1:length(filenames)){
    df <- read.csv(file = filenames[f], header = TRUE, sep = "\t", quote = "", fileEncoding = "UTF8", stringsAsFactors = F)
    sentence.df.clean[[length(sentence.df.clean) + 1]] <- df
  }
  names(sentence.df.clean) <- basename(list.files)
  
  sentence.df.clean
}
}

clean.corpus <- function(corpus, dir.corpus.cleaned,
                         lower.case = FALSE,
                         remove.punctuation = TRUE, 
                         remove.digits = TRUE, 
                         remove.non.alphanum = TRUE,
                         remove.stopwords = FALSE,
                         n.char.filter = 2,
                         freq.token.filter = 0){
  
  {
  "
  This function applies some cleaning preprocessing to the corpus
  
  Input
  @corpus The dataframe containing the corpus
  dataframe containing every issue
  each line will refer to a specidi issue
   'data.frame':	14 obs. of  4 variables:
   $ title: chr  'Cronaca_Sovversiva'  ...
   $ issue: chr  '2012271201_1903-06-06_ed-1_seq-1_ocr.txt' ...
   $ text : chr  'Ebdomadario anàrchico di propaganda rivoluzionaria'
   $ date : chr  '1903-06-06''  ...'
    
  @dir.corpus.cleaned String containing the directory that will contain the cleaned corpus
  @lowe.case Logical. If True, text will beturned to lower case letters
  @remove.punctuation Logaical. If true, all punctuation will be removed from the text
  @remove digits Logical. If true, all digits will be removed from the text
  @remove.non.alpha Logical. If true, all non alphanumeric characters will be removed from the text
  @remove.stopwords Logical. If true, all Italian and English stopwords will be removed from the text
  @n.char.filter Numeric. All words with number of letters ower than this value will be removed. Set to zero to ignore this effect
  @freq.token.filter Numeric. All words with frequency lower than this value (in the entire title) will be removed. Set to zero to ignore this effect

  This function will create a directory for the cleaned corpus, with the same structure as the one containing the original corpus
  each subdiretory will contain all issues of the said title
  
  Output:
  dataframe containing every issue
  each line will refer to a specidi issue
   'data.frame':	14 obs. of  4 variables:
   $ issue: chr  '2012271201_1903-06-06_ed-1_seq-1_ocr.txt' ...
   $ title: chr  'Cronaca_Sovversiva'  ...
   $ text : chr  'Ebdomadario anàrchico di propaganda rivoluzionaria'
   $ date : chr  '1903-06-06'  ...'
   "
  }
  
  conf.dirs <- unique(corpus$title)#list.dirs(path = dir.corpus, full.names = FALSE, recursive = FALSE)
  
  dir.create(dir.corpus.cleaned, showWarnings = FALSE)
  setwd(dir.corpus.cleaned)
  
  corpus.cleaned <- NULL
  for(dir in conf.dirs){
    news.title <- basename(dir)
    news <- corpus[corpus$title == news.title, ]
    
    news.cleaned <- news
  
    
    if(lower.case){
      news.cleaned <- news.cleaned %>% 
        utate(text = tolower(text))
    }
    if(remove.punctuation){
      news.cleaned <- news.cleaned %>% 
        mutate(text = str_remove_all(text, '[[:punct:]]+')) # remove punctuation
    }
    if(remove.digits){
      news.cleaned <- news.cleaned %>% 
        mutate(text = str_remove_all(text, '[[:digit:]]+')) #remove numbers
    }
    if(remove.non.alphanum){
      news.cleaned <- news.cleaned %>% 
        mutate(text = str_replace_all(text, "[^[:alnum:] [:punct:]]+", " ")) #remove all non-alphanumeric characters (special characters), but leave punctuation
    } 
    
    news.cleaned <- news.cleaned %>% 
      mutate(tokens = str_split(text, "\\s+")) %>%
      unnest()
    
    if(remove.stopwords){
      news.cleaned <- news.cleaned %>% 
        filter(!tokens %in% union(stopwords(kind = "italian"), stopwords(kind = "en")) ) #remove Italian and English stopwords
    }
    
    news.cleaned <- news.cleaned %>% 
      filter(n.char.filter < nchar(tokens) ) %>% # remove all wordsa with length 2 or less
      group_by(tokens) %>%
      filter(freq.token.filter < n()) %>% # remove tokens that appear only once
      # ungroup() %>%
      group_by(issue) %>%
      dplyr::summarize(title = unique(title), text = paste(tokens, collapse = " "), date = unique(date)) %>%
      arrange
    
    corpus.cleaned <- rbind.data.frame(corpus.cleaned, news.cleaned)
        
    dir.create(file.path(dir), showWarnings = FALSE)
    setwd(file.path(dir))
    
    skip_to_next <- FALSE
    
    # Note that print(b) fails since b doesn't exist
    
    tryCatch(    news.cleaned %>%
                   group_by(issue) %>%
                   do(writeLines(.$text, paste0(unique(.$issue))))
                 , error = function(e) { skip_to_next <<- TRUE})

    setwd("..")
    
    if(skip_to_next) { next }
    
    news.cleaned %>%
      group_by(issue) %>%
      do(writeLines(.$text, paste0(unique(.$issue))))
    
  }
  setwd(home.dir)
  
  corpus.cleaned
  
}


write.files.NER <- function(corpus, dir.NER){
  {
  "
  This function reads the corpus and create files ready to be used as input for NER
  
  Input
  @corpus The dataframe containing the corpus
  dataframe containing every issue
  each line will refer to a specidi issue
   'data.frame':	14 obs. of  4 variables:
   $ title: chr  'Cronaca_Sovversiva'  ...
   $ issue: chr  '2012271201_1903-06-06_ed-1_seq-1_ocr.txt' ...
   $ text : chr  'Ebdomadario anàrchico di propaganda rivoluzionaria'
   $ date : chr  '1903-06-06''  ...'
    
  This function will create a directory for the the files used for Named Entity Recognition, with the same structure as the one containing the original corpus
  For each issue, a file is created that contains one word for each line
  
   "
  }
  dir.create(file.path(dir.NER), showWarnings = FALSE)
  setwd(file.path(dir.NER))
  
  corpus.split <- split(corpus, corpus$title)
  
  for(i in 1:length(corpus.split)){
    np.name <- names(corpus.split)[i]
    dir.create(file.path(np.name), showWarnings = FALSE)
    setwd(file.path(np.name))
    
    corp <- corpus.split[[i]]
    for(j in 1:nrow(corp)){
      issue <- corp[j, ]
      issue %>%
        # mutate(text = str_remove_all(text, '[[:punct:]]+')) %>%
        mutate(text = gsub('([[:punct:]])', ' \\1 ', text)) %>%
        mutate(tokens = str_split(text, "\\s+")) %>%
        unnest(tokens) %>% 
        select(tokens) %>% 
        write.table(file = issue$issue, fileEncoding = "UTF8", quote = F, row.names = F, col.names = F)
    }
    setwd(file.path("./.."))
    
  }
  setwd(file.path("./.."))
  
}



create.NER.dataframe <- function(dir.NER, dir.NER.dataframe){
  
  '
  This function reads the result of the NER and createx different dataframes that will be used for the analysis
  
  Input
  @dir.NER.tagged Directory contained the NER tagged corpus
  @dir.NER.dataframe Directory that will contain the dataframes created

  This function will create a directory that will contan the files that will be used for the analysis (sentiment analysis, social network analysis and so on)
  For each title, three dataframes are created
  
  NER.df
  $ entity      : chr [1:195] "Italia" "Austria" ...
  $ entity.lower: chr [1:195] "italia" "austria" ...
  $ type        : chr [1:195] "B-GPE" "B-GPE"  ...
  $ issue       : chr [1:195] "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt"  "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt" ...
  $ sentence    : num [1:195] 1059 1139 1028 ...
  $ date        : chr [1:195] "1915-01-01" "1915-01-01" ...
  $ issue.freq  : int [1:195] 10 5 4 4 3 3 3 3 2 2 ...
  
  NER.df.grouped
  $ entity  : chr [1:195] "Abbonatevi giornale L" "Adesso ...
  $ type    : chr [1:195] "B-ORG" "B-PER"  ...
  $ issue   : chr [1:195] "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt"  "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt" ...
  $ sentence: chr [1:195] "1275" "1102" "1234" "1055" ...
  $ date    : chr [1:195] "1915-01-01" "1915-01-01"  ...
  
  sentences
  $ index    : chr  "999" "1000"...
  $ sentence : chr  "INNO XXIX" "PII ) VECCHIO DIPESO QUOTIDIANO ITALIANO ALL ’ OVEST NEW YORK L ’ ITALIA PRESS CO" ...
  $ separator: chr  "." "."  ...
  $ issue    : chr  "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt" "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt" ...
 
   '
  
  conf.dirs <- list.dirs(path = dir.NER.tagged, full.names = FALSE, recursive = FALSE)
  
  conf.dirs <- conf.dirs[sapply(conf.dirs, function(cd){ any(grepl("NER_output", list.files(file.path(dir.NER.tagged, cd)))) })]
  
  dir.create(file.path(dir.NER.dataframe), showWarnings = FALSE)
  
  first.write <- TRUE
  
  
  f <- list.files(dir.NER.dataframe, include.dirs = F, full.names = F, recursive = F)
  # remove the files
  file.remove(file.path(dir.NER.dataframe, f))
  
  # dir <- conf.dirs[[1]]
  for(dir in conf.dirs){
    list.files <- list.files(file.path(dir.NER.tagged, dir))
    list.files <- list.files[grepl("NER_output", list.files)]
    
    filenames <- file.path(dir.NER.tagged, dir, list.files)
    #NER <- lapply(1:length(filenames),  function(i){
    
    # file.remove(file.path(dir.NER.dataframe, dir))

    print(paste("Processing ", dir))
    
    separators <- c(".", ";", ":", "!", "?")
    index <- 1
    
    first.write <- TRUE
    for(f in 1:length(list.files)){
      
      file <- gsub("_NER_output", "", list.files[f])
      if(!is.na(file.info(filenames[f])$size) && 0 < file.info(filenames[f])$size){
        csv <- read.csv(file = filenames[f], header = FALSE, sep = "\t", quote = "", fileEncoding = "UTF8", stringsAsFactors = F)
        if(nrow(csv) == 1){
          csv <- NULL
        }else{
          csv <- csv[-1, ]
          
          sentences <- data.frame(index = NULL, sentence = NULL, separator = NULL, issue = NULL)
          csv.test <- cbind(csv, sentence = rep(0, nrow(csv)))
          #str_detect(e, '[[:punct:]]+')
          i <- 1
          while(i <= nrow(csv)){
            words <- NULL
            indexes <- NULL
            e <- csv[i, ]$V1
            while(!e %in% separators & i <= nrow(csv)){
              
              words <- c(words, e)
              indexes <- c(indexes, i)
              
              csv.test[i, "sentence"] <- index
              i <- i + 1
              e <- csv[i, ]$V1
            }
            sentences <- bind_rows(sentences, c(index = index, sentence = paste(words, collapse = " "), separator = e, issue = file))
            index <- index + 1
            i <- i + 1
            
          }
          
          csv <- csv.test
          csv <- csv[!csv$V4 %in% c("O", ""), ]
          while(grepl("I-", csv[1, ]$V4))
            csv <- csv[-1, ]
          # while(cvs)
          csv <- cbind(issue = rep(file, nrow(csv)), csv, date = rep(str_extract(file, "[0-9]{4}-[0-9]{2}-[0-9]{2}"), nrow(csv)))
        }
      }else{
        csv <- NULL
      }
      
      if(is.null(csv))
        next
      #}
      
      setwd(file.path(dir.NER.dataframe))
      
      
      NER <- csv#rbindlist(NER, fill = TRUE)
      
      NER <- NER[, c("issue", "V1", "V2", "V3", "V4", "sentence", "date")]
      # NER <- NER[, apply(NER, 2, function(cc){!any(is.na(cc))})]
      
      
      ner <- NER
      
      
      nert <- tibble(issue = ner$issue, word = ner$V1, word.lower = ner$V2, known = ner$V3, entity = ner$V4, sentence = ner$sentence, date = ner$date)
      nert <- nert %>%
        filter(!nert$entity %in% c("O", ""))
      
      ent.type <- grepl("B-", nert$entity)
      vect <- vector(length = nrow(nert))
      
      count <- 0
      i <- 1
      while(i <= length(vect)){
        
        # if(nert[i, ]$entity %in% c("B-ORG", "B-GPE", "B-PER", "B-LOC")){
        if(ent.type[i]){
          count <- count + 1
          vect[i] <- count
        }else{
          vect[i] <- count
        }
        i <- i + 1
      }
      
      nert <- cbind(nert, ent.ind = vect)
      nert <- tibble(issue = nert$issue, word = nert$word, word.lower = nert$word.lower, entity = nert$entity, sentence = nert$sentence, date = nert$date, ent.ind = nert$ent.ind)
      
      
      # nert.split <- split(nert, (as.numeric(rownames(nert))-1) %/% as.integer(nrow(nert)/100))
      
      
      nert.merged <- nert %>%
        group_by(ent.ind) %>%
        summarize(issue = unique(issue), word = paste(word, collapse = " "), word.lower = paste(word.lower, collapse = " "), entity = entity[1],
                  sentence = sentence[1], date = unique(date))
      
      
      # nert.merged <- rbindlist(nert.merged.chunk)
      nert.merged <- nert.merged[, -1]
      
      
      
      
      
      
      nd <- nert.merged
      
      NER.df <- nd %>%
        mutate(word = str_replace_all(word, "[^[:alnum:]]+", " "), word.lower = str_replace_all(word.lower, "[^[:alnum:]]+", " ")) %>%
        mutate(word = str_replace(gsub("\\s+", " ", str_trim(word)), "B", "b"), word.lower = str_replace(gsub("\\s+", " ", str_trim(word.lower)), "B", "b")) %>%
        filter(!nchar(word) < 2) %>%
        group_by(word, entity, issue) %>%
        add_count(word, sort = TRUE) %>%
        distinct(word, entity, issue, .keep_all = TRUE) %>% 
        arrange(desc(n))
      
      colnames(NER.df) <- c("issue", "entity", "entity.lower", "type", "sentence", "date", "issue.freq")
      NER.df <- NER.df[, c("entity", "entity.lower", "type", "issue", "sentence", "date", "issue.freq")]
      
      
      NER.df.grouped <- nd %>%
        mutate(word = str_replace_all(word, "[^[:alnum:]]+", " "), word.lower = str_replace_all(word.lower, "[^[:alnum:]]+", " ")) %>%
        mutate(word = str_replace(gsub("\\s+", " ", str_trim(word)), "B", "b"), word.lower = str_replace(gsub("\\s+", " ", str_trim(word.lower)), "B", "b")) %>%
        filter(!nchar(word) < 2) %>%
        group_by(word, entity) %>%
        summarize(issue = paste(unique(issue), collapse = " "), sentence = paste(sentence, collapse = " "), date = paste(unique(date), collapse = " "))
      
      colnames(NER.df.grouped) <- c("entity", "type", "issue", "sentence", "date")

      
      
      NER.df %>%
        write.table(file = dir, append = TRUE, col.names = first.write, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
      
      NER.df.grouped %>%
        write.table(file = paste(dir, "_grouped"), append = TRUE, col.names = first.write, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
      
      write.table(sentences, file = paste(dir, "_sentences", sep = ""), append = TRUE, col.names = first.write, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
      
      if(first.write){
        first.write <- FALSE
      }
      
      setwd("..")
    }
    

    
    
  }
  
}

process.dataframes.NER <- function(dir.NER.dataframe, dir.NER.dataframe.clean, title.freq.filter = 0){
  
  
  '
  This function reads the NER dataframes and handles exceptions such as wrong entity type, remove useless entities and so on
  
  Input
  @dir.NER.dataframe Directory that will contain the NER dataframes
  @dir.NER.dataframe.clean Directory that will contain the NER dataframes cleaned
  @title.freq.filter entities that appear, in a specific title, a number of times lower than this value will be filtered out. Set to 0 to ignore this
  

  The following dataframes are written in the dedicated folder:
  
  NER.df
  $ entity      : chr [1:195] "Italia" "Austria" ...
  $ entity.lower: chr [1:195] "italia" "austria" ...
  $ type        : chr [1:195] "B-GPE" "B-GPE"  ...
  $ issue       : chr [1:195] "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt"  "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt" ...
  $ sentence    : num [1:195] 1059 1139 1028 ...
  $ date        : chr [1:195] "1915-01-01" "1915-01-01" ...
  $ issue.freq       : int [1:195] 10 5 4 4 3 3 3 3 2 2 ...
  
  NER.df.title
  $ entity    : chr [1:57] "fourmies" "adorava"  ...
  $ type      : chr [1:57] "B-ORG" "B-PER" ...
  $ title.freq: int [1:57] 2 1 ...
  
  df.all.counts
  $ source    : chr [1:9] "Cronaca_Sovversiva" "Il_Patriota" ...
  $ type      : chr [1:9] "B-ORG" "B-GPE"  ...
  $ count.all : int [1:9] 1 13 ...
  $ count.many: int [1:9] 1 13 ...
  $ count.one : int [1:9] 0 0 ...
  $ freq.one  : num [1:9] 0 0 ...
  
  count.all counts the number of entities of a certan type in a title, count.many counts the number of entities that appear more than ones,
  count.one counts the number of entities that appear only once, freq.one is the ratio between counts one and counts.many 
  '
  
  NER.df <- read.NER.dataframes(dir.NER.dataframe)
  
  NER.df <- process.dataframes.NER.exceptions(NER.df, dir.NER.dataframe.clean)
  
  
  NER.df.title <- NULL
  for(f in 1:length(NER.df)){
    df <- NER.df[[f]] %>%
      group_by(entity.lower, type) %>%
      dplyr::summarize(title.freq = sum(issue.freq)) %>%
      filter(title.freq.filter < title.freq) %>%
      arrange(desc(title.freq))
    colnames(df) <- c("entity", "type", "title.freq")
    write.table(df, file = file.path(dir.NER.dataframe.clean, paste(names(NER.df)[f], "_clean_grouped", sep = "")), col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
    NER.df.title[[length(NER.df.title) + 1]] <- df
  }
  
  
  df.counts <- NULL
  for(f in 1:length(NER.df.title)){
    df <- NER.df.title[[f]] %>%
      group_by(type) %>%
      dplyr::summarize(count.all = n(), count.many = sum(1 < title.freq), count.one = sum(title.freq == 1)) %>%
      mutate(freq.one = count.one/count.all) %>%
      arrange(desc(freq.one))
    
    # write.table(df, file = paste(filenames[f], "_title", sep = ""), col.names = TRUE, row.names = TRUE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
    df.counts[[length(df.counts) + 1]] <- df
  }
  
  names(df.counts) <- names(NER.df)
  
  df.all.counts <- bind_rows(df.counts, .id = 'source')
  write.table(df.all.counts, file = file.path(dir.NER.dataframe.clean, "Entity counts"), col.names = TRUE, row.names = TRUE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
  
  
  NER.sentence <- read.sentence.dataframes(dir.NER.dataframe)
  
  for(f in 1:length(NER.sentence)){
    df <- NER.sentence[[f]]
    write.table(df, file = file.path(dir.NER.dataframe.clean, paste(names(NER.sentence)[f], "_sentences_clean", sep = "")), col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
  }
  
  
}

#Handle exceptions for entitites
{
  
  process.dataframes.NER.exceptions <- function(NER.df, dir.NER.dataframe.clean){
    
    # list.files <- list.files(file.path(dir.NER.dataframe))
    # list.files <- list.files[!grepl("_grouped", list.files)]
    # list.files <- list.files[!grepl("_title", list.files)]
    # list.files <- list.files[!grepl("_clean", list.files)]
    # list.files <- list.files[!grepl("_sentences", list.files)]
    # 
    # filenames <- file.path(dir.NER.dataframe, list.files)
    # 
    dir.create(file.path(dir.NER.dataframe.clean), showWarnings = FALSE)
    
    NER.df.exception <- NULL
    for(f in 1:length(NER.df)){
      # for(f in c(1,2,3,5,6,7,8,9)){
      
      df <- NER.df[[f]]
      
      dff <- df
      
      print(paste("Title: ", names(NER.df)[f]))
      
      #Entities that must be removed
      entities.strict <- c("presidenza", "sei mesi", "francese", "conn", "tel",
                           "tre mesi", "dopo", "compagnie", "americana", "applausi", "federazione", "italiano",
                           "un", "ii", "nell", "inc", "val", "stati", 
                           "perÃ¹", "perÃ¹", "per", "marzo",  "l'on", "san", "città", "costa", "co", "una", "alleati",
                           "mrs", "fon", "vive", "conferenza", "unione", "umanità", "sabato", "egli",
                           "cav", "cai", "cal", "ita", "st", "pag", "provincia", "nazione", 
                           "comitato", "compagnia", "corte", "cor", "iti", "commissione", "coni",
                           "in", "corona", "maggio", "sindaco", "guerra", "anno", "domani", "avvisa",
                           "pagina", "universitÃ", "aprile", "hi", "comune", "cents copia", "the most up",
                           "gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto",
                           "settembre", "ottobre", "novembre", "dicembre",
                           "consulta", "allora", "un", "im", "nazionale", "ma", "americana", "societÃ", 
                           "cosi", "giustizia", "fotografi", "tribunale", "october", "act", "stalo", "comm",
                           "canada messico", "regione", "l ita", "inter", "art", "agenzia", "ut", "disse",
                           "fi", "pro", "genova napoli", "francesi", "americane", "uro", "oal", "die", "all",
                           "vapori", "mass", "city", "italo", "new", "ini", "si", "nav", "ino", "dice", 
                           "russia giappone", "venerdÃ¬", "imi", "al", "an", "dal", "del", "ia", "alia", "malgrado", "rii",
                           "lunedi", "martedi", "mercoledi", "giovedi", "venerdi", "sabato", "domenica",
                           "year", "proprietaria", "estero", "citta", "ih", "inchiesta")
      entities.substring <- c("giornale italiano", "largest circulation", "telegramma speciale",
                              "giustizia e fratellanza", "giustizia fratellanza", "dispacci", "the italian daily newspaper",
                              "proprietari ing", "telegrafico", "italiana", "agenzie viaggi", "under permit",
                              "vive lotta difesa", "stockton drug", "carrier foreign countries", "telefono red con",
                              "cambio berlino")
      
      
      #change to person
      
      df <- remove.entity.strict(entities.strict, df, 0)
      
      df <- remove.entity.substring(entities.substring, df, 1)
      
      
      #Entities that must changed type (find it in longer sentences too)
      
      #change name after merged
      entities.titles <- c("principe", "ministro", "ministri", "direttore", "sovrano", "sindaco", "avvocato",
                           "generale", "segretario", "vice", "pontefice", "regina", "senatore", "cardinale",
                           "maestro", "poeta", "governatore", "colonnello", "imperatore", "generale")
      entities.title.short <- c("dr", "mr", "duca", "gen", "prof", "re", "czar", "zar", "ing", "sig", "rev")
      
      entities.GPE <- c("new york", "italia", "l'italia", "stati uniti", "austria", "napoli", "roma", "trieste",
                        "montenegro", "port arthur", "tokio", "milano", "palermo", "new orleans", "svizzera", 
                        "salonicco", "inghilterra", "dalmazia", "germania", "costantinopoli", "chicago", 
                        "pacific coast", "california", "torino", "china", "galizia", "filadelfia", "new jersey",
                        "austria", "san francisco", "united states", "sofia", "spezia", "parigi", "belgrado",
                        "firenze", "gram bretagna", "francia", "los angeles", "ancona", "varsavia", "como", 
                        "buenos aires", "fiume", "genova", "cirenaica", "alaska", "gorizia", "finlandia",
                        "cleveland", "colorado", "madrid", "albania", "anversa", "eritrea", "tripoli", 
                        "versailles", "sicilia", "seattle")
      entities.LOC <- c("pacific coast", "columbus avenue", "columbus avenue", "montgomery avenue", "mediterraneo", "carso", 
                        "canale panama", "market street", "liguria", "america")
      entities.PER <- c("orlando", "evans", "turati", "nunzio nasi", "garibaldi", "verdi", "william taft", "taft",
                        "kaiser", "duca degli abruzzi", "marcora", "duca d'aosta", "cook", "ksso", "president theodor roosevelt",
                        "wilson", "kgel", "burleson", "luigi cadorna", "enrico caruso", "nunzio nasi", "margherita", 
                        "marconi", "sennino", "carranza", "giolitti", "menelik", "woodrow wilson", "diaz", "generale armando diaz", "generale felice diaz",
                        "giosuÃ¨ carducci", "savoia", "kuropatkin", "dettolo")
      entities.ORG <- c("vaticano", "ettore patrizi editore", "veloce", "marina", "new york times", "governo", "regno")
      
      
      df <- entity.OCR.mistake(c(entities.GPE, entities.LOC, entities.ORG, entities.PER, entities.titles), df, 1)
      
      #entities that must be merged
      entities.merged <- c("diaz", "san francisco", "tullio murri", "pontefice, papa", 
                           "roosevelt, presidente roosevelt, teodoro roosevelt, theodor roosevelt", "wilson, woodrow wilson",
                           "ettore, ettore patrizi, patrizi editore, ettore patrizi editore",
                           "nunzio ansi, nunzio, nasi", "stati uniti, united states", "l'austria, austria",
                           "roma, koma, itoma, romÃ, doma", "generale cadorna, cadorna",
                           "william taft, taft",
                           "città del messico, città messico,"
      )
      
      
      
      df <- merge.entities(df)
      
      df <- change.entity.type(entities.GPE, "B-GPE", df, 1)
      df <- change.entity.type(entities.PER, "B-PER", df, 1)
      df <- change.entity.type(entities.title.short, "B-PER", df, 0)
      
      
      
      df <- special.entities(df)
      
      NER.df.exception[[length(NER.df.exception) + 1]] <- df
      
      write.table(df, file = paste(file.path(dir.NER.dataframe.clean, names(NER.df)[f]), "_clean", sep = ""), col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
      
    }
    
    names(NER.df.exception) <- names(NER.df)
    NER.df.exception
  }
  
  remove.entity.strict <- function(entity, df, max.tolerance){
    removed.ind <- unique(unlist(sapply(entity, function(e){ which(stringdist(e, df$entity.lower, "lv") <= max.tolerance)})))
    if(length(removed.ind) == 0)
      return(df)
    df <- df[-removed.ind, ]
    df
  }
  
  remove.entity.substring <- function(entity, df, max.tolerance){
    removed.ind <- unique(unlist(sapply(entity, function(e){agrep(e, df$entity.lower, max = max.tolerance)})))
    if(length(removed.ind) == 0)
      return(df)
    df <- df[-removed.ind, ]
    df
  }
  
  entity.OCR.mistake <- function(entity, df, max.tolerance){
    
    dff <- df
    for(e in entity){
      change.ind <- unique(unlist(which(stringdist(e, df$entity.lower, "lv") <= max.tolerance)))
      if(length(change.ind) != 0){
        dff[change.ind, ]$entity.lower <- e
      }
    }
    
    dff <- dff %>%
      group_by(entity, entity.lower, type, issue) %>% 
      summarise(date = unique(date), issue.freq = sum(issue.freq), sentence = paste(sentence, collapse = " "))
    
    dff
  }
  
  change.entity.type <- function(entity, entity.type, df, max.tolerance){
    
    dff <- df
    
    change.ind <- unique(unlist(sapply(entity, function(e){ which(stringdist(e, df$entity.lower, "lv") <= max.tolerance)})))
    if(length(change.ind) == 0)
      return(df)
    dff[change.ind, ]$type <- entity.type
    
    dff <- dff %>%
      group_by(entity, entity.lower, type, issue) %>% 
      summarise(date = unique(date), issue.freq = sum(issue.freq), sentence = paste(sentence, collapse = " "))
    
    dff
  }
  
  merge.entities <- function(df){
    
    dff <- df
    merge.ind <- unlist(unique(union(agrep("generale diaz", df$entity.lower, max = 2), 
                                     agrep("armando diaz", df$entity.lower, max = 2))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "generale armando diaz"
      dff[merge.ind, ]$type <- "B-PER"
    }
    
    merge.ind <- unique(c(unlist(agrep("felice diaz", df$entity.lower, max = 1)), 
                          unlist(agrep("felix diaz", df$entity.lower, max = 1)),
                          unlist(agrep("generale felix diaz", df$entity.lower, max = 2)),
                          unlist(agrep("gpresidente", df$entity.lower, max = 1))
                          
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "generale felice diaz"
      dff[merge.ind, ]$type <- "B-PER"
    }
    
    merge.ind <- unlist(unique(c(agrep("generale cadorna", df$entity.lower, max = 1), 
                                 agrep("generale luigi cadorna", df$entity.lower, max = 2), 
                                 agrep("cadorna", df$entity.lower, max = 0),
                                 agrep("luigi cadorna", df$entity.lower, max = 1)
    )))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "luigi cadorna"
      dff[merge.ind, ]$type <- "B-PER"
    }
    
    merge.ind <- unlist(unique(c(agrep("woodrow wilson", df$entity.lower, max = 1), 
                                 agrep("presidente wilson", df$entity.lower, max = 1) 
    )))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "woodrow wilson"
      dff[merge.ind, ]$type <- "B-PER"
    }
    
    
    
    merge.ind <- unlist(unique(c(agrep("caruso", df$entity.lower, max = 0),
                                 agrep("enrico caruso", df$entity.lower, max = 1)
    )))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "enrico caruso"
      dff[merge.ind, ]$type <- "B-PER"
    }
    
    merge.ind <- unlist(agrep("san francisco", df$entity.lower, max = 1))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "san francisco"
      dff[merge.ind, ]$type <- "B-GPE"
    }
    
    merge.ind <- unlist(agrep("port arthur", df$entity.lower, max = 1))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "port arthur"
      dff[merge.ind, ]$type <- "B-GPE"
    }
    
    merge.ind <- unlist(agrep("columbus ave", df$entity.lower, max = 1))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "columbus avenue"
      dff[merge.ind, ]$type <- "B-LOC"
    }
    
    merge.ind <- unlist(agrep("montgomery ave", df$entity.lower, max = 1))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "montgomery avenue"
      dff[merge.ind, ]$type <- "B-LOC"
    }
    
    merge.ind <- unique(c(unlist(agrep("societÃ dante alighieri roma", df$entity.lower, max = 2)),
                          unlist(agrep("societÃ dante alighieri", df$entity.lower, max = 1)),
                          unlist(agrep("societÃ dante", df$entity.lower, max = 1)),
                          unlist(agrep("societÃ alighieri", df$entity.lower, max = 1))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "societÃ dante alighieri"
      dff[merge.ind, ]$type <- "B-ORG"
    }
    
    merge.ind <- unlist(which(stringdist("dante alighieri roma", df$entity.lower, "lv") == 0))
    
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "pontefice"
      dff[merge.ind, ]$type <- "B-PER"
    }
    
    merge.ind <- unique(c(unlist(agrep("murri", df$entity.lower, max = 0)),
                          unlist(agrep("murrl", df$entity.lower, max = 0)),
                          unlist(agrep("murri", df$entity.lower, max = 0)),
                          unlist(agrep("tullio murri", df$entity.lower, max = 1))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "tullio murri"
      dff[merge.ind, ]$type <- "B-PER"
    }
    
    merge.ind <- unique(union(unlist(agrep("pontefice", df$entity.lower, max = 0)), 
                              unlist(which(stringdist("papa", df$entity.lower, "lv") == 0))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "pontefice"
      dff[merge.ind, ]$type <- "B-PER"
    }
    
    merge.ind <- unique(c(unlist(which(stringdist("kssl", df$entity.lower, "lv") == 0)), 
                          unlist(which(stringdist("kssi", df$entity.lower, "lv") == 0)), 
                          unlist(which(stringdist("kssa", df$entity.lower, "lv") == 0))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "kssl"
      dff[merge.ind, ]$type <- "B-PER"
    }
    
    
    merge.ind <- unlist(agrep("roosevelt", df$entity.lower, max = 1))
    merge.ind.2 <- unlist(agrep("alice roosevelt", df$entity.lower, max = 2))
    merge.ind <- setdiff(merge.ind, merge.ind.2)
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "president theodor roosevelt"
      dff[merge.ind, ]$type <- "B-PER"
    }
    if(length(merge.ind.2) != 0){
      dff[merge.ind.2, ]$entity.lower <- "alice rooselvel"
      dff[merge.ind.2, ]$type <- "B-PER"
    }
    
    
    merge.ind <- unique(c(unlist(agrep("ettore patrizi editore", df$entity.lower, max = 3)), 
                          unlist(agrep("ettore patrizi", df$entity.lower, max = 3)),
                          unlist(agrep("patrizi editore", df$entity.lower, max = 2)),
                          unlist(which(stringdist("patrizi", df$entity.lower, "lv") == 0)),
                          unlist(which(stringdist("patrizi co", df$entity.lower, "lv") == 0))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "ettore patrizi editore"
      dff[merge.ind, ]$type <- "B-ORG"
    }
    
    
    merge.ind <- unique(c(unlist(agrep("nunzio nasi", df$entity.lower, max = 1)), 
                          unlist(agrep("nunzio ansi", df$entity.lower, max = 0)),
                          unlist(agrep("nasi", df$entity.lower, max = 0))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "nunzio nasi"
      dff[merge.ind, ]$type <- "B-PER"
    }
    
    merge.ind <- unique(c(unlist(which(stringdist("stati uniti", df$entity.lower, "lv") == 1)),
                          unlist(which(stringdist("uniti", df$entity.lower, "lv") == 0)),
                          unlist(which(stringdist("stati uniti d'america", df$entity.lower, "lv") == 2)),
                          unlist(which(stringdist("stati uniti damerica", df$entity.lower, "lv") == 2)),
                          unlist(which(stringdist("united states", df$entity.lower, "lv") == 1)),
                          unlist(which(stringdist("united states of america", df$entity.lower, "lv") == 2))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "stati uniti"
      dff[merge.ind, ]$type <- "B-GPE"
    }
    
    merge.ind <- unique(c(unlist(which(stringdist("austria", df$entity.lower, "lv") == 1)),
                          unlist(which(stringdist("l'austria", df$entity.lower, "lv") == 1))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "austria"
      dff[merge.ind, ]$type <- "B-GPE"
    }
    
    merge.ind <- unique(c(unlist(which(stringdist("cina", df$entity.lower, "lv") == 0)),
                          unlist(which(stringdist("china", df$entity.lower, "lv") == 0))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "cina"
      dff[merge.ind, ]$type <- "B-GPE"
    }
    
    merge.ind <- unique(c(unlist(which(stringdist("italia", df$entity.lower, "lv") == 1)),
                          unlist(which(stringdist("l italia", df$entity.lower, "lv") == 1)),
                          unlist(which(stringdist("l italia", df$entity.lower, "lv") == 1))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "italia"
      dff[merge.ind, ]$type <- "B-GPE"
    }
    
    merge.ind <- unique(c(unlist(which(stringdist("roma", df$entity.lower, "lv") == 0)),
                          unlist(which(stringdist("koma", df$entity.lower, "lv") == 0)),
                          unlist(which(stringdist("itoma", df$entity.lower, "lv") == 0)),
                          unlist(which(stringdist("doma", df$entity.lower, "lv") == 0)),
                          unlist(which(stringdist("romÃ", df$entity.lower, "lv") == 0)),
                          unlist(which(stringdist("italia roma", df$entity.lower, "lv") == 1))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "roma"
      dff[merge.ind, ]$type <- "B-GPE"
    }
    
    
    merge.ind <- unique(c(unlist(agrep("william taft", df$entity.lower, max = 1)), 
                          unlist(agrep("taft", df$entity.lower, max = 0))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "william taft"
      dff[merge.ind, ]$type <- "B-PER"
    }
    
    
    merge.ind <- unique(c(unlist(agrep("città del messico", df$entity.lower, max = 1)), 
                          unlist(agrep("città messico", df$entity.lower, max = 1))
    ))
    if(length(merge.ind) != 0){
      dff[merge.ind, ]$entity.lower <- "città del messico"
      dff[merge.ind, ]$type <- "B-GPE"
    }
    
    dff <- dff %>%
      group_by(entity, entity.lower, type, issue) %>% 
      summarise(date = unique(date), issue.freq = sum(issue.freq), sentence = paste(sentence, collapse = " "))
    
    dff
    
  }
  
  special.entities <- function(df){
    
    dannunzio.ind <- unique(c(agrep("annunzio", df$entity.lower, max = 0), agrep("gabriele d", df$entity.lower, max = 0))) 
    lannunzio.ind <- which((stringdist("annunzio", df$entity.lower, "lv") == 0 | agrepl("l'annunzio", df$entity.lower, max = 1)) & (df$type == "B-ORG" | df$type == "B-GPE"))
    
    dannunzio.ind <- setdiff(dannunzio.ind, lannunzio.ind)
    
    dff <- df
    # df[dannunzio.ind, ]$entity <- "Gabriele D'Annunzio"
    if(length(dannunzio.ind) != 0){
      dff[dannunzio.ind, ]$entity.lower <- "gabriele d'annunzio"
      dff[dannunzio.ind, ]$type <- "B-PER"
    }
    if(length(lannunzio.ind) != 0){
      dff <- dff[-lannunzio.ind, ]
    }
    
    dff <- dff %>%
      group_by(entity, entity.lower, type, issue) %>% 
      summarise(date = unique(date), issue.freq = sum(issue.freq), sentence = paste(sentence, collapse = " "))
    
    dff
  }
  
}


#GOOGLE API
#geocoding
{
  # ///////////////////////////////////////////////
  # SUPLEMENTARY FUNCTIONS
  # ///////////////////////////////////////////////
  
  # ///////////////////////////////////////////////
  # 1. GENERATE API CALLS
  # ///////////////////////////////////////////////
  
  url_google_geocoding <- function(search_query_url, key_url) {
    # load libraries
    library(RCurl)
    # convert input into a list
    search_query_url <- sapply(search_query_url, as.list)
    # google gecoding api url
    url_geocoding_api <- "https://maps.googleapis.com/maps/api/geocode/"
    # percent-encode search request
    search_query_url <- sapply(search_query_url, URLencode)
    # construct search request for geocode
    url_geocoding_call <- paste0(url_geocoding_api, "json",
                                 "?address=", search_query_url, "&key=", key_url, "&language=it")
    return(url_geocoding_call)
  }
  
  # ///////////////////////////////////////////////
  
  url_google_place_search <- function(search_query_url, key_url) {
    # load libraries
    library(RCurl)
    # convert input into a list
    search_query_url <- sapply(search_query_url, as.list)
    # google places api url
    url_places_api <- "https://maps.googleapis.com/maps/api/place/"
    # percent-encode search request
    search_query_url <- sapply(search_query_url, URLencode)
    # construct search request for place id
    url_place_search_call <- paste0(url_places_api, "findplacefromtext/",
                                    "json", "?input=", search_query_url,
                                    "&inputtype=textquery","&fields=place_id",
                                    "&key=", key_url)
    return(url_place_search_call)
  }
  
  # ///////////////////////////////////////////////
  
  url_google_place_details <- function(place_id_url, key_url) {
    # load libraries
    library(RCurl)
    # google places api url
    url_places_api <- "https://maps.googleapis.com/maps/api/place/"
    # in case you would want to add "fields" as an argument
    # fields_url <- paste(fields_url, collapse = ",")
    # construct search request for place details
    url_place_details_call <- paste0(url_places_api, "details/",
                                     "json", "?place_id=", place_id_url,
                                     "&fields=formatted_phone_number,website",
                                     "&key=", key_url)
    return(url_place_details_call)
  }
  
  # ///////////////////////////////////////////////
  
  
  
  
  # ///////////////////////////////////////////////
  # SUPLEMENTARY FUNCTIONS
  # ///////////////////////////////////////////////
  
  # ///////////////////////////////////////////////
  # 2. EXTRACT DATA FROM JSON
  # ///////////////////////////////////////////////
  
  get_geodata_from_json_google <- function(geodata_json) {
    # load library
    library(jsonlite)
    # convert json output into r object
    geodata <- lapply(geodata_json, fromJSON,simplifyVector = FALSE)
    # extract coordinates, address and city name
    lat_lng_a <- data.frame(place_id = NA, address = NA, location_type = NA, continent = NA, colloquial_area = NA, country =NA,
                            admin_1 = NA, admin_2 = NA, lat = NA, lng = NA,  city = NA)
    
    for (i in 1:length(geodata)) {
      if (geodata[[i]]$status=="OK") {
        # extract coordinates and address
        lat <- geodata[[i]]$results[[1]]$geometry$location$lat
        lng <- geodata[[i]]$results[[1]]$geometry$location$lng
        address <- geodata[[i]]$results[[1]]$formatted_address
        place_id <- geodata[[i]]$results[[1]]$place_id
        type_ <- paste(geodata[[i]]$results[[1]]$type, collapse = "+")
        
        # find out how many elements there are in "address_components"
        n <- length(geodata[[i]]$results[[1]]$address_components)           
        # extract city and country
        city <- ""
        admin_1 <- ""
        admin_2 <- ""
        country <- ""
        continent <-  ""
        colloquial_area <- ""
        
        for (j in 1:n) {
          # extract the type of the "address_components"
          type <- geodata[[i]]$results[[1]]$address_components[[j]]$types[[1]]
          # extract the city name
          if (type == "postal_town" || type == "administrative_area_level_3") {
            city <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name 
          }
          if(type == "administrative_area_level_1"){
            admin_1 <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
          }
          if(type == "administrative_area_level_2"){
            admin_2 <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
          }
          if(type == "country"){
            country <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
          }
          if(type == "continent"){
            continent <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
          }
          if(type == "colloquial_area"){
            colloquial_area <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
          }
        }                
        lat_lng_a[i, ] <- c(place_id, address, type_, continent, colloquial_area, country, admin_1, admin_2, lat, lng, city)
      } else {
        lat_lng_a[i, ] <- NA
      }
    }
    return(lat_lng_a)
  }
  
  # ///////////////////////////////////////////////
  
  get_place_id_from_json_google <- function(place_json) {
    # load library
    library(jsonlite)
    # convert json output into r object
    place_search <- lapply(place_json, fromJSON,simplifyVector = FALSE)
    # extract place id
    place_id <- list()
    for (i in 1:length(place_search)) {
      if (place_search[[i]]$status=="OK") {
        place_id[[i]] <- place_search[[i]]$candidates[[1]]$place_id
      } else {
        place_id[[i]] <- NA
      }
    }
    return(place_id)
  }
  
  # ///////////////////////////////////////////////
  
  get_contacts_from_json_google <- function(place_details_json) {
    # load library
    library(jsonlite)
    # convert json output into r object
    place_details <- lapply(place_details_json, fromJSON, simplifyVector = FALSE)
    # extract phone number and website
    contacts <- data.frame("phone number" = NA, "website" = NA)
    for (i in 1:length(place_details)) {
      if (place_details[[i]]$status=="OK") {
        # get data
        phone_number <- place_details[[i]]$result$formatted_phone_number
        website <- place_details[[i]]$result$website
        # get rid of NULLs
        info <- list(phone_number, website)
        for (j in 1:length(info)) {
          if (is.null(info[[j]])) info[[j]] <- NA
        }
        # create output data frame
        contacts[i, ] <- info
      } else {
        contacts[i, ] <- NA
      }
    }
    return(contacts)
  }
  
  # ///////////////////////////////////////////////
  
  
  
  
  # ///////////////////////////////////////////////
  # MAIN FUNCTION
  # ///////////////////////////////////////////////
  
  geocode_google <- function(search_query, fields = "coordinates", key) {
    
    # LOAD LIBRARIES
    library(RCurl)
    
    # EXTRACT COORDINATES
    if (any(c("coordinates", "address") %in% fields) || "all" %in% fields) {
      # construct url for geocoding
      url_geocode <- url_google_geocoding(search_query, key)
      # get data from google
      geodata_json <- getURL(url_geocode)
      # get data from json output
      geodata_df <- as.data.frame(sapply(search_query, as.character),
                                  stringsAsFactors = FALSE)
      names(geodata_df) <- "search query"
      rownames(geodata_df) <- NULL
      geodata_df[, 2:12] <- get_geodata_from_json_google(geodata_json)
      # return dataframe with the geodata
      if (all(c("coordinates", "address") %in% fields) || "all" %in% fields) {
        geodata_df
      } else if ("coordinates" %in% fields) {
        geodata_df <- geodata_df[, c(1, 10, 11)]
      } else {
        geodata_df <- geodata_df[, c(1,2,3,4,5,6,7,8,9,12)]
      }
    }
    
    # EXTRACT CONTACTS
    if ("contacts" %in% fields) {
      # /// get place_id from Place Search API ///
      # construct url for place search
      url_place_search <- url_google_place_search(search_query, key)
      # get data from google
      place_json <- getURL(url_place_search)
      # get place_id from json output
      place_id <- get_place_id_from_json_google(place_json)
      # /// get contacts from Place Details API ///
      # construct url for place details
      url_place_details <- url_google_place_details(place_id, key)
      # get data from google
      place_details_json <- getURL(url_place_details)
      # get place_id from json output
      contacts <- get_contacts_from_json_google(place_details_json)
      # /// add contacts to our output data frame ///
      if (!exists("geodata_df")) {
        geodata_df <- as.data.frame(sapply(search_query, as.character),
                                    stringsAsFactors = FALSE)
        names(geodata_df) <- "search query"
        rownames(geodata_df) <- NULL
      }
      geodata_df[, c("phone", "web page")] <- contacts
    }
    return(geodata_df)
  }
  
  # ///////////////////////////////////////////////
  
  
}

#reverse geocoding
{
  # ///////////////////////////////////////////////
  # SUPLEMENTARY FUNCTIONS
  # ///////////////////////////////////////////////
  
  # ///////////////////////////////////////////////
  # 1. GENERATE API CALLS
  # ///////////////////////////////////////////////
  
  url_google_rev_geocoding <- function(coordinates_url, key_url) {
    # load libraries
    library(RCurl)
    # convert everything into data frame
    if (is.matrix(coordinates_url) || is.data.frame(coordinates_url)) {
      coordinates <- data.frame(matrix(NA, nrow(coordinates_url), ncol(coordinates_url)))
      names(coordinates) <- c("lat", "lng")
      coordinates[, 1] <- coordinates_url[, 1]
      coordinates[, 2] <- coordinates_url[, 2]
    } else if (is.list(coordinates_url)) {
      coordinates <- data.frame(matrix(NA, nrow = length(coordinates_url), ncol = 2))
      names(coordinates) <- c("lat", "lng")
      for (i in 1:length(coordinates_url)) {
        coordinates[i, 1] <- coordinates_url[[i]][1]
        coordinates[i, 2] <- coordinates_url[[i]][2]
      }
    } else if (is.vector(coordinates_url)) {
      coordinates <- data.frame(lat = NA, lng = NA)
      coordinates[1,1] <- coordinates_url[1]
      coordinates[1,2] <- coordinates_url[2]
    }
    coordinates$lat_lng <- paste0(coordinates$lat, ",", coordinates$lng)
    # google gecoding api url
    url_geocoding_api <- "https://maps.googleapis.com/maps/api/geocode/"
    # construct search request for reverse geocoding
    url_rev_geocoding_call <- paste0(url_geocoding_api, "json",
                                     "?latlng=", coordinates$lat_lng, "&key=", key_url, "&language=en")
    # return data frame with coordinates and API call
    coordinates$api_call <- url_rev_geocoding_call
    return(coordinates)
  }
  
  # ///////////////////////////////////////////////
  # 2. EXTRACT DATA FROM JSON
  # ///////////////////////////////////////////////
  
  get_rev_geodata_from_json_google <- function(geodata_json) {
    # load library
    library(jsonlite)
    # convert json output into r object
    geodata <- lapply(geodata_json, fromJSON,simplifyVector = FALSE)
    # extract address, city and country from the json output
    # address_df <- data.frame(address = NA, city = NA, country = NA)
    address_df <- data.frame(city = NA, formatted_address = NA, continent = NA, colloquial_area = NA, country =NA,
                             admin_1 = NA, admin_2 = NA)
    
    for (i in 1:length(geodata)) {
      if (geodata[[i]]$status=="OK") {
        # extract address
        address <- geodata[[i]]$results[[1]]$formatted_address
        # find out how many elements there are in "address_components"
        n <- length(geodata[[i]]$results[[1]]$address_components)
        # extract city and country
        city <- ""
        admin_1 <- ""
        admin_2 <- ""
        admin_3 <- ""
        country <- ""
        continent <-  ""
        colloquial_area <- ""
        formatted_address <- ""
        for (j in 1:n) {
          # extract type of "address_components"
          type <- geodata[[i]]$results[[1]]$address_components[[j]]$types[[1]]
          # extract city and country
          if (type == "postal_town" || type == "administrative_area_level_3") {
            city <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name 
          } else if (type == "country") {
            country <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
          }
          
          if(type == "administrative_area_level_1"){
            admin_1 <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
          }
          if(type == "administrative_area_level_2"){
            admin_2 <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
          }
          if(type == "administrative_area_level_3"){
            admin_3 <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
          }
          if(type == "country"){
            country <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
          }
          if(type == "continent"){
            continent <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
          }
          if(type == "colloquial_area"){
            colloquial_area <- geodata[[i]]$results[[1]]$address_components[[j]]$long_name
          }
          
        }
        
        {
          if(0 < nchar(admin_3)){
            formatted_address <- paste0(formatted_address, admin_3, ", ", collapse = "")
          }
          if(0 < nchar(admin_2)){
            formatted_address <- paste0(formatted_address, admin_2, ", ", collapse = "")
          }
          if(0 < nchar(admin_1)){
            formatted_address <- paste0(formatted_address, admin_1, ", ", collapse = "")
          }
          if(0 < nchar(country)){
            formatted_address <- paste0(formatted_address, country, ", ", collapse = "")
          }
          if(0 < nchar(continent)){
            formatted_address <- paste0(continent, continent, ", ", collapse = "")
          }
          if(0 < nchar(colloquial_area)){
            formatted_address <- paste0(formatted_address, colloquial_area, ", ", collapse = "")
          }
          formatted_address <-  substr(formatted_address, 1, nchar(formatted_address)-2) 
        }
        
        # prepare output
        address_df[i, ] <- c(city, formatted_address,  continent, colloquial_area, country, admin_1, admin_2)
        #address_df[i, ] <- c(address, city, country)
      } else {
        address_df[i, ] <- NA
      }
    }
    return(address_df)
  }
  
  # ///////////////////////////////////////////////
  # MAIN FUNCTION
  # ///////////////////////////////////////////////
  
  rev_geocode_google <- function(coordinates, key) {
    # load libraries
    library(RCurl)
    # construct url for reverse geocoding
    rev_geocoding_info <- url_google_rev_geocoding(coordinates, key)
    # get data from google
    geodata_json <- getURL(rev_geocoding_info$api_call)
    # get data from json output
    geodata_df <- rev_geocoding_info[, c("lat", "lng")]
    geodata_df[, 3:9] <- get_rev_geodata_from_json_google(geodata_json)
    # return dataframe with the geodata
    return(geodata_df)
  }
}

geocoding <- function(dir.NER.dataframe.clean, dir.geocoding, api_key){
  
  '
  This function reads the NER dataframes clean, prepares the datafarame for geocoding andperforms geocoding
  
  Input
  @dir.NER.dataframe.clean Directory that will contain the NER dataframes cleaned
  @api_key Key used to access Google api
  

  The following dataframes are written in the dedicated folder:
  
  NER.df.GPE
  $ entity    : chr [1:115] "italia" "roma" ...
  $ type      : chr [1:115] "B-GPE" "B-GPE" ...
  $ title.freq: int [1:115] 35 18 ...

  NER.df.GPE.title
  $ title     : chr  "Cronaca_Sovversiva" "Cronaca_Sovversiva" "Il_Patriota" "Il_Patriota" ...
  $ entity    : chr  "colorado" "ginevra" "roma" "austria" ...
  $ type      : chr  "B-GPE" "B-GPE" "B-GPE" "B-GPE" ...
  $ title.freq: int  1 1 9 8 7 6 5 3 3 2 ...
  
  dataframe df_geocoding contains all fields retrieved by the geocoding
  
  dataframe df_geocoding_all contains all fields retrieved by the geocoding + reverse_geocoding

   '
  dir.create(file.path(dir.geocoding), showWarnings = FALSE)
  
  NER.df.grouped <- read.NER.dataframes.grouped(dir.NER.dataframe.clean)
  
  NER.df.GPE <- NULL
  #for(f in 1:length(filenames)){
  for(f in 1:length(NER.df.grouped)){
    
    df <- NER.df.grouped[[f]] %>% 
      filter(type == "B-GPE") %>% 
      arrange(desc(title.freq))
    NER.df.GPE[[length(NER.df.GPE) + 1]] <- df
    
  }
  
  names(NER.df.GPE) <- gsub("_clean_grouped", "", names(NER.df.grouped))
  
  
  NER.df.GPE.title <- rbindlist(NER.df.GPE, fill = TRUE, idcol = names(NER.df.GPE))
  colnames(NER.df.GPE.title) <- c("title", colnames(NER.df.GPE.title)[-1])
  
  gpe.file.title <- file.path(dir.geocoding, "GPE_entities_title")
  write.table(NER.df.GPE.title, file = gpe.file.title, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
  
  
  # NER.df.GPE <- rbind_list(NER.df.GPE)
  NER.df.GPE <- rbindlist(NER.df.GPE)
  
  NER.df.GPE <- NER.df.GPE %>%
    group_by(entity, type) %>%
    summarize(title.freq = sum(title.freq)) %>%
    arrange(desc(title.freq))
  
  dir.create(file.path(dir.geocoding), showWarnings = FALSE)
  
  
  gpe.file <- file.path(dir.geocoding,  "GPE_entities")
  write.table(NER.df.GPE, file = gpe.file, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
  # NER.df.GPE <- read.csv(file = gpe.file, header = TRUE, sep = "\t", quote = "", fileEncoding = "UTF8", stringsAsFactors = F)
  
  

  
  step <- 10
  length <- nrow(NER.df.GPE)
  df.geocoding <- NULL
  for(i in 1:(step + 1)){
    u <- floor(length/step)
    
    range <- (1 + (i-1)*u):(i*u)
    if(i == step + 1)
      range <- (1 + (i-1)*u):length(nrow)
    
    print(paste("Processing batch", (1 + (i-1)*u), "-", (i*u)))
    
    df <- NER.df.GPE[range, ]
    
    geocod <- geocode_google(df$entity, "all", api_key)# check results
    
    geocod_reverse <- rev_geocode_google(geocod[, c("lat", "lng")], api_key)
    # df <- cbind(df, lat = geocod$lat, lng = geocod$lng, place_id = geocod$place_id,
    #             address_ita = geocod$address, address = geocod_reverse$address,
    #             type = geocod$type, city = geocod$city)
    
    comb <- cbind(geocod, geocod_reverse)
    
    df.geocoding <- rbind(df.geocoding, comb)
    
    list.files <- list.files(file.path(dir.geocoding))
    list.files <- list.files[grepl("df_geocoding_all", list.files)]
    exist.geocoding.file <- ifelse(0 < length(list.files), FALSE, TRUE)
    
    write.table(df.geocoding, file = file.path(dir.geocoding, "df_geocoding_all"), append = TRUE, col.names = exist.geocoding.file, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
    
  }    
  
  write.table(df.geocoding, file = file.path(dir.geocoding, "df_geocoding"), col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
  
  
  
}

prepare.dataframe.sentiment <- function(dir.NER.dataframe.clean, dir.sentiment){
  
  '
  This function creates the dataframes that will be used for the sentiment analysis
  
  Input
  @dir.NER.dataframe.clean Directory that will contain the NER dataframes cleaned
  @dir.sentimetn Directory that will contain the Ndataframes for the sentiment analysis
  

  The following dataframes are written in the dedicated folder:
  df.joined
  $ entity.lower: chr  "italia" "italia" "italia" "italia" ...
  $ entity      : chr  "L ITALIA" "ITALIA" "ITALIA" "L ITALIA" ...
  $ type        : chr  "B-GPE" "B-GPE" "B-GPE" "B-GPE" ...
  $ issue       : chr  "sn85066408_1897-01-26_ed-1_seq-1_ocr.txt" "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt" "sn85066408_1903-01-02_ed-1_seq-1_ocr.txt" "sn85066408_1903-01-02_ed-1_seq-1_ocr.txt" ...
  $ date        : chr  "1897-01-26" "1915-01-01" "1903-01-02" "1903-01-02" ...
  $ sentence    : chr  "200" "1179" "383" "383" ...
  $ issue.freq  : int  1 2 2 1 10 1 1 1 4 3 ...
  $ title.freq  : int  29 29 29 29 29 29 29 29 29 29 ..
  
  df.sentence.selected
  $ index    : int  14 ...
  $ sentence : chr  "Red San Francisco"  ...
  $ separator: chr  "." ...
  $ issue    : chr  "sn85066408_1897-01-25_ed-1_seq-1_ocr.txt" ...
  $ processed: num  0 ...

   '
  
  df.entity.grouped <- read.NER.dataframes.grouped(dir.NER.dataframe.clean)
  
  df.sentence <- read.sentence.dataframes.clean(dir.NER.dataframe.clean)
  
  dir.create(file.path(dir.sentiment), showWarnings = FALSE)
  
  # most <- 10
  df.entity.selected <- NULL
  for(f in 1:length(df.entity.grouped)){
    df <- df.entity.grouped[[f]]
    most <- ceiling(log(nrow(df))) * 2
    df.selected <- NULL
    for(type in c("B-PER", "B-ORG", "B-GPE", "B-LOC")){
      df.sel <- df[df$type == type, ]
      if(0 < nrow(df.sel)){
        df.sel <- df.sel %>%
          arrange(desc(title.freq))
        df.selected <- rbind(df.selected, df.sel[1:min(most, nrow(df.sel)), ])
      }
    }
    df.entity.selected[[length(df.entity.selected) + 1]] <- df.selected
  }
  names(df.entity.selected) <- names(df.entity.grouped)
  
  df.entity.selected.title <- rbindlist(df.entity.selected, fill = TRUE, idcol = names(df.entity.grouped))
  colnames(df.entity.selected.title) <- c("title", colnames(df.entity.selected.title)[-1])
  
  
  write.table(df.entity.selected.title, file = file.path(dir.sentiment, "sentiment_entities"), col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
  
  
  df.entity <- read.NER.dataframes.clean(dir.NER.dataframe.clean)
  df.entity.grouped <- NULL
  df.joined <- NULL
  df.sentence.selected <- NULL
  for(f in 1:length(df.entity.selected)){
    
    df.selected <- df.entity.selected[[f]]
    df <- df.entity[[f]]
    joined.df <- merge(df, df.selected, by.x = "entity.lower", by.y = "entity", all.x = FALSE, all.y = FALSE) %>%
      arrange(desc(title.freq))
    joined.df <- joined.df %>%
      mutate(type = type.x) %>%
      select(-c("type.x", "type.y")) 
    joined.df <- joined.df[, c(1, 2, 8, 3, 4, 6, 5, 7)]
      
    df.joined[[length(df.joined) + 1]] <- joined.df
    
    df.sent <- df.sentence[[f]]
    sent.ind <- unlist(strsplit(as.character(joined.df$sentence), "\\s+"))
    df.sent <- df.sent[as.numeric(df.sent$index) %in% as.numeric(sent.ind), ]
    df.sentence.selected[[length(df.sentence.selected) + 1]] <- cbind(df.sent,  processed = rep(0, nrow(df.sent)))
    
    
  }
  names(df.joined) <- names(df.entity)
  names(df.sentence.selected)  <- names(df.sentence)
  
  
  dir.create(dir.sentiment)
  for(f in 1:length(df.sentence.selected)){
    write.table(df.sentence.selected[[f]], 
                file = paste(file.path(dir.sentiment, names(df.sentence.selected)[f]), "_sentences_selected", sep = ""), 
                append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
    
    write.table(df.joined[[f]], 
                file = paste(file.path(dir.sentiment, names(df.joined)[f]), "_selected", sep = ""), 
                append = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
  }
  
}

do.sentiment <- function(dir.sentiment, api_key){
  
  '
  This function performs sentiment analysis and creates the dataframes containing the results
  Input
  @dir.sentiment Directory that will contain the output dataframes
  @api_key Key used to access Google api


  The following dataframes are written in the dedicated folder:
  $ index    : int  1228
  $ sentence : chr  "Dinanzi deserto , squalido quadro disperaifone , miseria pianto può , per istante , sentire tutta tristezza delle cose della vita agli esseri sani ccrebiù tosto rassere tempesta fugata dalle nubi evanescenti serenità ritorna con essa gioia sete della vita"
  $ separator: chr  "." "." "."
  $ issue    : chr  "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt"
  $ processed: num  1
  $ magnitude: num  0.1
  $ score    : num  0.1

   '
  
  gl_auth(api_key)
  df.sentence.selected <- read.sentence.dataframes.clean.selected(dir.sentiment)
  
  batch <- 50
  
  for(f in 1:length(df.sentence.selected)){
    df <- df.sentence.selected[[f]]
    
    file.name <- paste(names(df.sentence.selected)[f], "_processed", sep = "")
    
    all.files <- list.files(file.path(dir.sentiment))
    df.processed <- NULL
    if(any(all.files == file.name)){
      df.processed <- read.csv(file = file.path(dir.sentiment, file.name), header = TRUE, sep = "\t", quote = "", fileEncoding = "UTF8", stringsAsFactors = F)
      df <- df[!df$index %in% df.processed$index, ]
    }
    
    scores <- NULL
    processed <- NULL
    if(0 < nrow(df)){
      for(i in 1:nrow(df)){
        
        sentiment <- gl_nlp(df[i, ]$sentence, nlp_type = "analyzeSentiment", type = "PLAIN_TEXT", language = "it", encodingType = "UTF8") 
        
        if(2 <= length(unlist(sentiment$sentences))){ # checl for error message
          if(1 == nrow(sentiment$sentences[[1]])){ #check if text is split in more than one sentence
            scores <- rbind(scores, sentiment$sentences[[1]][c("magnitude", "score")])
          }else{
            scores <- rbind(scores, sapply(sentiment$sentences[[1]][, c("magnitude", "score")], mean))
          }
          processed <- rbind(processed, df[i, ])
          processed.scores <- cbind(processed, scores)
        }
        
        if(i %% batch == 0 ||  i == nrow(df)){
          processed.scores$processed <- rep(1, nrow(processed.scores))
          
          colnames.b <- TRUE
          if(any(all.files == file.name)){
            colnames.b <- FALSE
          }
          write.table(processed.scores, 
                      file = file.path(dir.sentiment, file.name), 
                      append = TRUE, col.names = colnames.b, row.names = FALSE, sep = "\t", quote = FALSE, fileEncoding = "UTF8")
          
          scores <- NULL
          processed <- NULL
          
          print(paste("Processing title:", names(df.sentence.selected)[f], "   Row", nrow(df.processed) + i))
        }
      }
    }
  }
  
  
  #use a sample: for example only sentences which contein entity "United States" or random sample
}

#### Function to create the dataframes used by shiny
{
  
  create.shiny.entity.dataframe <- function(dir.NER.dataframe.clean, dir.shiny.datasets){
    
    '
  This function pcreates a datafram containing all the entities, used by the shiny app
  Input
  @dir.NER.dataframe.clean Directory that contains the the results of NER
  @dir.shiny.datasets Directory that will contain all dataframes used by the shiny app


  The following dataframes are written in the dedicated folder:
  $ title     : chr  "Cronaca Sovversiva" "Cronaca Sovversiva" "Cronaca Sovversiva" "Cronaca Sovversiva" ...
  $ type      : chr  "PER" "ORG" "ORG" "ORG" ...
  $ issue     : chr  "2012271201_1904-01-02_ed-1_seq-1_ocr.txt" "2012271201_1903-06-06_ed-1_seq-1_ocr.txt" "2012271201_1904-01-02_ed-1_seq-1_ocr.txt" "2012271201_1903-06-06_ed-1_seq-1_ocr.txt" ...
  $ date      : chr  "1904-01-02" "1903-06-06" "1904-01-02" "1903-06-06" ...
  $ issue.freq: int  1 1 1 1 1 1 1 1 1 1 ...
  $ sentence  : chr  "293" "92" "179" "125" ...
 
   '
    
    NER.df.clean <- read.NER.dataframes.clean(dir.NER.dataframe.clean)
    NER.df.all <- rbindlist(NER.df.clean, fill = TRUE, idcol = names(NER.df.clean))
    colnames(NER.df.all) <- c("title", colnames(NER.df.all)[2:ncol(NER.df.all)])
    
    NER.df.all$title <- gsub("_clean", "", NER.df.all$title)
    NER.df.all$title <- gsub("_", " ", NER.df.all$title)
    NER.df.all$type <- gsub("B-", "", NER.df.all$type)
    
    NER.df.all <- NER.df.all[, -c("entity")]
    colnames(NER.df.all)[colnames(NER.df.all) == 'entity.lower'] <- 'entity'
    
    NER.df.all <- NER.df.all %>%
      mutate(year = date) %>%
      select(-c("date"))
    dir.create(dir.shiny.datasets, showWarnings = FALSE)
    
    write.table(NER.df.all, file = file.path(dir.geocoding, "place_names.csv"), col.names = TRUE, row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF8")
    
  }
  
  create.shiny.geocoding.file <- function(dir.geocoding, dir.shiny.datasets){
    
    '
  This function creates a dataframe containing all the geocoding information, used by the shiny app
  Input
  @dir.geocoding Directory that contains the the results of geocoding
  @dir.shiny.datasets Directory that will contain all dataframes used by the shiny app
 
   '
    geo.new <- read.csv(file = file.path(dir.geocoding, "df_geocoding_all"), header = TRUE, sep = "\t", quote = "", fileEncoding = "UTF8", stringsAsFactors = F)
    
    #if the reverse geocoding fields are empry, fill them with the geocoding data
    geo.new <- geo.new %>% mutate(city.1 = case_when(
      is.null(city.1) ~ city, 
      TRUE   ~ city.1 ),
      formatted_address = case_when(
        is.na(formatted_address) ~ address, 
        TRUE   ~ formatted_address),
      country.1 = case_when(
        is.na(country.1) ~ country, 
        TRUE   ~ country.1),
      admin_1.1 = case_when(
        is.na(admin_1.1) ~ admin_1, 
        TRUE   ~ admin_1.1),
      admin_2.1 = case_when(
        is.na(admin_2.1) ~ admin_2, 
        TRUE   ~ admin_2.1),
      lat.1 = case_when(
        is.na(lat.1) ~ lat, 
        TRUE   ~ lat.1),
      lng.1 = case_when(
        is.na(lng.1) ~ lng, 
        TRUE   ~ lng.1),
      continent.1 = case_when(
        is.na(continent.1) ~ continent.1, 
        TRUE   ~ continent.1)
    )
    

    
    #if a location appears in the old geocoding file, use that data
    if(FALSE){
      geo.new$place_id <- sapply(1:nrow(geo.new), function(i){
        new <- geo.new[i, ]
        match <- geo.old[geo.old$location == new$search.query, ][1, ]
        if(nrow(match) != 0){
          return(match$placeid)
        }
        new$place_id
      })
      
      geo.new$location_type <- sapply(1:nrow(geo.new), function(i){
        new <- geo.new[i, ]
        match <- geo.old[geo.old$location == new$search.query, ][1, ]
        if(nrow(match) != 0){
          return(match$location_type)
        }
        new$location_type
      })
      
      geo.new$formatted_address <- sapply(1:nrow(geo.new), function(i){
        new <- geo.new[i, ]
        match <- geo.old[geo.old$location == new$search.query, ][1, ]
        if(nrow(match) != 0){
          return(match$formatted_address)
        }
        new$formatted_address
      })
      
      geo.new$lat.1 <- sapply(1:nrow(geo.new), function(i){
        new <- geo.new[i, ]
        match <- geo.old[geo.old$location == new$search.query, ][1, ]
        if(nrow(match) != 0 && !is.na(match$lat) && !is.na(as.numeric(match$lat))){
          return(match$lat)
        }
        new$lat.1
      })
      
      geo.new$lng.1 <- sapply(1:nrow(geo.new), function(i){
        new <- geo.new[i, ]
        match <- geo.old[geo.old$location == new$search.query, ][1, ]
        if(nrow(match) != 0 && !is.na(match$lon) && !is.na(as.numeric(match$lon))){
          return(match$lon)
        }
        new$lng.1
      })
      
      geo.new$admin_1.1 <- sapply(1:nrow(geo.new), function(i){
        new <- geo.new[i, ]
        match <- geo.old[geo.old$location == new$search.query, ][1, ]
        if(nrow(match) != 0){
          return(match$admin_1)
        }
        new$admin_1.1
      })
      
      geo.new$admin_2.1 <- sapply(1:nrow(geo.new), function(i){
        new <- geo.new[i, ]
        match <- geo.old[geo.old$location == new$search.query, ][1, ]
        if(nrow(match) != 0){
          return(match$admin_2)
        }
        new$admin_2.1
      })
      
      geo.new$country.1 <- sapply(1:nrow(geo.new), function(i){
        new <- geo.new[i, ]
        match <- geo.old[geo.old$location == new$search.query, ][1, ]
        if(nrow(match) != 0){
          return(match$country)
        }
        new$country.1
      })
      
      geo.new$continent.1 <- sapply(1:nrow(geo.new), function(i){
        new <- geo.new[i, ]
        match <- geo.old[geo.old$location == new$search.query, ][1, ]
        if(nrow(match) != 0){
          return(match$continent)
        }
        new$continent.1
      })
      
      geo.new$colloquial_area.1 <- sapply(1:nrow(geo.new), function(i){
        new <- geo.new[i, ]
        match <- geo.old[geo.old$location == new$search.query, ][1, ]
        if(nrow(match) != 0){
          return(match$colloquial_area)
        }
        new$colloquial_area.1
      })
    }
    
    
    geo.new <- geo.new %>% select(location = search.query, placeid = place_id, formatted_address = formatted_address,
                                  location_type = location_type, continent = continent.1, colloquial_area = colloquial_area.1,
                                  country = country.1, admin_1 = admin_1.1, admin_2 = admin_2.1, lat = lat.1, lon = lng.1)
    
    geo.new <- geo.new %>% filter(0 < nchar(lat) & 0 < nchar(lon) & !is.na(as.numeric(lat)) & !is.na(as.numeric(lon)))
    
    geo.new <- geo.new %>% mutate(lat = as.double(lat), lon = as.double(lon))
    
    geo.new <- geo.new %>% mutate(locality = ifelse(
      location_type == "locality+political", formatted_address,
      ""))
    
    geo.new$locality <- sapply(geo.new$locality, function(loc){
      s <- strsplit(loc, ",")[[1]][1]
      ifelse(is.na(s), "",  gsub("^\\s+|\\s+$", "", gsub("[^[:alpha:] ]", "", s)) )
    })
    
    geo.new[is.na(geo.new)] <- ""
    
    geo.new.filtered <- geo.new %>% distinct(location, .keep_all = TRUE)
    
    write.table(geo.new.filtered, file = file.path(dir.shiny.datasets, "geo_data.csv"), col.names = TRUE, row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF8")
    
    
    
    ################
    
    
    
    NER.df <- read.NER.dataframes.clean(dir.NER.dataframe.clean)
    
    for(f in 1:length(NER.df)){
      NER.df[[f]] <- NER.df[[f]] %>% 
        mutate(sentence = as.character(sentence)) %>%
        filter(type == "B-GPE") 
    }
    
    names(NER.df) <- gsub("_", " ", names(NER.df))
    
    ###### Group by issue
    NER.df.GPE <- bind_rows(NER.df,  .id = 'title')
    NER.df.GPE <- NER.df.GPE %>%
      group_by(entity.lower, type, issue) %>%
      summarize(Title = title[1], freq = issue.freq, loc.gpe = gsub("B-", "", type), date = date, year = lubridate::year(date)) %>%
      rename(location = entity.lower, filename = issue) %>%
      arrange(date)
    
    gpe.file <- file.path(dir.shiny.datasets, "place_names.csv")
    write.table(NER.df.GPE, file = gpe.file, col.names = TRUE, row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF8")
    
    
    
    
  }
  
  
  #sentiment only
  create.shiny.entity.sentiment.dataframe <- function(dir.NER.dataframe.clean, dir.shiny.datasets){
    '
  This function creates a dataframe containing all the sentiment associated to entities, used by the shiny app
  Input
  @dir.NER.dataframe.clean Directory that contains the the results of NER
  @dir.NER.dataframe.clean Directory that will contain all dataframes used by the shiny app


  The following dataframes are written in the dedicated folder:
  $ sentence  : chr [1:141] "1002" "1003" "1013" "1023" ...
  $ title     : chr [1:141] "L\'Italia" "L\'Italia" "L\'Italia" "L\'Italia" ...
  $ entity    : chr [1:141] "ettore patrizi editore" "san francisco" "generale pau" "vienna" ...
  $ type      : chr [1:141] "ORG" "GPE" "ORG" "GPE" ...
  $ issue     : chr [1:141] "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt" "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt" "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt" "sn85066408_1915-01-01_ed-1_seq-1_ocr.txt" ...
  $ date      : chr [1:141] "1915-01-01" "1915-01-01" "1915-01-01" "1915-01-01" ...
  $ issue.freq: int [1:141] 1 1 2 1 4 2 3 2 10 2 ...
  $ timestamp : Date[1:141], format: "1915-01-01" "1915-01-01" "1915-01-01" "1915-01-01" ...
  $ processed : chr [1:141] "1" "1" "1" "1" ...
  $ magnitude : chr [1:141] "0.2" "0.1" "0" "0.3" ...
  $ score     : chr [1:141] "0.2" "0.1" "0" "-0.3" ...
  $ id        : chr [1:141] "ettore patrizi editore - ORG" "san francisco - GPE" "generale pau - ORG" "vienna - GPE" ...
 
   '
    
    df.sentence <- read.sentence.dataframes.clean.processed(dir.sentiment)
    
    #df.entity <- read.NER.dataframes.clean(dir.sentiment)
    df.entity <- read.NER.dataframes.clean(dir.NER.dataframe.clean)
    
    df.entity.all <- rbindlist(df.entity, fill = TRUE, idcol = names(df.entity))
    names(df.entity.all) <- c("title", names(df.entity.all)[2:ncol(df.entity.all)])
    # df.entity.all <- df.entity.all[, -c("type.y")]
    # names(df.entity.all)[names(df.entity.all) == "type.x"] <- "type"
    
    df.sentence.all <- rbindlist(df.sentence, fill = TRUE, idcol = names(df.sentence))
    names(df.sentence.all) <- c("title", names(df.sentence.all)[2:ncol(df.sentence.all)])
    
    df.entity.all <- separate_rows(df.entity.all, sentence, sep = "\\s+")
    df.entity.all$timestamp <- as.Date(str_extract(df.entity.all$issue, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))
    
    #   df.entity.all$score <-  sapply(df.entity.all$sentence, function(se){
    #     se.index <- unlist(strsplit(se, "\\s+"))
    #     mean(as.numeric(df.sentence.all[df.sentence.all$index %in% se.index, ]$score))
    #   })
    
    df.all <- merge(df.entity.all, df.sentence.all, by.x = c("sentence"), by.y = c("index"), allow.cartesian = TRUE, all.x = FALSE)
    df.all <- df.all[df.all$issue.x == df.all$issue.y, ]
    
    df.all <- df.all[, -c("title.y", "issue.y", "sentence.y", "separator")]
    names(df.all)[names(df.all) == "title.x"] <- "title"
    names(df.all)[names(df.all) == "issue.x"] <- "issue"
    
    df.all$title <- gsub("_clean", "", df.all$title)
    df.all$title <- gsub("_", " ", df.all$title)    
    df.all$type <- gsub("B-", "", df.all$type)
    
    
    df.all <- df.all %>%
      mutate(entity = entity.lower, year = date) %>%
      select(-c("entity.lower", "date")) %>%
      dplyr::group_by(entity, type) %>%
      dplyr::filter(1 < sum(issue.freq))
    
    df.all <- within(df.all, id <- paste(entity, type, sep= ' - '))    
    
    
    write.table(df.all, file = file.path(dir.shiny.datasets, "entities_sentiment.csv"), col.names = TRUE, row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF8")
    
  }
  
  create.shiny.network.file <- function(dir.NER.dataframe.clean, dir.sentiment, dir.shiny.datasets, filter.frequency = 0){
    
    '
  This function creates the fie containing the network used for Social Network ANalysis
  Input
  @dir.NER.dataframe.clean Directory that contains the the results of NER
  @dir.sentiment Directory that contains the the results of Sentiment Analysis
  @dir.shiny.datasets Directory that will contain all dataframes used by the shiny app
  @filter.frequency entities that appear, in the network, a number of times lower than this value will be filtered out. Set to 0 to ignore this
   '
    
    df.sentence <- read.sentence.dataframes.clean.processed(dir.sentiment)
    
    #df.entity <- read.NER.dataframes.clean(dir.sentiment)
    df.entity <- read.NER.dataframes.clean(dir.NER.dataframe.clean)
    
    df.entity.all <- rbindlist(df.entity, fill = TRUE, idcol = names(df.entity))
    names(df.entity.all) <- c("title", names(df.entity.all)[2:ncol(df.entity.all)])
    # df.entity.all <- df.entity.all[, -c("type.y")]
    # names(df.entity.all)[names(df.entity.all) == "type.x"] <- "type"
    
    df.sentence.all <- rbindlist(df.sentence, fill = TRUE, idcol = names(df.sentence))
    names(df.sentence.all) <- c("title", names(df.sentence.all)[2:ncol(df.sentence.all)])
    
    df.entity.all <- separate_rows(df.entity.all, sentence, sep = "\\s+")
    df.entity.all$timestamp <- as.Date(str_extract(df.entity.all$issue, "[0-9]{4}-[0-9]{2}-[0-9]{2}"))
    
  
    df.all <- merge(df.entity.all, df.sentence.all, by.x = c("sentence"), by.y = c("index"), allow.cartesian = TRUE, all.x = FALSE)
    df.all <- df.all[df.all$issue.x == df.all$issue.y, ]
    
    df.all <- df.all[, -c("title.y", "issue.y", "sentence.y")]
    names(df.all)[names(df.all) == "title.x"] <- "title"
    names(df.all)[names(df.all) == "issue.x"] <- "issue"
    
    df.all$title <- gsub("_clean", "", df.all$title)
    df.all$title <- gsub("_", " ", df.all$title)    
    df.all$type <- gsub("B-", "", df.all$type)
    
    
    df.all <- df.all %>%
      dplyr::group_by(entity.lower, type) %>%
      dplyr::filter(filter.frequency < sum(issue.freq))
    
    df.all <- within(df.all, id <- paste(entity.lower, type, sep= ' - '))    
    
    
    # write.table(df.all, file = file.path(dir.shiny.datasets, "entities_sentiment.csv"), col.names = TRUE, row.names = FALSE, sep = ";", quote = FALSE, fileEncoding = "UTF8")
    
    
    nodes.info <- df.all %>%
      dplyr::group_by(id, entity.lower, type) %>%
      # dplyr::filter(1 < n()) %>%
      dplyr::summarize(n = n())
    # %>%
    #   arrange(desc(n.issue))
    
    #### n.title for node too
    
    collab.network <- graph.empty(n=0)
    collab.network <- collab.network + vertex(nodes.info$id,
                                              id = nodes.info$id,
                                              entity = nodes.info$entity.lower,
                                              type = nodes.info$type
                                              #n = nodes.info$n#,
                                              # titles = nodes.info$titles,
                                              # n.issue.unique = nodes.info$n.issue.unique,
                                              # n.issue = nodes.info$n.issue,
                                              # n.sentence.unique = nodes.info$n.sentence.unique,
                                              # type = nodes.info$type,
                                              # sentiment.score = nodes.info$sentiment.score,
                                              # sentiment.magnitude = nodes.info$sentiment.magnitude,
                                              # start.date = nodes.info$start.date,
                                              # end.date = nodes.info$end.date
    )
    
    entity.collab <- merge(df.all, df.all, by = c("sentence"), allow.cartesian=TRUE)
    entity.collab <- entity.collab[entity.collab$issue.x == entity.collab$issue.y, ]
    
    # entity.collab <- entity.collab %>% distinct(sentence, entity.lower.x, entity.lower.y, .keep_all = TRUE)
    
    #allow self loops to preserve lone entities in a sentence
    # entity.collab <- entity.collab[entity.collab$id.x != entity.collab$id.y, ]
    
    edges.info <- tibble(id.x = entity.collab$id.x, id.y = entity.collab$id.y,
                         entity.x = entity.collab$entity.lower.x, entity.y = entity.collab$entity.lower.y, 
                         type.x = entity.collab$type.x, type.y = entity.collab$type.y,
                         title = entity.collab$title.x, issue = entity.collab$issue.x, sentence = entity.collab$sentence,
                         timestamp = entity.collab$timestamp.x, year = entity.collab$date.x, 
                         magnitude = entity.collab$magnitude.x, score = entity.collab$score.x)
    
    edges.info <- edges.info %>%
      dplyr::group_by(sentence, id.x, id.y) %>%
      # dplyr::filter(1 < n()) %>%
      mutate(sentence.freq = n())
    
    ####ADD EDGES
    
    collab.network <- collab.network + edge(c(t(edges.info[, c(1,2)])),
                                            entity.x = edges.info$entity.x, entity.y = edges.info$entity.y, 
                                            title = edges.info$title,
                                            issue = edges.info$issue,
                                            sentence = edges.info$sentence,
                                            timestamp = edges.info$timestamp, year = edges.info$year,
                                            sentence.freq = edges.info$sentence.freq, 
                                            magnitude = edges.info$magnitude, 
                                            score = edges.info$score
    )
    #}
    
    
    if(FALSE){
      end <- ends(collab.network, E(collab.network))
      
      E(collab.network)$n.issue.unique <- sapply(1:ecount(collab.network),  function(e){
        v1 <- V(collab.network)[end[e, 1]]
        v2 <- V(collab.network)[end[e, 2]]
        
        E(collab.network)$n.issue.unique[get.edge.ids(collab.network, c(v1, v2))]/(v1$n.issue.unique + v2$n.issue.unique)
      })
      
      E(collab.network)$n.sentence.unique <- apply(ends(collab.network, V(collab.network)), 1,  function(e){
        v1 <- V(collab.network)[e[1]]
        v2 <- V(collab.network)[e[2]]
        
        E(collab.network)$n.sentence.unique[get.edge.ids(collab.network, c(v1, v2))]/(v1$n.sentence.unique + v2$n.sentence.unique)
      })
      
      E(collab.network)$duration <- apply(ends(collab.network, V(collab.network)), 1,  function(e){
        v1 <- V(collab.network)[e[1]]
        v2 <- V(collab.network)[e[2]]
        
        E(collab.network)$collab.duration[get.edge.ids(collab.network, c(v1, v2))]/(v1$duration + v2$duration)
      })
    }
    
    
    
    
    collab.network <- as.undirected(collab.network, mode = "mutual", edge.attr.comb = "first")
    
    # collab.network <- igraph::simplify(collab.network, remove.multiple = TRUE, remove.loops = TRUE,
    #                                    edge.attr.comb = igraph_opt("edge.attr.comb"))
    
    gclust <- clusters(collab.network, mode='strong')
    gc <- induced.subgraph(collab.network, V(collab.network)[which(gclust$membership == which.max(gclust$csize))])
    
    
    dir.create(dir.SNA, showWarnings = FALSE)
    
    write_graph(collab.network, paste(dir.shiny.datasets, paste("network"), sep = "/") , format = "graphml")
    
    
    
    # gc <- subgraph.edges(collab.network, E(collab.network)[issue == "sn85054967_1917-03-17_ed-1_seq-5_ocr.txt"])
  }
  
  
}




#Run this code to perform all steps
{
#set the home directory
setwd(home.dir)

#First step: read the corpus
corpus <- read.corpus(dir.corpus)

# Corpus preprocessing
corpus.cleaned <- clean.corpus(corpus, dir.corpus.cleaned,
                               lower.case = FALSE,
                               remove.punctuation = TRUE, 
                               remove.digits = TRUE, 
                               remove.non.alphanum = TRUE,
                               remove.stopwords = FALSE,
                               n.char.filter = 2,
                               freq.token.filter = 0)

# Prepare the dataframs that will be used as input by the NER algorithm
write.files.NER(corpus.cleaned, dir.NER)

#Based on the output of the NER, prepare the dataframes that will be used for sentiment, geocoding and SNA
create.NER.dataframe(dir.NER.tagged, dir.NER.dataframe)

#Process NER dataframes to handle mistakes and exceptions
process.dataframes.NER(dir.NER.dataframe, dir.NER.dataframe.clean, title.freq.filter = 0)

# Perform geocoding
geocoding(dir.NER.dataframe.clean, dir.geocoding, API_KEY_GEOCODING)

#Perform Sentiment Analysis
prepare.dataframe.sentiment(dir.NER.dataframe.clean, dir.sentiment)
do.sentiment(dir.sentiment, API_KEY_SENTIMENT_C2DH)

#Create files used by the shiny app
{
# Create entity files
create.shiny.entity.dataframe(dir.NER.dataframe.clean, dir.shiny.datasets)
create.shiny.entity.sentiment.dataframe(dir.NER.dataframe.clean, dir.shiny.datasets)

# Create geocoding dataframes
create.shiny.geocoding.file(dir.geocoding, dir.shiny.datasets)

#Create network file for Social Network Analysis
create.shiny.network.file(dir.NER.dataframe.clean, dir.sentiment, dir.shiny.datasets, filter.frequency = 0)

}
}
