Bastuklubben
================

Getting data
------------

``` r
temp <- list.files(pattern = "*.html")
myfiles <- lapply(temp, read.delim)

read_files <- function(x) {
  
  myfiles[[x]]
}

s<-seq(1:34)

bastuklubben <- map_dfr(s, read_files)

# names(bastuklubben)

# byt = c( "Ã…", "Ã„", "Ã–", "Ã¥", "Ã¤", "Ã¶" )
# med = c( "Å", "Ä", "Ö", "å", "ä", "ö" )

names <- "^arian jafari$|^Tobias $|^Bingo.Bosse $|^Axel PÃ¥lsson$|^Andreas FagerstrÃ¶m$|^Victor JÃ¤rnberg$|^Pavarotti $|^Jesper $|^Jesper S$"


bastuklubben <- bastuklubben %>%
  rename(words = X..DOCTYPE.html.) %>%
  mutate(words = as.character(words)) %>% 
  filter(!grepl(">$", words), !grepl("^[0-9]{2}:[0-9]{2}$", words),
         !grepl("^aj$|^AP$|^AF$|^J$|^T$|^JS$|^B$|^VJ$|^P$", words)) %>% 
  slice(-1:-4) %>% 
  mutate(lines = str_detect(pattern = names, words)) %>% 
  mutate(lines = ifelse(lines == TRUE, words, lines))

for (i in 1:length(bastuklubben$lines)){
  if (bastuklubben$lines[i] == FALSE){
    bastuklubben$lines[i] = bastuklubben$lines[i-1]
  }
}

bastuklubben <- bastuklubben %>% 
  filter(words != lines) %>% 
  rename(user = lines, message = words)
```

Testing
-------

``` r
# Mean length of message
bastuklubben %>% 
  mutate(length = nchar(message)) %>% 
  group_by(user) %>%
  summarise(mean_length_of_message = mean(length)) %>%
  arrange(-mean_length_of_message) %>% 
  kable()
```

| user                |  mean\_length\_of\_message|
|:--------------------|--------------------------:|
| Jesper S            |                   43.04728|
| Axel PÃ¥lsson       |                   41.38983|
| Jesper              |                   40.25708|
| Pavarotti           |                   37.96318|
| arian jafari        |                   37.04833|
| Victor JÃ¤rnberg    |                   31.02853|
| Bingo-Bosse         |                   29.48079|
| Tobias              |                   27.69263|
| Andreas FagerstrÃ¶m |                   24.13151|
