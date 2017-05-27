# Load the dataset
sample.df <-
  read.csv("./data/consol_with_custid.csv", stringsAsFactors = F) %>%
  dplyr::filter(type == 'train') %>%
  dplyr::sample_n(1050)

#partition the dataset
part.size <- 500
part.marker <- ceiling(nrow(sample.df) / part.size)

sample.df %<>%
  dplyr::mutate(part = rep(1:part.marker, part.size, length.out = nrow(sample.df)))


# create the list of data frames
list.df <- sample.df %>%
  dplyr::group_by(part) %>%
  dplyr::do(data = data.frame(.)) %>%
  dplyr::select(data) %>%
  purrr::map(identity)


# init the cluster using doMC
library(doMC)
doMC::registerDoMC(8)

# init coreNLP (assume already downloaded files!)
coreNLP::initCoreNLP()

# function to parse the nouns and wh- POS tags
parse.nouns.and.wh <- function(r) {
  ann.q1 <- coreNLP::annotateString(r$question1)$token
  ann.q2 <- coreNLP::annotateString(r$question2)$token
  
  nouns.q1 <- ann.q1 %>%
    dplyr::filter(stringr::str_detect(POS, '^N')) %>%
    magrittr::extract2('lemma')
  
  nouns.q2 <- ann.q2 %>%
    dplyr::filter(stringr::str_detect(POS, '^N')) %>%
    magrittr::extract2('lemma')
  
  wh.q1 <- ann.q1 %>%
    dplyr::filter(stringr::str_detect(POS, '^W')) %>%
    magrittr::extract2('lemma') %>%
    union(ifelse(
      stringr::str_detect(r$question1, "(^[Ss]hould)|([.;:?,] *[Ss]hould)"),
      "should",
      NA
    )) %>%
    union(ifelse(
      stringr::str_detect(r$question1, "(^[Ii]s)|([.;:?,] *[Ii]s)"),
      "is",
      NA
    )) %>%
    na.omit()
  
  wh.q2 <- ann.q2 %>%
    dplyr::filter(stringr::str_detect(POS, '^W')) %>%
    magrittr::extract2('lemma') %>%
    union(ifelse(
      stringr::str_detect(r$question2, "(^[Ss]hould)|([.;:?,] *[Ss]hould)"),
      "should",
      NA
    )) %>%
    union(ifelse(
      stringr::str_detect(r$question2, "(^[Ii]s)|([.;:?,] *[Ii]s)"),
      "is",
      NA
    )) %>%
    na.omit()
  
  data.frame(r, stringsAsFactors = F) %>%
    dplyr::mutate(
      q1.nouns = length(nouns.q1),
      q2.nouns = length(nouns.q2),
      q1.nouns.uniq = length(unique(nouns.q1)),
      q2.nouns.uniq = length(unique(nouns.q2)),
      common.nouns = length(intersect(nouns.q1, nouns.q2)),
      q1.wh = length(wh.q1),
      q2.wh = length(wh.q2),
      q1.wh.uniq = length(unique(wh.q1)),
      q2.wh.uniq = length(unique(wh.q2)),
      common.wh = length(intersect(wh.q1, wh.q2))
    )
}


# Process the data
foreach(df = list.df$data) %dopar% {
  g <-  df$part[1]
  sprintf("%s Processing group %s (Total rows: %s)",
          lubridate::now(),
          g,
          nrow(df)) %>%
    print()
  
  df %>%
    dplyr::rowwise() %>%
    dplyr::do(parse.nouns.and.wh(.)) %>%
    write.csv(sprintf("./results/nouns_and_wh/nouns_and_wh_group_%s.csv", g), row.names = F)
}
