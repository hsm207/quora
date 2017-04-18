library(magrittr)
library(foreach)



files <- list.files("./results/sim_measures/")
n <- ceiling(length(files) / 4)

files.list <- split(files, as.integer(seq_along(files) / n))

doMC::registerDoMC(4)


res <-
  foreach(f = files.list, .combine = dplyr::bind_rows) %dopar% {
    path <- "./results/sim_measures/"
    coltypes <- c("character", "character") %>%
      magrittr::set_names(c("qid1", "qid2"))
    df1 <-
      read.csv(paste0(path, f[1]),
               stringsAsFactors = F,
               colClasses = coltypes)
    
    res <- purrr::reduce(f[-1], function(df, f)
      dplyr::bind_rows(
        df,
        read.csv(
          paste0(path, f),
          stringsAsFactors = F,
          colClasses = coltypes
        )
      ), .init = df1)
    
  }

res %>%
  write.csv("./results/consolidation/consol_sim_measures.csv",
            row.names = F)
print(
  sprintf(
    "Done consolidating. Dimension of consolidated data frame is %s x %s",
    dim(res)[1],
    dim(res)[2]
  )
)
