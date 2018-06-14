df <- df %>% select(id, grein)

impute_func <- function(list_dat) {
      len <- length(list_dat)
      if (len < 15) {
            list_dat <- c(list_dat, rep(NA, 15 - len))
            list_dat <- data_frame(nr_grein = factor(paste0("Grein ", 1:15), levels = c(paste0("Grein ", 1:15))), 
                                   kodi = as.character(list_dat))
      } else(
            list_dat <- data_frame(nr_grein = 1:15, kodi = as.character(list_dat))
      )
      list_dat
}

df %>%
      sample_n(100) %>%
      mutate(kodar = str_match_all(grein, "[A-Z][0-9]{2}[\\.]{0,1}[0-9]{0,1}")) %>%
      mutate(kodar = map(kodar, impute_func)) %>%
      unnest %>%
      spread(nr_grein, kodi)
