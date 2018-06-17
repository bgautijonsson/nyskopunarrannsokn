add_surv <- function(df, n_fail = "n_fail", n_risk = "n_risk") {
      out <- vector("numeric", nrow(df))
      out[1] <- 1 - df[[n_fail]][1] / df[[n_risk]][1]
      for (i in seq(2, nrow(df))) {
            out[i] <- 1 - df[[n_fail]][i] / df[[n_risk]][i]
            out[i] <- out[i] * out[i - 1]
      }
      
      df %>% mutate(surv = out)
}

surv_plot <- function(variable, min_num = 100) {
      df %>% 
            filter(!grepl("sjúkrahúss", afdrif),
                   !id %in% c(6028, 775)) %>% 
            select(id, dags_inn, dags_ut, timi_milli, censor, variable) %>% 
            rename(fail_time = timi_milli) %>% 
            arrange(fail_time, id) %>%
            group_by_(variable) %>%
            mutate(n = n()) %>%
            filter(n > min_num) %>%
            group_by_("fail_time", "censor", "n", variable) %>%
            summarize(num_fail = n()) %>%
            ungroup %>%
            spread(censor, num_fail, fill = 0) %>%
            set_names(c("time", "n_risk", variable, "n_fail", "n_censor")) %>%
            select(time, n_fail, n_censor, n_risk, variable) %>%
            group_by_(variable) %>%
            mutate(total_fail = cumsum(n_fail),
                   total_censor = cumsum(n_censor),
                   n_risk = n_risk - cumsum(lag(n_censor, default = 0)) - cumsum(lag(n_fail, default = 0))) %>%
            nest %>%
            mutate(data = map(data, add_surv)) %>%
            unnest %>%
            ggplot(aes_string("time", "surv", col = variable)) +
            geom_line() +
            scale_y_continuous(labels = scales::percent) +
            theme(legend.position = "top") +
            labs(x = "Dagar síðan útskrifaðist",
                 y = "Hlutfall sjúklinga sem eiga eftir að leggjast aftur inn",
                 title = "Lifunarmyndrit",
                 subtitle = str_to_title(variable))
}
surv_plot("kringumstaedur", min_num = 100)


