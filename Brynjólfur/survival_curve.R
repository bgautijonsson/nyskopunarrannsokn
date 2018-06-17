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
            set_names(c("time", "total", variable, "fail", "censor")) %>%
            select(time, fail, censor, total, variable) %>%
            group_by_(variable) %>%
            mutate(total_fail = cumsum(fail),
                   total_censor = cumsum(censor),
                   total = total - cumsum(lag(censor, default = 0)),
                   s = 1 - total_fail / total) %>%
            ggplot(aes_string("time", "s", col = variable)) +
            geom_line() +
            scale_y_continuous(labels = scales::percent) +
            theme(legend.position = "top") +
            labs(x = "Dagar síðan útskrifaðist",
                 y = "Hlutfall sjúklinga sem eiga eftir að leggjast aftur inn",
                 title = "Lifunarmyndrit",
                 subttle = str_to_title(variable))
}
surv_plot("postnr", min_num = 100)
