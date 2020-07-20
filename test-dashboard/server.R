# -- Set up
source("init.R")

shinyServer(function(input, output, session) {
    
    
    # -- This is used to print table in app
    output$tabla <- DT::renderDataTable(DT::datatable({
        z <- qnorm(0.995)
        ret <- tests %>%
          mutate(rate = paste0(format(round(100*rate,1), nsmall=1),"%"),
                 avg_7_day = paste0(format(round(100*expit(fit), 1), nsmall=1),"%"),
                 lower_ci = paste0(format(round(100*expit(fit - z*se), 1), nsmall=1),"%"),
                 upper_ci = paste0(format(round(100*expit(fit + z*se), 1), nsmall=1),"%"))%>%
          select(date, positives, tests, rate, avg_7_day, lower_ci, upper_ci) %>%
          arrange(desc(date)) %>%
          filter(date >= input$range[1], date <= input$range[2]) %>%
          mutate(date = format(date, "%B %d")) %>%
          setNames(c("Fecha", "Positivos", "Pruebas", "Tasa", "Promedio 7 dias", "LCI", "UCI"))
        return(ret)
    }), 
    rownames= FALSE,
    options = list(dom = 't', pageLength = -1))
    
    # -- This creates the positivity rate figure
    output$tasa_positividad <- renderPlot({
      
       tests %>%
         filter(date >= input$range[1], date <= input$range[2]) %>%
         ggplot(aes(date, rate)) +
         geom_hline(yintercept = 0.05, lty=2, color="gray") +
         geom_point(aes(date, rate), size=2, alpha=0.40) +
         geom_ribbon(aes(ymin= expit(fit - z*se), ymax = expit(fit + z*se)), alpha=0.20) +
         geom_line(aes(y = expit(fit)), color="blue2", size=0.80) +
         ylab("Tasa de positividad") +
         xlab("Fecha") +
         scale_y_continuous(labels = scales::percent) +
         coord_cartesian(ylim = c(0, 0.25)) +
         scale_x_date(date_labels = "%B %d") +
         theme_bw()
    })
    
    # -- This creates the positivity rate figure per agegroup
    output$tasa_positividad_edad <- renderPlot({
      
      the_breaks <- c(0, 10, 20, 40, 60, 80, Inf)
      age_tests <- tests_by_strata %>%
        filter(date >= input$range[1], date <= input$range[2]) %>%
        group_by(date, ageRange) %>%
        dplyr::summarize(positives = sum(positives),
                         tests     = sum(tests)) %>%
        ungroup() %>%
        separate(ageRange, c("age_lower", "age_upper"), sep = " to ", remove = FALSE) %>%
        mutate(age_lower = as.numeric(age_lower),
               age_upper = as.numeric(age_upper),
               agegroup  = cut(age_lower, the_breaks, right = FALSE, labels = paste(the_breaks[-length(the_breaks)], c(the_breaks[-1]-1), sep="-")),
               agegroup  = factor(agegroup)) %>%
        na.omit() %>%
        group_by(date, agegroup) %>%
        dplyr::summarize(positives = sum(positives),
                         tests     = sum(tests)) %>%
        ungroup()
      
      age_tests <- map_df(levels(age_tests$agegroup), function(x){
        
        tmp_dat <- filter(age_tests, agegroup == x)
        
        # -- Extracting variables for model fit
        x <- as.numeric(tmp_dat$date)
        y <- tmp_dat$positives
        n <- tmp_dat$tests
        
        # -- Design matrix for splines
        df  <- round(3 * nrow(tmp_dat)/30)
        x_s <- ns(x, df = df, intercept = FALSE)
        i_s <- c(1:(ncol(x_s)+1))
        
        # -- Design matrix for weekday effect
        w            <- factor(wday(tmp_dat$date))
        contrasts(w) <- contr.sum(length(levels(w)), contrasts = TRUE)
        x_w          <- model.matrix(~w)
        
        # -- Design matrix
        X <- cbind(x_s, x_w)
        
        # -- Fitting model 
        fit  <- glm(cbind(y, n-y) ~ -1 + X, family = "quasibinomial")
        beta <- coef(fit)
        
        # -- Computing probabilities
        tmp_dat$fit  <- as.vector(X[, i_s] %*% beta[i_s])
        tmp_dat$se  <- sqrt(diag(X[, i_s] %*%
                                   summary(fit)$cov.scaled[i_s, i_s] %*%
                                   t(X[, i_s])))
        
        return(tmp_dat)
      })
      
      age_tests %>%
        mutate(rate = positives / tests,
               agegroup = case_when(agegroup == "0-9" ~ "0 a 9 años",
                                    agegroup == "10-19" ~ "10 a 19 años",
                                    agegroup == "20-39" ~ "20 a 39 años",
                                    agegroup == "40-59" ~ "40 a 59 años",
                                    agegroup == "60-79" ~ "60 a 79 años",
                                    agegroup == "80-Inf" ~ "80 años o más",
                                    TRUE ~ "other")) %>%
        ggplot(aes(date, rate)) +
        geom_hline(yintercept = 0.05, lty=2, color="gray") +
        geom_point(aes(date, rate), size=1.5, alpha=0.40) +
        geom_ribbon(aes(ymin= expit(fit - z*se), ymax = expit(fit + z*se)), alpha=0.20) +
        geom_line(aes(y = expit(fit)), color="blue2", size=0.80) +
        ylab("Tasa de positividad") +
        xlab("Fecha") +
        # ggtitle("Tasa de Positividad en Puerto Rico") +
        scale_y_continuous(labels = scales::percent) +
        coord_cartesian(ylim = c(0, 0.25)) +
        scale_x_date(date_labels = "%B %d") +
        facet_wrap(~agegroup) +
        theme_bw()
      
    })
    
    # -- This creates the daily number of tests figure
    output$numero_pruebas <- renderPlot({
        
        tests %>% 
        group_by(date = ceiling_date(date, unit = "week", 
                                     week_start = wday(max(date)))) %>%
        dplyr::summarize(tests = sum(tests)) %>%
        filter(date >= input$range[1], date <= input$range[2]) %>%
        ggplot(aes(date, tests)) +
        geom_bar(color="black", fill="#252525", size=0.20, stat = "identity") +
        ggtitle("Número de Pruebas Semanales en Puerto Rico") +
        ylab("Número de pruebas") +
        xlab("Semana acabando en esta fecha") +
        scale_y_continuous(labels = scales::comma,
                           breaks = seq(0, 30000, by = 5000)) +
        scale_x_date(date_labels = "%B %d") +
        theme_bw()
        # ggplotly(displayModeBar = FALSE)
    
    })
    
    # -- This creates the daily number of tests per agegroup figure
    output$numero_pruebas_edad <- renderPlot({
      
      the_breaks <- c(0, 10, 20, 40, 60, 80, Inf)
      tests_by_strata %>%
        filter(date >= input$range[1], date <= input$range[2]) %>%
        group_by(date, ageRange) %>%
        dplyr::summarize(tests = sum(tests)) %>%
        ungroup() %>%
        separate(ageRange, c("age_lower", "age_upper"), sep = " to ", remove = FALSE) %>%
        mutate(age_lower = as.numeric(age_lower),
               age_upper = as.numeric(age_upper),
               agegroup  = cut(age_lower, the_breaks, right = FALSE, labels = paste(the_breaks[-length(the_breaks)], c(the_breaks[-1]-1), sep="-")),
               agegroup  = factor(agegroup)) %>%
        na.omit() %>%
        group_by(date = ceiling_date(date, unit = "week", 
                                     week_start = wday(max(date))), agegroup) %>%
        dplyr::summarize(tests = sum(tests)) %>%
        ungroup() %>%
        mutate(agegroup = case_when(agegroup == "0-9" ~ "0 a 9 años",
                                    agegroup == "10-19" ~ "10 a 19 años",
                                    agegroup == "20-39" ~ "20 a 39 años",
                                    agegroup == "40-59" ~ "40 a 59 años",
                                    agegroup == "60-79" ~ "60 a 79 años",
                                    agegroup == "80-Inf" ~ "80 años o más",
                                    TRUE ~ "other")) %>%
        ggplot(aes(date, tests)) +
        geom_bar(color="black", fill="#252525", size=0.20, stat = "identity") +
        ylab("Número de pruebas") +
        xlab("Semana acabando en esta fecha") +
        scale_y_continuous(labels = scales::comma,
                           breaks = seq(0, 130000, by = 2000)) +
        coord_cartesian(ylim = c(0, 13000)) +
        scale_x_date(date_labels = "%B %d") +
        facet_wrap(~agegroup) +
        theme_bw()
      # ggplotly(displayModeBar = FALSE)
      
    })
    
    # -- This creates the positivity rate map by municipio
    output$mapa_positividad <- renderPlot({
      
      municipio_tests <- tests_by_strata %>%
        group_by(date, patientCity) %>%
        dplyr::summarize(positives = sum(positives),
                         tests     = sum(tests)) %>%
        ungroup() %>%
        filter(date >= input$range[1], date <= input$range[2]) %>%
        group_by(patientCity) %>%
        dplyr::summarize(positives  = sum(positives),
                         tests      = sum(tests),
                         rate       = positives / tests) %>%
        ungroup() %>%
        mutate(rate = pmax(0.25/1000, rate)) %>%
        na.omit() %>%
        mutate(lwr  = 100 * qbinom(alpha/2, tests, rate) / tests,
               upr  = 100 * qbinom(1 - alpha/2, tests, rate) / tests, 
               rate = 100 * rate)
      
      municipio_tests %>%
        {merge(map, .,by.x = "ADM1_ES", by.y = "patientCity", all.y = T)} %>%
        ggplot() +
        geom_sf(aes(fill = rate), color="black", size=0.15) +
        geom_text(data = map, aes(X, Y, label = ADM1_ES),
                  size  = 2.2,
                  color = "black",
                  fontface = "bold") +
        scale_fill_gradient2(low  = "#2171b5", 
                             high = "#cb181d", 
                             mid  = "white", 
                             name = "Tasa de Positividad:",
                             midpoint = 5) +
        theme_void() +
        theme(legend.position = "bottom")
      
      
      
    })
    
    # -- This creates the tests by 1000 people map by municipio
    output$mapa_pruebas <- renderPlot({
      
      municipio_tests <- tests_by_strata %>%
        group_by(patientCity) %>%
        dplyr::summarize(tests = sum(tests)) %>%
        ungroup() %>%
        left_join(pop, by = "patientCity") %>%
        na.omit() %>%
        mutate(rate = 1000 * tests / pop, 
               rate_cat = cut(rate, c(0, 50, 100, 150, 200, 250, 300)))
      
      municipio_tests %>%
        {merge(map, .,by.x = "ADM1_ES", by.y = "patientCity", all.y = T)} %>%
        ggplot() +
        geom_sf(data = map, fill="gray", size=0.15) +
        geom_sf(aes(fill = rate_cat), color="black", size=0.15) +
        geom_text(data = map, aes(X, Y, label = ADM1_ES),
                  size  = 2.2,
                  color = "black",
                  fontface = "bold") +
        scale_fill_brewer(name = "Número de pruebas \npor cada 1,000 personas",
                          palette = "Blues", 
                          type="div", 
                          drop = F) +
        theme_void() +
        theme(legend.position = "bottom")
    })
    
    # -- This allows users to download data
    output$downloadData <- downloadHandler(
        filename = function() {
            load("rdas/all_tests.rda")
            paste0("pruebas-",attr(all_tests,"date"),".csv")
        },
        content = function(file) {
            load("rdas/all_tests.rda")
            write.csv(all_tests, file, row.names = FALSE)  
        }
    )
})
