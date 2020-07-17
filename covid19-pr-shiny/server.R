# -- Set up
source("init.R")

# -- Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    

    ntext <- eventReactive(input$do, {
        "Hello World"
    })
    output$nText <- renderText({
        ntext()
    })
    
    # -- This is used to print table in app
    output$table <- DT::renderDataTable(DT::datatable({
       
        if(input$do == 0)
        {
            return(dat)
        } else{
            return(update_data())
        }
    }))
    
    # -- Used to download data when the use wants to
    update_data <- eventReactive(input$do, {
        
        # -- Getting url
        url <- "https://bioportal.salud.gov.pr/api/administration/reports/minimal-info-unique-tests"

        # -- From JSON to dataframe 
        dat <- jsonlite::fromJSON(url)

        # -- Wrangle
        dat <- dat %>%  
            as_tibble() %>%
            mutate(ageRange       = gsub(" to ", "-", ageRange),
                   collectedDate  = mdy(collectedDate),
                   reportedDate   = mdy(reportedDate),
                   createdAt      = mdy_hm(createdAt),
                   ageRange       = na_if(ageRange, "N/A"),
                   result         = tolower(result),
                   result         = case_when(grepl("positive", result) ~ "positive",
                                              grepl("negative", result) ~ "negative",
                                              result == "not detected" ~ "negative",
                                              TRUE ~ "other")) %>%
            arrange(reportedDate, collectedDate) 
        
        # -- Impute missing dates
        dat <- dat %>% mutate(date = if_else(is.na(collectedDate), reportedDate - days(2),  collectedDate))
        
        # -- Remove inconsistent dates
        dat <- dat %>%
            mutate(date = if_else(year(date) != 2020 | date > today(), reportedDate - days(2),  date)) %>%
            filter(year(date) == 2020 & date <= today()) %>%
            arrange(date, reportedDate)
        
        # -- Saving things
        save(dat, file = "pruebas-pr.rda", compress = "xz")
        return(dat)
    })
    
    # -- This creates the positivity rate figure
    output$tasa_positividad <- renderPlotly({
        
        if(input$do == 0)
        {
            tmp_dat <- dat
            
        } else {
            tmp_dat <- update_data()
        }
        
        # -- Observed tasa de positividad
        tests <- tmp_dat %>%  
            filter(date>=make_date(2020, 3, 15)) %>%
            group_by(date) %>%
            summarize(positives = sum(result == "positive"), tests = n()) %>%
            mutate(rate = positives / tests) %>%
            mutate(weekday = factor(wday(date)))
        
        # -- Extracting from tests
        x <- as.numeric(tests$date)
        y <- tests$positives
        n <- tests$tests
        
        # -- Design matrix for splines
        df  <- round(3 * nrow(tests)/30)
        x_s <- ns(x, df = df, intercept = FALSE)
        i_s <- c(1:(ncol(x_s)+1))
        
        # -- Design matrix for weekday effect
        w            <- factor(wday(tests$date))
        contrasts(w) <- contr.sum(length(levels(w)), contrasts = TRUE)
        x_w          <- model.matrix(~w)
        
        # -- Design matrix
        X <- cbind(x_s, x_w)
        
        # -- Fitting model 
        fit  <- glm(cbind(y, n-y) ~ -1 + X, family = "quasibinomial")
        beta <- coef(fit)
        summary(fit)
        
        # -- Computing probabilities
        tests$fit <- X[, i_s] %*% beta[i_s]
        tests$se  <- sqrt(diag(X[, i_s] %*%
                                   summary(fit)$cov.scaled[i_s, i_s] %*%
                                   t(X[, i_s])))
        
        
        tests %>%
            ggplot(aes(date, rate)) +
            geom_hline(yintercept = 0.05, lty=2, color="gray") +
            geom_point(aes(date, rate), size=2, alpha=0.40) +
            geom_ribbon(aes(ymin= expit(fit - 3*se), ymax = expit(fit + 3*se)), alpha=0.20) +
            geom_line(aes(y = expit(fit)), color="blue2", size=0.80) +
            ylab("Tasa de positividad") +
            xlab("Fecha") +
            ggtitle("Tasa de Positividad en Puerto Rico") +
            scale_y_continuous(labels = scales::percent) +
            coord_cartesian(ylim = c(0, 0.25)) +
            scale_x_date(date_labels = "%B %d") +
            theme_bw()
        
        ggplotly()
    })
    
    # -- This creates the daily number of tests figure
    output$numero_pruebas <- renderPlotly({
        
        if(input$do == 0){
            pruebas <- dat
        } else {
            pruebas <- update_data()
        }
        
        pruebas %>% 
            filter(result %in% c("positive", "negative")) %>% 
            filter(date > make_date(2020, 3, 15)) %>%
            group_by(date = ceiling_date(date, 
                                         unit = "week", 
                                         week_start = wday(max(dat$date)))) %>%
            summarize(tests = n()) %>%
            ggplot(aes(date, tests)) +
            geom_bar(color="black", fill="#252525", size=0.20, stat = "identity") +
            ggtitle("Número de Pruebas Semanales en Puerto Rico") +
            ylab("Número de pruebas") +
            xlab("Semana acabando en esta fecha") +
            scale_y_continuous(labels = scales::comma,
                               breaks = seq(0, 30000, by = 5000)) +
            scale_x_date(date_labels = "%B %d") +
            theme_bw()
        
        ggplotly()
    
    })
    
    # -- This creates the daily number of tests figure
    output$numero_pruebas_resultados <- renderPlotly({
        
        if(input$do == 0)
        {
            dat %>% 
                filter(result %in% c("positive", "negative")) %>% 
                filter(date > make_date(2020, 3, 15)) %>%
                group_by(date = ceiling_date(date,
                                             unit = "week", 
                                             week_start = wday(max(dat$date))), result) %>%
                summarize(tests = n()) %>%
                ggplot(aes(date, tests, fill=result)) +
                geom_bar(color="black", alpha=0.90, size=0.20, stat = "identity", position = "dodge2") +
                ggtitle("Número de Pruebas Semanales en Puerto Rico") +
                ylab("Número de pruebas") +
                xlab("Semana acabando en esta fecha") +
                scale_y_continuous(labels = scales::comma, 
                                   trans  = "log10") +
                scale_x_date(date_labels = "%B %d") +
                scale_fill_manual(name = "Result:",
                                  values = c("#252525", "#cb181d")) +
                
                theme_bw()
            
            ggplotly()
        } else {
            update_data() %>% 
                filter(result %in% c("positive", "negative")) %>% 
                filter(date > make_date(2020, 3, 15)) %>%
                group_by(date = ceiling_date(date,
                                             unit = "week", 
                                             week_start = wday(max(dat$date))), result) %>%
                summarize(tests = n()) %>%
                ggplot(aes(date, tests, fill=result)) +
                geom_bar(color="black", alpha=0.90, size=0.20, stat = "identity", position = "dodge2") +
                ggtitle("Número de Pruebas Semanales en Puerto Rico") +
                ylab("Número de pruebas") +
                xlab("Semana acabando en esta fecha") +
                scale_y_continuous(labels = scales::comma, 
                                   trans  = "log10") +
                scale_x_date(date_labels = "%B %d") +
                scale_fill_manual(name = "Result:",
                                  values = c("#252525", "#cb181d")) +
                
                theme_bw()
            
            ggplotly()
        }
    })
    
    # -- This allows users to download data
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("pruebas-pr.csv", sep = "")
        },
        content = function(file) {
            if(input$do == 0){
                write.csv(dat, file, row.names = FALSE)  
            } else {
                write.csv(update_data(), file, row.names = FALSE)  
            }
        }
    )
    
    
    
    
    
    
    
    
    # -- This creats the positivity rate plot
    output$model_fit <- renderPlotly({
        
        # -- Loading pruebas pr
        load("rdas/pruebas-pr.rda")

        # -- Reported dates
        dates <- dat$reported
        
        # -- Outcome
        Y <- mutate(dat, y = ifelse(result == "positive", 1, 0))$y
        
        # -- Design matrix for weekday effect
        w            <- factor(wday(dates))
        contrasts(w) <- contr.sum(length(levels(w)), contrasts = TRUE)
        x_w          <- model.matrix(~w)
        i_w <- 1:ncol(x_w)
        
        # -- Design matrix for splines
        x_s <- ns(dates, df = 7, intercept = FALSE)
        i_s <- ncol(x_w) + 1:ncol(x_s)
        
        # -- Design matrix
        X <- cbind(x_w, x_s)
        
        # -- Fitting model 
        fit  <- glm(Y ~ -1 + X, family = "binomial")
        beta <- coef(fit)
        
        # -- Computing probabilities adjusted for week days
        probs <- X[, i_s] %*% beta[i_s]
        probs <- expit(probs)
        
        # -- Visualization
        tibble(dates, probs) %>%
            unique() %>%
            ggplot(aes(dates, probs)) +
            geom_hline(yintercept = 0.05, lty=2, color="red3") +
            # geom_point(aes(reported, rate), alpha=0.50, data = rate) +
            geom_line() +
            xlab("Fecha") +
            ylab("Tasa de positividad estimada") +
            ggtitle("Tasa de Positividad Estimada en Puerto Rico") +
            scale_y_continuous(labels = scales::percent) +
            theme_bw()
        
        ggplotly()
    })
    
})
