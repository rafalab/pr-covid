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

        # -- HTTP get
        http_get <- GET(url)

        # -- Turning object into json
        json <- content(http_get)

        # -- Wrangle
        dat <- do.call(rbind, json) %>%
            as_tibble() %>%
            mutate_all(as.character) %>%
            setNames(c("collected", "reported", "agegroup", "test_type", "result", "municipio", "created_at")) %>%
            mutate_all(as.character) %>%
            mutate(agegroup   = gsub(" to ", "-", agegroup),
                   collected  = gsub("/", "-", collected),
                   reported   = gsub("/", "-", reported),
                   created_at = gsub("/", "-", created_at),
                   collected  = mdy(collected),
                   reported   = mdy(reported),
                   created_at = mdy_hm(created_at),
                   municipio  = ifelse(municipio=="NULL", NA, municipio),
                   result     = ifelse(result=="NULL", NA, result),
                   test_type  = ifelse(test_type=="NULL", NA, test_type),
                   agegroup   = ifelse(agegroup %in% c("NULL", "N/A"), NA, agegroup),
                   result     = tolower(result),
                   result     = case_when(grepl("positive", result) ~ "positive",
                                          grepl("negative", result) ~ "negative",
                                          result == "not detected" ~ "negative",
                                          TRUE ~ "other")) %>%
            filter(year(reported) == 2020, year(collected) == 2020) %>%
            arrange(reported, collected)
        
        # -- Saving things
        save(dat, file = "~/Desktop/covid19-pr-shiny/rdas/pruebas-pr.rda", compress = "xz")
        return(dat)
    })
    
    # -- This creates the positivity rate figure
    output$tasa_positividad <- renderPlotly({
        
        if(input$do == 0)
        {
            tmp_dat <- dat %>%
                group_by(reported, result) %>%
                summarize(number_test = n()) %>%
                ungroup() %>%
                complete(reported, result, fill = list(number_test = 0)) %>%
                spread(result, number_test) %>%
                mutate(rate = positive / (positive+other+negative)) %>%
                select(reported, rate)
            
            tmp_dat %>%
                ggplot(aes(reported, rate)) +
                geom_hline(yintercept = 0.05, lty=2, color="gray") +
                geom_point(size=2, alpha=0.40) +
                geom_smooth(formula = y ~ x,
                            method  = "loess",
                            size    = 0.60,
                            color   = "red3",
                            fill    = "red3",
                            alpha   = 0.20,
                            span    = 21/nrow(tmp_dat),
                            method.args = list(degree = 1)) +
                ylab("Tasa de positividad") +
                xlab("Fecha") +
                ggtitle("Tasa de Positividad en Puerto Rico") +
                scale_y_continuous(labels = scales::percent) +
                scale_x_date(date_labels = "%B %d") +
                theme_bw()
            
            ggplotly()
        } else {
            tmp_dat <- update_data() %>%
                group_by(reported, result) %>%
                summarize(number_test = n()) %>%
                ungroup() %>%
                complete(reported, result, fill = list(number_test = 0)) %>%
                spread(result, number_test) %>%
                mutate(rate = positive / (positive+other+negative)) %>%
                select(reported, rate)
            
            tmp_dat %>%
                ggplot(aes(reported, rate)) +
                geom_hline(yintercept = 0.05, lty=2, color="gray") +
                geom_point(size=2, alpha=0.40) +
                geom_smooth(formula = y ~ x,
                            method  = "loess",
                            size    = 0.60,
                            color   = "red3",
                            fill    = "red3",
                            alpha   = 0.20,
                            span    = 21/nrow(tmp_dat),
                            method.args = list(degree = 1)) +
                ylab("Tasa de positividad") +
                xlab("Fecha") +
                ggtitle("Tasa de Positividad en Puerto Rico") +
                scale_y_continuous(labels = scales::percent) +
                scale_x_date(date_labels = "%B %d") +
                theme_bw()
            
            ggplotly()
        }
    })
    
    # -- This creates the daily number of tests figure
    output$numero_pruebas <- renderPlotly({
        
        if(input$do == 0)
        {
            tmp_dat <- dat %>%
                group_by(reported) %>%
                summarize(number_test = n()) %>%
                ungroup()
            
            tmp_dat %>%
                ggplot(aes(reported, number_test)) +
                geom_point(size=2, alpha = 0.40) +
                geom_smooth(formula = y ~ x,
                            method  = "loess",
                            size    = 0.60,
                            color   = "red3",
                            fill    = "red3",
                            alpha   = 0.20,
                            span    = 21/nrow(tmp_dat),
                            method.args = list(degree = 1)) +
                ylab("Número de pruebas") +
                xlab("Fecha") +
                ggtitle("Número de Pruebas Diarias en Puerto Rico") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_labels = "%B %d") +
                theme_bw()
            
            ggplotly()
        } else {
            tmp_dat <- update_data() %>%
                group_by(reported) %>%
                summarize(number_test = n()) %>%
                ungroup()
            
            tmp_dat %>%
                ggplot(aes(reported, number_test)) +
                geom_point(size=2, alpha = 0.40) +
                geom_smooth(formula = y ~ x,
                            method  = "loess",
                            size    = 0.60,
                            color   = "red3",
                            fill    = "red3",
                            alpha   = 0.20,
                            span    = 21/nrow(tmp_dat),
                            method.args = list(degree = 1)) +
                ylab("Número de pruebas") +
                xlab("Fecha") +
                ggtitle("Número de Pruebas Diarias en Puerto Rico") +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_labels = "%B %d") +
                theme_bw()
            
            ggplotly()
        }
    })
    
    # -- This creates the daily number of tests figure
    output$numero_pruebas_resultados <- renderPlotly({
        
        # -- To be used below
        tmp_dat <- dat %>%
            group_by(reported) %>%
            summarize(number_test = n()) %>%
            ungroup()
        
        if(input$do == 0)
        {
            dat %>%
                group_by(reported, result) %>%
                summarize(number_test = n()) %>%
                ungroup() %>%
                ggplot(aes(reported, number_test, color=result, fill=result)) +
                geom_point(size=2, alpha = 0.20) +
                geom_smooth(method  = "loess",
                            size    = 0.60,
                            alpha   = 0.40,
                            span    = 21/nrow(tmp_dat),
                            method.args = list(degree = 1)) +
                ylab("Número de pruebas") +
                xlab("Fecha") +
                ggtitle("Número de Pruebas Diarias en Puerto Rico") +
                scale_y_continuous(labels = scales::comma, trans = "log10") +
                scale_x_date(date_labels = "%B %d") +
                scale_color_manual(name = "Result:",
                                   values = c("#cb181d", "#252525", "#4292c6"),
                                   labels = c("Negative", "Positive", "Inconclusive")) +
                scale_fill_manual(name = "Result:",
                                  values = c("#cb181d", "#252525", "#4292c6"),
                                  labels = c("Negative", "Positive", "Inconclusive")) +
                theme_bw()
            
            ggplotly()
        } else {
            update_data() %>%
                group_by(reported, result) %>%
                summarize(number_test = n()) %>%
                ungroup() %>%
                ggplot(aes(reported, number_test, color=result, fill=result)) +
                geom_point(size=2, alpha = 0.20) +
                geom_smooth(method  = "loess",
                            size    = 0.60,
                            alpha   = 0.40,
                            span    = 21/nrow(tmp_dat),
                            method.args = list(degree = 1)) +
                ylab("Número de pruebas") +
                xlab("Fecha") +
                ggtitle("Número de Pruebas Diarias en Puerto Rico") +
                scale_y_continuous(labels = scales::comma, trans = "log10") +
                scale_x_date(date_labels = "%B %d") +
                scale_color_manual(name = "Result:",
                                   values = c("#cb181d", "#252525", "#4292c6"),
                                   labels = c("Negative", "Positive", "Inconclusive")) +
                scale_fill_manual(name = "Result:",
                                  values = c("#cb181d", "#252525", "#4292c6"),
                                  labels = c("Negative", "Positive", "Inconclusive")) +
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
