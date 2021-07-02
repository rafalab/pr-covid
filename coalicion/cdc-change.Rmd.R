url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv"
vaccines <- read_csv(url) %>% 
  select(date, location, people_fully_vaccinated_per_hundred) %>%
  arrange(date) %>%
  filter(location %in% c(state.name, "Puerto Rico"))

vaccines %>% 
  filter(date == max(date, na.rm = TRUE)) %>% 
  arrange(people_fully_vaccinated_per_hundred) %>% 
    arrange(desc(people_fully_vaccinated_per_hundred))
  
dat1 <- vaccines %>% filter(location == "Puerto Rico") %>% 
  filter(!is.na(people_fully_vaccinated_per_hundred))
dat2 <- vaccines %>% filter(location != "Puerto Rico") %>%
  filter(!is.na(people_fully_vaccinated_per_hundred))

vaccines %>% 
  ggplot(aes(date, people_fully_vaccinated_per_hundred, group = location, color = location == "Puerto Rico")) +
  geom_line(data = dat2, color = "grey", alpha = 0.5) +
  geom_line(data = dat1, color = "red", lwd = 1) +
  annotate("text", max(dat1$date)-days(2), 
            max(dat1$people_fully_vaccinated_per_hundred),
            label = "Puerto Rico", color = "red", hjust = 1) +
  labs(title = "Vacunación en Puerto Rico comparado a estados de EEUU",
      subtitle = "Según datos de los CDC", 
      caption = "Las 51 líneas grises representan los 50 estados y DC.") +
  xlab("Fecha")+
  ylab("Por ciento de la población con dosis completa")+
  theme_bw() 
ggsave("~/Desktop/vacunas.png",width = 8, height = 4)
