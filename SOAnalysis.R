# Input load. Please do not change #
`dataset` = read.csv('C:/Users/destro/REditorWrapper_a6cf63c8-8f8f-4484-8d9f-07ec94018299/input_df_b0ea58fa-b1cc-47cb-b2a2-2f07bd703ca6.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #

library(tidyverse)
library(highcharter)

data <- read_csv("G:/Learning/Aritificial Intelligence Course/Stackoverflow/survey_results_public.csv")

schema <- read_csv('G:/Learning/Aritificial Intelligence Course/Stackoverflow/survey_results_schema.csv')
replies <- read_csv('G:/Learning/Aritificial Intelligence Course/Stackoverflow/survey_results_public.csv')

# dim(replies) # Get the dimensions of the replies data
# 
# head(replies, n = 1) # View first row

# map_df(replies, ~ sum(is.na(.))) %>%
#   gather("question", "n_missing") %>%
#   mutate(perc_answered = (nrow(replies) - n_missing) / nrow(replies) * 100)


replies %>%
  filter(!is.na(Hobby)) %>%
  ggplot(aes(Hobby, fill = Hobby)) + 
  geom_bar() + 
  labs(x = 'Do you code as a hobby?', y = 'Count') +
  guides(fill = FALSE)


data[is.na(data)] <- ""
data[data$Country=="United States",]$Country <- "United States of America"
data[data$Country=="Bolivia",]$Country <- "Bolivia (Plurinational State of)"
data[data$Country=="Venezuela, Bolivarian Republic of...",]$Country <- "Venezuela, Bolivarian Republic of"
data[data$Country=="Iran, Islamic Republic of...",]$Country <- "Iran (Islamic Republic of)"
data[data$Country=="United Kingdom",]$Country <- "United Kingdom of Great Britain and Northern Ireland"

countries <- data %>% group_by(Country) %>% summarise(Total = round(n()))
names(countries) <- c("country.code", "total")
countries$iso3 <- countrycode_data[match(countries$country.code, countrycode_data$country.name.en), "iso3c"]

data(worldgeojson, package = "highcharter")
dshmstops <- data.frame(q = c(0, exp(1:5)/exp(5)),
                        c = substring(viridis(5 + 1, option = "B"), 0, 7)) %>%  list_parse2()

highchart() %>% 
  hc_add_series_map(worldgeojson, countries, value = "total", joinBy = "iso3") %>% 
  hc_colorAxis(stops = dshmstops) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "Stack Overflow Survey Respondents - Country of Residence") %>%
  hc_credits(enabled = TRUE, text = "Sources: Stack Overflow 2018 Developer Survey", style = list(fontSize = "10px")) %>%
  hc_add_theme(hc_theme_chalk())

top10countries <- data %>% group_by(Country) %>% summarise(Total = n()) %>% arrange(desc(Total)) %>% head(10)

print(top10countries)

hchart(top10countries, "pie", hcaes(x = Country, y = Total, color = Country)) %>%
  #hc_colors(c("darkorange", "darkgray")) %>% 
  hc_title(text = "Stack Overflow Survey Respondents - Country of Residence") %>%
  hc_credits(enabled = TRUE, text = "Sources: Stack Overflow 2018 Developer Survey", style = list(fontSize = "10px")) 

head(unique(replies$LanguageWorkedWith), n = 5)
current_languages <- unlist(str_split(unique(replies$LanguageWorkedWith, ';'))
print(current_languages)
