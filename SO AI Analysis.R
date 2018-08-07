library(tidyverse)
library(highcharter)
# replies <- read_csv('C:/Users/mungaialex/Desktop/Aritificial Intelligence Course/Stackoverflow/survey_results_public.csv')
replies <- read_csv('G:/Learning/Aritificial Intelligence Course/Stackoverflow/survey_results_public.csv')

#Genderwise AI Interesting
replies %>%
  filter(!is.na(AIInteresting)) %>%
  filter(Gender %in% c('Male','Female')) %>% 
  group_by(AIInteresting,Gender) %>%
  summarise(count = n()) %>%
  ggplot(aes(reorder(AIInteresting,count), count, fill = factor(count) )) +
  facet_wrap(~Gender) +
  geom_col() +
  geom_text(aes(label = AIInteresting, y = 500), size = 5, hjust = 0) +
  coord_flip() +
  guides(fill = FALSE) +
  labs(x = 'Answer', y = 'Count', title = 'Exciting aspects of AI') +
  theme(axis.text.y = element_blank()) +
  scale_fill_brewer(palette = 'Oranges')

#What do you think is the most exciting aspect of increasingly advanced AI technology?
replies %>% 
  filter(!is.na(AIInteresting)) %>%
  filter(!is.na(Age)) %>% 
  select(AIInteresting,  Age) %>%
  group_by(AIInteresting, Age) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Gender = reorder(AIInteresting,Count)) %>%
  hchart('column',hcaes('AIInteresting','Count', group = 'Age')) %>% 
  hc_title(text = 'Exciting Aspects of AI') %>% 
  hc_yAxis(type = 'logarithmic')
# replies %>%  
#   filter(!is.na(AIInteresting)) %>% 
#   filter(!is.na(Age)) %>% 
#   #filter(Age %in% c('Male','Female')) %>% 
#   group_by(AIInteresting,Age) %>% 
#   count() %>% 
#   ungroup() %>% 
#   group_by(Age) %>% 
#   mutate(Percentage = round((n / sum(n))*100),2) %>% 
#   hchart('column',hcaes('AIInteresting','Percentage', group = 'Age')) %>% 
#   hc_title(text = ' Exciting Aspects of AI')  %>% 
#   hc_yAxis(labels = list(format = "{value}%"), title = list(text = "Response in %"))

current_languages <- unlist(str_split(unique(replies$LanguageWorkedWith), ';'))
print(current_languages)
                           
devTypes <- unlist(str_split(replies$DevType, ';'))
print(unique(devTypes))
                            

count<-replies%>%filter(Gender %in% c('Male','Female')) %>% group_by(AIInteresting,Gender) %>%summarise(count = n())

replies %>% 
  filter(!is.na(Country)) %>% 
  count(Country) -> countries

print(countries)

highchart() %>%
  hc_add_series_map(worldgeojson, countries, value = "n", joinBy = c('name','Country'))  

highchart() %>%
  hc_add_series_map(worldgeojson, countries, value = "n", joinBy = c('name','Country'))  %>% 
  #hc_colors(c("darkorange", "darkgray")) %>% 
  hc_colorAxis(stops = color_stops()) %>% 
  hc_title(text = "Countries in World Map") %>% 
  hc_subtitle(text = "This is beauty")


#What do you think is the most dangerous aspect of increasingly advanced AI technology?
replies %>% 
  filter(!is.na(AIDangerous)) %>%
  filter(!is.na(Age)) %>% 
  select(AIDangerous,  Age) %>%
  group_by(AIDangerous, Age) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Gender = reorder(AIDangerous,Count)) %>%
  hchart('column',hcaes('AIDangerous','Count', group = 'Age')) %>% 
  hc_title(text = 'Dangers of AI') %>% 
  hc_yAxis(type = 'logarithmic')

#Data scientist or machine learning specialist

replies %>% 
  #filter(Employment %in% 'Employed full-time') %>% 
  filter(!is.na(DevType)) %>%
  filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  select(DevType,Gender) %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>%
  group_by(DevType,Gender) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(DevType = reorder(DevType,Count)) %>%
  #mutate(Percentage = round((n / sum(n))*100),2) %>% 
  mutate(perc = round((Count / sum(Count)) * 100)) %>%
  hchart('column',hcaes('DevType','perc',group = 'Gender')) %>% 
  hc_colors(c("darkorange", "darkgray")) %>%  
  hc_title(text = "Developer Type Count by Gender") %>% 
  hc_yAxis(type = 'logarithmic')


replies %>% 
  #filter(Employment %in% 'Employed full-time') %>% 
  filter(!is.na(DevType)) %>%
  #filter(!is.na(Gender), Gender %in% c('Male','Female')) %>% 
  select(DevType) %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>%
  group_by(DevType) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(DevType = reorder(DevType,Count)) %>%
  mutate(Percent = round((Count / sum(Count)) * 100)) %>%
  #hchart('bar',hcaes('DevType','Percent'), colorByPoint = TRUE, name = "Data scientist or machine learning specialist") %>%
  hchart('bar',hcaes('DevType','Percent')) %>% 
  hc_colors(c("darkorange4", "darkgray")) %>%  
  hc_title(text = "Developer Type Count") %>% 
  hc_yAxis(type = 'logarithmic') %>%
hc_add_theme(hc_theme_ffx())

#Percentage of developer types.
replies %>% 
  filter(!is.na(DevType)) %>%
  select(DevType) %>%
  mutate(DevType = str_split(DevType, pattern = ";")) %>%
  unnest(DevType) %>%
  group_by(DevType) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>% mutate(perc = round((Count / sum(Count)) * 100)) %>% arrange(desc(perc))


#Who is Primarily Responsible for Considering the Ramifications of AI?
replies %>% 
  filter(!is.na(AIResponsible)) %>%
  filter(!is.na(Age)) %>% 
  select(AIResponsible,  Age) %>%
  group_by(AIResponsible, Age) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Gender = reorder(AIResponsible,Count)) %>%
  hchart('column',hcaes('AIResponsible','Count', group = 'Age')) %>% 
  hc_title(text = 'Who is responsible for AI ramifications') %>% 
  hc_yAxis(type = 'logarithmic')

#Overall, what's your take on the future of artificial intelligence?
replies %>% 
  filter(!is.na(AIFuture)) %>%
  filter(!is.na(Age)) %>% 
  select(AIFuture,  Age) %>%
  group_by(AIFuture, Age) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Gender = reorder(AIFuture,Count)) %>%
  hchart('column',hcaes('AIFuture','Count', group = 'Age')) %>% 
  hc_title(text = 'The Future of AI') %>% 
  hc_yAxis(type = 'logarithmic')
