library(RMySQL)
mysqlconnection <- dbConnect(RMySQL::MySQL(),
                             user = 'lolo7no',
                             password = 'Utana1991',
                             dbname = 'twitter',
                             host = 'localhost')
tb <- dbListTables(mysqlconnection)
d1 <- dbReadTable(mysqlconnection, tb[2])

saveRDS(d1, "~/../Desktop/twitter_full_18062020.rds")

dbDisconnect(mysqlconnection)
rm(tb, mysqlconnection)

d1 <- d1 %>%
  mutate(covid = stri_detect(text, fixed = "covid", case_insensitive = T))%>%
  filter(covid)

d1 <- d1 %>% 
  distinct(id_str, .keep_all = T)%>%
  mutate(
    hashtags = stri_extract_all(text, regex = "#[[:alnum:]_]+"),
    ats = stri_extract_all(text, regex = "@[[:alnum:]_]+"),
    emojis = stri_extract_all_charclass(text, "\\p{EMOJI}"),
    
    texto = gsub(text, pattern = " ", replacement = "_"),
    texto = gsub(texto, pattern = "[[:space:]]", replacement = ""),
    texto = gsub(texto, pattern = "_", replacement = " "),
    texto = gsub(texto, pattern = "ª", replacement = ""),
    texto = gsub(texto, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""),
    texto = gsub(texto, pattern = "https[^[:space:]]*", replacement = ""),
    texto = gsub(texto, pattern = "[^[:alpha:][:space:]]*", replacement = ""),
    texto = gsub(texto, pattern = "[[:punct:]]", replacement = ""),
    texto = gsub(texto, pattern = "  ", replacement = " "),
    texto = trimws(texto),
    texto = str_replace_all(texto, pattern = "rt", 
                            replacement = ""),
    fecha = as.POSIXct(created, 
                       format = "%Y-%m-%d %T") - hours(7),
    RT = stri_detect(text, fixed = "RT"),
    replied_TO = ifelse(RT, stri_extract_first(text, regex = "@[[:alnum:]_]+"), "")
  ) %>% filter(fecha>as.POSIXct("2020-03-30", format = "%Y-%m-%d"))

saveRDS(d1, "~/../Desktop/covid018JUN20.rds")


d2 <- readRDS("GitHub/COVID-19-Opinion/01_datos/tweets.rds") 

d3 <- bind_rows(d1, d2) %>% 
  distinct(id_str, .keep_all = T)
saveRDS(d3, "~/GitHub/COVID-19-Opinion/01_datos/tweets.rds")





ur <- ggplot(data = tweets_sum, 
       aes(size = `Número de Tweets`,
           y = `Número de Tweets`,
           color = `Número de Tweets`,
           x = Fecha)) +
  geom_smooth(method = "loess",
              show.legend = F,
              colour="black") +
  geom_point() +
  scale_color_continuous(NULL, NULL, NULL)+
  scale_size(NULL, NULL, NULL)+
  scale_x_datetime(date_breaks = "1 week", date_labels =  "%b/%d")+
  theme_ipsum(grid="Y") +
  labs(title=str_wrap(titulo, width = 80),
       subtitle = str_wrap(subtitulo, width = 80),
       y="Número de tweets",
       x="",
       caption=caption1)+
  theme(plot.title = element_text(size = 35),
        plot.subtitle = element_text(size = 25),
        plot.caption = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(angle = 90))


plotly <- ggplotly(ur+theme(axis.text.x = element_text(size=8)),height=600,  
                   tooltip = c("x", "y"),
                   dynamicTicks = TRUE
)%>%
  rangeslider() %>%
  layout(title = list(text = "",
                      y = 1.1),
         hovermode = "x", 
         tickvalues ="", 
         annotations = list(x = 1, y = 0, 
                            text = caption, 
                            showarrow = F, 
                            xref='paper', 
                            yref='paper', 
                            xanchor='right', 
                            yanchor='auto', 
                            xshift=0, 
                            yshift=0,
                            font=list(size=15, color="red")))

plotly

