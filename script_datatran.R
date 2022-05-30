#=================
# LOADING PACKAGES
#=================

library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(treemapify)
library(viridis)


#===================
# IMPORTING DATASETS
#===================

datatran2017 <- read_delim("datatran2017.csv", delim = ";",
                           locale = locale(date_names = "pt",
                                           decimal_mark = ",",
                                           grouping_mark = ".",
                                           encoding = "ISO-8859-1"))

datatran2018 <- read_delim("datatran2018.csv", delim = ";",
                           locale = locale(date_names = "pt",
                                           decimal_mark = ",",
                                           grouping_mark = ".",
                                           encoding = "ISO-8859-1"))

datatran2019 <- read_delim("datatran2019.csv", delim = ";",
                           locale = locale(date_names = "pt",
                                           decimal_mark = ",",
                                           grouping_mark = ".",
                                           encoding = "ISO-8859-1"))

datatran2020 <- read_delim("datatran2020.csv", delim = ";",
                           locale = locale(date_names = "pt",
                                           decimal_mark = ",",
                                           grouping_mark = ".",
                                           encoding = "ISO-8859-1"))

datatran2021 <- read_delim("datatran2021.csv", delim = ";",
                           locale = locale(date_names = "pt",
                                           decimal_mark = ",",
                                           grouping_mark = ".",
                                           encoding = "ISO-8859-1"))

datatran2022 <- read_delim("datatran2022.csv", delim = ";",
                           locale = locale(date_names = "pt",
                                           decimal_mark = ",",
                                           grouping_mark = ".",
                                           encoding = "ISO-8859-1"))


#==================================
# PREPARING AND COMBINE ALL DATASETS
#==================================

datatran2017_f <- filter(datatran2017, month(datatran2017$data_inversa) > 4)

datatran2021$latitude <- as.double(datatran2021$latitude)

datatran2022$latitude <- as.double(datatran2022$latitude)

all_datatran <- bind_rows(datatran2017_f, datatran2018, datatran2019,
                          datatran2020, datatran2021, datatran2022)


#=================================
# FORMATTING AND ORGANIZING COLUMNS
#=================================

all_datatran$mes <- month(all_datatran$data_inversa,
                          label = TRUE, abbr = TRUE)

all_datatran$ano <- year(all_datatran$data_inversa)

all_datatran <- all_datatran %>%
  relocate(mes, .after = data_inversa) %>%
  relocate(ano, .after = data_inversa)


#==========================
# EXPLORATORY DATA ANALYSIS
#==========================

acidentes_mes <- all_datatran %>%
  count(mes, name = "acidentes", sort = TRUE) %>%
  mutate(media = acidentes/5)

acidentes_ano_mes <- all_datatran %>%
  count(ano, mes, name = "acidentes", sort = TRUE)

acidentes_uf <- all_datatran %>%
  count(uf, name = "acidentes", sort = TRUE)


#===============
# VISUALIZATIONS
#===============

plt_acidentes_mes <- ggplot(acidentes_mes, aes(x = mes, y = media)) +
  geom_segment(aes(x = mes, xend = mes, y = 0, yend = media),
               color = viridis(12), size = 1) +
  geom_point(size = 4, color = viridis(12)) +
  coord_flip() +
  annotate("text", x = 12, y = 6648 + 500, label = "6648",
           color = "black", size = 4, fontface = "bold") +
  labs(title = "Qual foi o mês com a maior média de acidentes?",
       x = "", y = "", caption = paste(
         "Fonte: Polícia Rodoviária Federal, Brasil", "\n",
         "Período: mai/2017 a abr/2022", "\n",
         "Última atualização:", today())) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 15, face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 5))

ggsave("plt_acidentes_mes.png", plot = plt_acidentes_mes,
       width = 6, height = 4, units = "in", dpi = 300)
ggsave("plt_acidentes_mes.pdf", plot = plt_acidentes_mes,
       width = 6, height = 4, units = "in", dpi = 300)

plt_acidentes_ano_mes <- ggplot(acidentes_ano_mes, aes(x = mes, y = acidentes, group = ano)) +
  geom_line(color = "grey", size = 0.4) +
  geom_point(color = "black", size = 1) +
  facet_wrap(~ano) +
  labs(title = "Evolução no número de acidentes",
       x = "", y = "", caption = paste(
         "Fonte: Polícia Rodoviária Federal, Brasil", "\n",
         "Período: mai/2017 a abr/2022", "\n",
         "Última atualização:", today())) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, face = "bold"),
        axis.text.y = element_text(size = 5),
        strip.text = element_text(size =  6, face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 5))

ggsave("plt_acidentes_ano_mes.png", plot = plt_acidentes_ano_mes,
       width = 6, height = 4, units = "in", dpi = 300)
ggsave("plt_acidentes_ano_mes.pdf", plot = plt_acidentes_ano_mes,
       width = 6, height = 4, units = "in", dpi = 300)

plt_acidentes_uf <- ggplot(acidentes_uf, aes(area = acidentes, fill = uf)) +
  geom_treemap(show.legend = FALSE, fill = viridis(27)) +
  geom_treemap_text(label = acidentes_uf$uf, color = "white") +
  labs(title = "Quais os estados com mais acidentes?",
       x = "", y = "", caption = paste(
         "Fonte: Polícia Rodoviária Federal, Brasil", "\n",
         "Período: mai/2017 a abr/2022", "\n",
         "Última atualização:", today()))

ggsave("plt_acidentes_uf.png", plot = plt_acidentes_uf,
       width = 6, height = 4, units = "in", dpi = 300)
ggsave("plt_acidentes_uf.pdf", plot = plt_acidentes_uf,
       width = 6, height = 4, units = "in", dpi = 300)


