# Library Requirements
library(shiny)
library(shinydashboard)
library(lubridate)
library(dplyr)
library(ggplot2)
library(glue)
library(scales)
library(plotly)
library(DT)
library(ggthemes)
library(cowplot)
library(dashboardthemes)
library(tidyverse)
library(leaflet)
library(rsconnect)

options(scipen = 999)
options(digits = 2)


# Theme Plot
theme_gppr <- function(){
  font <- "Georgia"   #assign font family up front

  theme_minimal() %+replace%    #replace elements we want to change

    theme(

      #grid elements
      panel.grid.major = element_blank(),    #strip major gridlines
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks

      #since theme_minimal() already strips axis lines,
      #we don't need to do that again

      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly

      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 14),               #font size

      plot.caption = element_text(           #caption
        family = font,            #font family
        size = 9,                 #font size
        hjust = 1),               #right align

      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 10),               #font size

      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 9),                #font size

      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))

      #since the legend often requires manual tweaking
      #based on plot content, don't define it here
    )
}

theme_algo <- function(){

  font <- "Georgia"

  theme(

    panel.background = element_rect(fill = "white"),
    panel.grid.major.y = element_line(colour = "grey80"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      family = font,
      size = 20,
      face = 'bold',
      hjust = 0,
      vjust = 1),

    axis.title = element_text(
      family = font,
      size = 10),

    axis.text = element_text(family = font,
                             size = 9),

    axis.text.x = element_text(
      margin=margin(5, b = 10))

  )
}

# Read Dataset
dataset_ready <- read.csv("dataset_wider.csv")

# Feature Engineering
## Adding new column growth(Local Tax)
dataset_ready <- dataset_ready %>%
  mutate(Growth_PD_2010_2010 = 0,
         Growth_PD_2010_2011 = (Pajak_Daerah_2011-Pajak_Daerah_2010)/Pajak_Daerah_2010,
         Growth_PD_2011_2012 = (Pajak_Daerah_2012-Pajak_Daerah_2011)/Pajak_Daerah_2011,
         Growth_PD_2012_2013 = (Pajak_Daerah_2013-Pajak_Daerah_2012)/Pajak_Daerah_2012,
         Growth_PD_2013_2014 = (Pajak_Daerah_2014-Pajak_Daerah_2013)/Pajak_Daerah_2013,
         Growth_PD_2014_2015 = (Pajak_Daerah_2015-Pajak_Daerah_2014)/Pajak_Daerah_2014
         )

## Making DataFrame that containts an average of local tax growth
mean_growth_dataset <- data.frame(province=dataset_ready[,1], Means_Growth_PD= rowMeans(dataset_ready[,15:19]))

## Joining dataframe(1)
dataset_ready <- dataset_ready %>%
  left_join(mean_growth_dataset, by = "province")

## Adding new column growth(GRDP)
dataset_ready <- dataset_ready %>%
  mutate(Growth_PDRB_2010_2010 = 0,
         Growth_PDRB_2010_2011 = (PDRB_2011-PDRB_2010)/PDRB_2010,
         Growth_PDRB_2011_2012 = (PDRB_2012-PDRB_2011)/PDRB_2011,
         Growth_PDRB_2012_2013 = (PDRB_2013-PDRB_2012)/PDRB_2012,
         Growth_PDRB_2013_2014 = (PDRB_2014-PDRB_2013)/PDRB_2013,
         Growth_PDRB_2014_2015 = (PDRB_2015-PDRB_2014)/PDRB_2014
  )

## Making DataFrame that containts an average of GRDP
mean_growth_dataset_pdrb <- data.frame(province=dataset_ready[,1], Means_Growth_PDRB= rowMeans(dataset_ready[,22:26]))

## Joining dataframe(2)
dataset_ready <- dataset_ready %>%
  left_join(mean_growth_dataset_pdrb, by = "province")

## Adding new column growth(5 Years)
dataset_ready <- dataset_ready %>%
  mutate(Growth_tdasar_takhir = (PDRB_2015-PDRB_2010)/PDRB_2010)

## Adding new column growth(Tax Bouyancy)
dataset_ready <- dataset_ready %>%
  mutate(Tax_Bouyancy = Means_Growth_PD/Growth_tdasar_takhir)



# Dataset Pre-paration Interactive plot 1 and 2
dataset_viz <- rename(dataset_ready, "2010" = Pajak_Daerah_2010,
                      "2011" = Pajak_Daerah_2011,
                      "2012" = Pajak_Daerah_2012,
                      "2013" = Pajak_Daerah_2013,
                      "2014" = Pajak_Daerah_2014,
                      "2015" = Pajak_Daerah_2015)

dataset_pivot_viz <- pivot_longer(data = dataset_viz,
                                  cols = c(2:7),
                                  names_to = "Pajak_Daerah",
                                  values_to = "Nilai")

dataset_pivot_viz_2 <- pivot_longer(data = dataset_viz,
                                    cols = c(8:13),
                                    names_to = "PDRB",
                                    values_to = "Nilai_PDRB")

dataset_pivot_viz_aceh <- dataset_pivot_viz %>%
  filter(province == "01 Provinsi Aceh") %>%
  select(Pajak_Daerah, Nilai) %>%
  mutate(Nilai_growth = 100*(Nilai - lag(Nilai))/lag(Nilai),
         Nilai_growth = replace_na(Nilai_growth,0))

dataset_pivot_viz_aceh_pdrb <- dataset_pivot_viz_2 %>%
  filter(province == "01 Provinsi Aceh") %>%
  select(PDRB, Nilai_PDRB) %>%
  mutate(Nilai_growth = 100*(Nilai_PDRB - lag(Nilai_PDRB))/lag(Nilai_PDRB),
         Nilai_growth = replace_na(Nilai_growth,0)) %>%
  mutate(PDRB = replace(PDRB, PDRB == "PDRB_2010", "2010"),
         PDRB = replace(PDRB, PDRB == "PDRB_2011", "2011"),
         PDRB = replace(PDRB, PDRB == "PDRB_2012", "2012"),
         PDRB = replace(PDRB, PDRB == "PDRB_2013", "2013"),
         PDRB = replace(PDRB, PDRB == "PDRB_2014", "2014"),
         PDRB = replace(PDRB, PDRB == "PDRB_2015", "2015"))

box_information_1 <- dataset_ready %>%
  filter(province == "29 Provinsi Bangka Belitung") %>%
  select(Means_Growth_PDRB) %>%
  pull() %>% percent()

box_information_2 <- dataset_ready %>%
  filter(province == "29 Provinsi Bangka Belitung") %>%
  select(Means_Growth_PD) %>%
  pull() %>% percent()

box_information_3 <- dataset_ready %>%
  filter(province == "29 Provinsi Bangka Belitung") %>%
  select(Growth_tdasar_takhir) %>%
  pull() %>% percent()

box_information_4 <- dataset_ready %>%
  filter(province == "29 Provinsi Bangka Belitung") %>%
  select(Tax_Bouyancy) %>%
  pull() %>% percent()


## MAP Visualization

# Read Dataset
dataset_map <- read.csv("MyData_Map.csv")





































