
library(shiny)


shinyServer(
    function(input, output) {

        output$interactive_plot_1 <- renderPlotly({
          trend_local_tax_revenue <- dataset_pivot_viz %>%
            filter(province == input$province_indo) %>%
            select(Pajak_Daerah, Nilai) %>%
            mutate(Nilai_growth = 100*(Nilai - lag(Nilai))/lag(Nilai),
                   Nilai_growth = replace_na(Nilai_growth,0))

          plot <- trend_local_tax_revenue %>%
            ggplot(aes(x=Pajak_Daerah,
                       y= round(Nilai/1000000),
                       group = 1,
                       text = glue("Year : {Pajak_Daerah}
                                    Tax Value : {number(Nilai, big.mark = ',')}
                                    Growth : {format(Nilai_growth, digits = 3)}")
            )) +
            geom_line() + geom_point() +
            labs(title = "Local Tax Revenue",
                 x = "Year",
                 y = "Tax Value (Millions)") +
            theme_algo()

          ggplotly(plot, tooltip = "text")
        })

        output$interactive_plot_2 <- renderPlotly({
          trend_GRDP <- dataset_pivot_viz_2 %>%
            filter(province == input$province_indo) %>%
            select(PDRB, Nilai_PDRB) %>%
            mutate(Nilai_growth = 100*(Nilai_PDRB - lag(Nilai_PDRB))/lag(Nilai_PDRB),
                   Nilai_growth = replace_na(Nilai_growth,0)) %>%
            mutate(PDRB = replace(PDRB, PDRB == "PDRB_2010", "2010"),
                   PDRB = replace(PDRB, PDRB == "PDRB_2011", "2011"),
                   PDRB = replace(PDRB, PDRB == "PDRB_2012", "2012"),
                   PDRB = replace(PDRB, PDRB == "PDRB_2013", "2013"),
                   PDRB = replace(PDRB, PDRB == "PDRB_2014", "2014"),
                   PDRB = replace(PDRB, PDRB == "PDRB_2015", "2015"))

          plot <- trend_GRDP %>%
            ggplot(aes(x=PDRB,
                       y=round(Nilai_PDRB/1000),
                       group = 1,
                       text = glue("Year : {PDRB}
                                    GRDP Value : {number(Nilai_PDRB, big.mark = ',')}
                                    Growth : {format(Nilai_growth, digits = 3)}")
            )) +
            geom_line() + geom_point() +
            labs(title = "GRDP",
                 x = "Year",
                 y = "GRDP Value (Thousands)") +
            theme_algo()

          ggplotly(plot, tooltip = "text")
        })

        output$box_1 <- renderValueBox({
          valueBox(
            dataset_ready %>%
              filter(province == input$province_indo) %>%
              select(Means_Growth_PDRB) %>%
              pull() %>% percent(),
            subtitle = "Average GRDP growth",
            color = "blue",
            icon = icon("sistrix")
          )
        })

        output$box_2 <- renderValueBox({
          valueBox(
            dataset_ready %>%
              filter(province == input$province_indo) %>%
              select(Means_Growth_PD) %>%
              pull() %>% percent(),
            subtitle = "Average growth of local tax revenue",
            color = "blue",
            icon = icon("money-bill")
          )
        })

        output$box_3 <- renderValueBox({
          valueBox(
            dataset_ready %>%
              filter(province == input$province_indo) %>%
              select(Growth_tdasar_takhir) %>%
              pull() %>% percent(),
            subtitle = "5 year regional tax revenue growth",
            color = "blue",
            icon = icon("chart-line")
          )
        })

        output$box_4 <- renderValueBox({
          valueBox(
            dataset_ready %>%
              filter(province == input$province_indo) %>%
              select(Tax_Bouyancy) %>%
              pull() %>% percent(),
            subtitle = "Tax Bouyancy",
            color = "red",
            icon = icon("rocket")
          )
        })

        output$map_leaflet <- renderLeaflet({

            content_popup <- paste(
                sep = " ",
                "Province :", dataset_map$province, "<br>",
                "Average GRDP Growth :", number(dataset_map$Means_Growth_PDRB*100),"%", "<br>",
                "Average Local Tax Revenue Growth :", number(dataset_map$Means_Growth_PD*100),"%", "<br>",
                "5 Year local tax revenue Growth :", number(dataset_map$Growth_tdasar_takhir*100),"%"
            )

            leaflet() %>%
                addTiles() %>%
                addMarkers(
                    data = dataset_map,
                    lng = ~longitude,
                    lat = ~latitude,
                    popup = content_popup,
                    clusterOptions = markerClusterOptions()
                )
        })

})








