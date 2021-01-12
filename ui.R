
library(shiny)

shinyUI(

    dashboardPage(

        dashboardHeader(title= "Local Tax Analytics"),

        dashboardSidebar(

            sidebarMenu(
                menuItem(text = "Historical", icon = icon("history"), tabName = "historical"),
                menuItem(text = "Geospatial", icon = icon("map-marked-alt"), tabName = "geospatial")
            )
        ),

        dashboardBody(

            shinyDashboardThemes("onenote"),

            tabItems(
                # Historical
                tabItem(tabName = "historical", align = "center",

                        h1(tags$b("Historical Review")),

                        selectInput(
                            inputId = "province_indo",
                            label = "Select Province",
                            choices = unique(dataset_pivot_viz$province),
                            selected = "01 Provinsi Aceh",
                            multiple = FALSE
                        ),

                        fluidRow(
                            column(
                                width = 3,
                                valueBoxOutput("box_1", width = 12)
                            ),
                            column(
                                width = 3,
                                valueBoxOutput("box_2", width = 12)
                            ),
                            column(
                                width = 3,
                                valueBoxOutput("box_3", width = 12)
                            ),
                            column(
                                width = 3,
                                valueBoxOutput("box_4", width = 12)
                            )
                        ),

                        fluidRow(
                            box(width = 4,
                                background = "blue",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotlyOutput("interactive_plot_1", height = "500")
                            ),
                            box(width = 4,
                                background = "blue",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                plotlyOutput("interactive_plot_2", height = "500")
                            ),
                            tabBox(
                                title = tagList(shiny::icon("file"), "Analytics Bot"),
                                tabPanel("Description Status",
                                         h3(tags$b("GRDP!")),
                                         div(p("Gross Regional Domestic Product based on market prices is the total gross value added arising from all economic
                                               sectors in a region. Added value is the value added from a combination of production factors and raw materials
                                               in the production process. The calculation of added value is the value of production (output) minus intermediate
                                               costs. The gross value added here includes the components of factor income (wages and salaries, interest, land rent and profits),
                                               depreciation and net indirect taxes. So by adding up the gross added value from each sector and adding up the gross added value from
                                               all these sectors, the Gross Regional Domestic Product will be obtained based on market prices."),
                                             h3(tags$b("Tax Bouyancy!")),
                                             p("Tax Bouyancy is an indicator to measure efficiency and responsiveness of revenue mobilization in response to growth
                                               in the GDP or Natinonal Income and GRDP")
                                         )),
                                tabPanel("Insight", h3(tags$b("Decision Recommendation!")),
                                         div(p("The growth performance of local tax revenue in each province can be illustrated through the tax bouyancy with the formula for the average
                                         growth of regional tax revenue divided by the growth of the 5-year period from 2010 to 2015. The small value of the tax bouyancy can also be
                                         influenced by the economic conditions in each region. If the economic downturn occurs in a row, it will make the tax bouyancy smaller.
                                         Regions with low economic levels should start to pay attention, it may be that the government's ability to collect taxes is also not optimal"),
                                             h3(tags$b("So, Is it potential area?")),
                                             p("Coordinate with our team to be able to start interacting with stakeholders in that area")
                                         )),
                                width = 4,
                                height = "540"
                            )
                        )
                ),

                tabItem(tabName = "geospatial", align = "center",
                        h1(tags$b("Local Tax Map Monitoring")),
                        leafletOutput("map_leaflet", height = "800px")
                )
            )

        )
    )
)

