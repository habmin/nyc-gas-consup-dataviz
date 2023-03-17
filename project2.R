#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(data.table)
library(shiny)
library(plotly)
library(ggplot2)
library(maps)
library(raster)
library(scales)
library(ggmap)
library(ggalt)
library(basemaps)
library(osmdata)
library(sf)
library(ggmap)
# ========================================
# ********* Loading our Database *********

# our main data
nyc_gas_data <- fread("Natural_Gas_Consumption_by_ZIP_Code_-_2010.csv")

# a pivot table with total of of consumption_therms and consumption_gj by building type
building_type_total <- fread("building_type_total.csv")

# a pivot table with total of of consumption_therms and consumption_gj by building type
borough_total <- fread("borough_total.csv")

# ========================================

# ========================================
# ******* Augmenting our Database ********

# adding long, lat, and borough to our dataset from another dataset

zipcodes <- fread("zipcodes.csv")

lat_vec <- c()
long_vec <- c()
borough <- c()

for (i in 1:nrow(nyc_gas_data)) {
    if (length(zipcodes[zipcodes$zip==nyc_gas_data[i]$zip_code,]$lat) == 0) {
        lat_vec <- append(lat_vec, 40.75065)
        long_vec <- append(long_vec, -73.99718)
        borough  <- append(borough , "Unknown")
    }
    else {
        lat_vec <- append(lat_vec, as.double(zipcodes[zipcodes$zip==nyc_gas_data[i]$zip_code,]$lat))
        long_vec <- append(long_vec, as.double(zipcodes[zipcodes$zip==nyc_gas_data[i]$zip_code,]$lng))

        cityBuffer <- zipcodes[zipcodes$zip==nyc_gas_data[i]$zip_code,]$city
        if (cityBuffer != "Brooklyn" && cityBuffer != "New York" && cityBuffer != "Bronx" && cityBuffer != "Staten Island") {
            borough  <- append(borough , "Queens")
        }
        else {
            borough  <- append(borough, zipcodes[zipcodes$zip==nyc_gas_data[i]$zip_code,]$city)
        }
    }
}

# for some reason, long_vec would return n+1 rows
length(long_vec) <- 1005

nyc_gas_data$lat <- lat_vec
nyc_gas_data$long <- long_vec
nyc_gas_data$borough <- borough

# ========================================

# ========================================
# ********* Factoring Datasets  **********
nyc_gas_commercial <- c()
nyc_gas_residential <- c()
nyc_gas_l_residential <- c()
nyc_gas_industrial <- c()
nyc_gas_s_residential <- c()
nyc_gas_institutional <- c()

for (i in 1:nrow(nyc_gas_data)){
    if (nyc_gas_data[i]$building_type=="Commercial")
        nyc_gas_commercial <- rbind(nyc_gas_commercial, nyc_gas_data[i])
    else if (nyc_gas_data[i]$building_type=="Industrial")
        nyc_gas_industrial <- rbind(nyc_gas_industrial, nyc_gas_data[i])
    else if (nyc_gas_data[i]$building_type=="Institutional")
        nyc_gas_institutional <- rbind(nyc_gas_institutional, nyc_gas_data[i])
    else if (nyc_gas_data[i]$building_type=="Large Residential")
        nyc_gas_l_residential <- rbind(nyc_gas_l_residential, nyc_gas_data[i])
    else if (nyc_gas_data[i]$building_type=="Small Residential")
        nyc_gas_s_residential <- rbind(nyc_gas_s_residential, nyc_gas_data[i])
    else if (nyc_gas_data[i]$building_type=="Residential")
        nyc_gas_residential <- rbind(nyc_gas_residential, nyc_gas_data[i])
}
# ========================================

# ========================================
# ******* Creating Color Schemes  ********
monoGreen <- c('#83C659', '#699F48', '#4F7837', '#355126', '#1B2A15', '#000000')
complimentGreen <- c('#83C659', '#82AA69', '#818E79', '#807289', '#7F5699', '#7C39A6')
analogGreen <- c('#C69B59', '#C6C059', '#A7C659', '#83C659', '#5EC659', '#59C677')

monoTeal <- c('#29827C', '#216864', '#194E4C', '#113434', '#091A1C', '#000000')
complimentTeal <- c('#29827C', '#4C817E', '#6F8080', '#927F82', '#B57E84', '#D67D83')
analogTeal <- c('#2F8228', '#288240', '#28825E', '#29827C', '#286A82', '#284C82')

monoRed <- c('#AF1D00', '#8C1800', '#691300', '#460E00', '#230900', '#000000')
complimentRed <- c('#AF1D00', '#9C4533', '#896D66', '#769599', '#63BDCC', '#50E2FF')
analogRed <- c('#AF0092', '#AF0057', '#AF001D', '#AF1D00', '#AF5700', '#AF9100')

monoPurple <- c('#7B00AF', '#63008C', '#4B0069', '#330046', '#1B0023', '#000000')
complimentPurple <- c('#7B00AF', '#7D339C', '#7F6689', '#819976', '#83CC63', '#84FF50')
analogPurple <- c('#0034AF', '#0600AF', '#4000AF', '#7B00AF', '#AF00A8', '#AF006E')

monoBlue <- c('#2B54B5', '#234491', '#1B346D', '#132449', '#0B1425', '#000000')
complimentBlue <- c('#2B54B5', '#4D66A0', '#6F788B', '#918A76', '#B39C61', '#D4AB4A')
analogBlue <- c('#2AB58B', '#2AB0B5', '#2A82B5', '#2B54B5', '#302AB5', '#5E2AB5')

monoColorChoices <- c(monoGreen, monoTeal, monoRed, monoPurple, monoBlue)
complimentColorChoices <- c(complimentGreen, complimentTeal, complimentRed, complimentPurple, complimentBlue)
analogColorChoices <- c(analogGreen, analogTeal, analogRed, analogPurple, analogBlue)
# ========================================


ui <- fluidPage(
    # Application title
    div(h1("NYC Natural Gas Consumption by ZIP Code - 2010"), style="text-align: center"),
    div(h2("RShiny App by Henry Andrew Baum - 12/16/2021"), style="text-align: center"),
    sidebarLayout(
        sidebarPanel(
            radioButtons("measurement4", label = h3("Consumption Measurements"),
                             choices = list("Therms" = 1, "GJ (Gigajoules)" = 2), 
                             selected = 1
            ),
            selectInput("colorScheme4", label = h4("Color Scheme"),
                        choices = list("Analogous Green" = 1, 
                                       "Analogous Teal" = 2, 
                                       "Analogous Red" = 3, 
                                       "Analogous Purple" = 4, 
                                       "Analogous Blue" = 5),
                        selected = 1),
            selectInput("mapOptions", "Select a Building Type",
                        list("Commercial" = 1, "Residential" = 2,
                             "Large Residential" = 3, "Small Residential" = 4,
                             "Industrial" = 5, "Institutional" = 6),
                        selected = 1
            ),
            conditionalPanel(
                condition = "input.mapOptions == '1'",
                checkboxInput("commercial", label = "Enable Layer", value = TRUE),
                sliderInput("opacityCommercial", label = h5("Point Opacity"),
                            min = -1,
                            max = 1,
                            step = .01,
                            value = 0),
                sliderInput("sizeCommercial", label = h5("Point Size Adjustment"),
                            min = -1,
                            max = 10,
                            step = .1,
                            value = 0),
            ),
            conditionalPanel(
                condition = "input.mapOptions == '2'",
                checkboxInput("residential", label = "Enable Layer", value = TRUE),
                sliderInput("opacityResidential", label = h5("Point Opacity"),
                            min = -1,
                            max = 1,
                            step = .01,
                            value = 0),
                sliderInput("sizeResidential", label = h5("Point Size Adjustment"),
                            min = -1,
                            max = 10,
                            step = .1,
                            value = 0),
            ),
            conditionalPanel(
                condition = "input.mapOptions == '3'",
                checkboxInput("l_residential", label = "Enable Layer", value = TRUE),
                sliderInput("opacityLResidential", label = h5("Point Opacity"),
                            min = -1,
                            max = 1,
                            step = .01,
                            value = 0),
                sliderInput("sizeLResidential", label = h5("Point Size Adjustment"),
                            min = -1,
                            max = 10,
                            step = .1,
                            value = 0),
            ),
            conditionalPanel(
                condition = "input.mapOptions == '4'",
                checkboxInput("s_residential", label = "Enable Layer", value = TRUE),
                sliderInput("opacitySResidential", label = h5("Point Opacity"),
                            min = -1,
                            max = 1,
                            step = .01,
                            value = 0),
                sliderInput("sizeSResidential", label = h5("Point Size Adjustment"),
                            min = -1,
                            max = 10,
                            step = .1,
                            value = 0),
            ),
            conditionalPanel(
                condition = "input.mapOptions == '5'",
                checkboxInput("industrial", label = "Enable Layer", value = TRUE),
                sliderInput("opacityIndustrial", label = h5("Point Opacity"),
                            min = -1,
                            max = 1,
                            step = .01,
                            value = 0),
                sliderInput("sizeIndustrial", label = h5("Point Size Adjustment"),
                            min = -1,
                            max = 10,
                            step = .1,
                            value = 0),
            ),
            conditionalPanel(
                condition = "input.mapOptions == '6'",
                checkboxInput("institutional", label = "Enable Layer", value = TRUE),
                sliderInput("opacityInstitutional", label = h5("Point Opacity"),
                            min = -1,
                            max = 1,
                            step = .01,
                            value = 0),
                sliderInput("sizeInstitutional", label = h5("Point Size Adjustment"),
                            min = -1,
                            max = 10,
                            step = .1,
                            value = 0),
            ),
        ),
        mainPanel(
            plotlyOutput("citymap")
        )
    ),
    sidebarLayout(
        sidebarPanel(
            radioButtons("measurement1", label = h4("Consumption Measurements"),
                         choices = list("Therms" = 1, "GJ (Gigajoules)" = 2), 
                         selected = 1),
            selectInput("colorScheme1", label = h4("Color Scheme"),
                         choices = list("Analogous Green" = 1, 
                                        "Analogous Teal" = 2, 
                                        "Analogous Red" = 3, 
                                        "Analogous Purple" = 4, 
                                        "Analogous Blue" = 5),
                         selected = 1),
            selectInput("outlierShape", label = h4("Outlier Shape"),
                        choices = list("Circle" = 1, 
                                       "Square" = 2, 
                                       "Diamond" = 3, 
                                       "Triangle" = 4, 
                                       "None" = 5),
                        selected = 1),
            sliderInput("jitter", label = h4("Jitter Point Opacity"),
                        min = 0,
                        max = 1,
                        value = 0)
        ),
        mainPanel(
            plotOutput("boxplot")
        )
    ),
    sidebarLayout(
        sidebarPanel(
            radioButtons("measurement2", label = h4("Consumption Measurements"),
                         choices = list("Therms" = 1, "GJ (Gigajoules)" = 2), 
                         selected = 1),
            checkboxInput("total", label = "Total Value Labels", value = FALSE),
            selectInput("colorScheme2", label = h4("Color Scheme"),
                        choices = list("Analogous Green" = 1, 
                                       "Analogous Teal" = 2, 
                                       "Analogous Red" = 3, 
                                       "Analogous Purple" = 4, 
                                       "Analogous Blue" = 5),
                        selected = 1),
            div(
                h4("Borders"),
                checkboxInput("border", label = "Enabled", value = FALSE),
                sliderInput("borderSize", label=h5("Border Size"),
                                min = 1,
                                max = 3,
                                value = 1),
            ),
            radioButtons("order", label = h4("Order of Bars"),
                         choices = list("Ascending" = 1, "Descending" = 2), 
                         selected = 1),
        ),
        mainPanel(
            plotOutput("barplot")
        )
    ),
    sidebarLayout(
        sidebarPanel(
            radioButtons("measurement3", label = h4("Consumption Measurements"),
                         choices = list("Therms" = 1, "GJ (Gigajoules)" = 2), 
                         selected = 1),
            checkboxInput("total3", label = "Total Value Labels", value = FALSE),
            selectInput("colorScheme3", label = h4("Color Scheme"),
                        choices = list("Analogous Green" = 1, 
                                       "Analogous Teal" = 2, 
                                       "Analogous Red" = 3, 
                                       "Analogous Purple" = 4, 
                                       "Analogous Blue" = 5),
                        selected = 1),
        ),
        mainPanel(
            plotOutput("pie")
        )
    ),
)

server <- function(input, output) {
    output$boxplot <- renderPlot({
        ggplot(nyc_gas_data, 
               aes(x=nyc_gas_data$building_type,
                   y= if (input$measurement1 == 1) nyc_gas_data$consumption_therms else nyc_gas_data$consumption_gj, 
                   fill=nyc_gas_data$building_type))+
            geom_boxplot(
                outlier.shape = if (input$outlierShape == 5) NA else (as.integer(input$outlierShape) + 20),
                outlier.size = 4
            )+
            stat_boxplot(geom = "errorbar") +
            geom_jitter(size=3, alpha=input$jitter, color=complimentColorChoices[(6 + ((as.integer(input$colorScheme1) - 1) * 6))])+
            labs(fill="Building Type")+
            xlab("Building Type")+
            ylab(if (input$measurement1 == 1) "Consumption in Therms" else "Consumption in Gigajouls")+
            scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
            scale_fill_manual(values=analogColorChoices[(1 + ((as.integer(input$colorScheme1) - 1) * 6)):(6 + ((as.integer(input$colorScheme1) - 1) * 6))])
    })
    output$barplot <- renderPlot({
        ggplot(building_type_total,
            aes(x=reorder(building_type_total$building_type,
                if(input$order == 1) building_type_total$consumption_gj
                else -building_type_total$consumption_gj
                ),
                y= if(input$measurement2 == 1) building_type_total$consumption_therms else building_type_total$consumption_gj,
                fill=building_type_total$building_type))+
            geom_bar(stat="identity", color= if (input$border) {
                complimentColorChoices[(6 + ((as.integer(input$colorScheme2) - 1) * 6))]
            }
                else
                    'transparent'
                ,size = if (input$border) input$borderSize else 1
            )+
            {
                if (input$total)
                    geom_text(aes(label = if(input$measurement2 == 1)
                        format(building_type_total$consumption_therms, big.mark=',', trim=TRUE)
                        else format(building_type_total$consumption_gj, big.mark=',', trim=TRUE)
                    ), vjust=-1)
            }+
            labs(fill="Building Type")+
            xlab("Building Type")+
            ylab(if (input$measurement2 == 1) "Consumption in Therms" else "Consumption in Gigajouls")+
            scale_fill_manual(values=analogColorChoices[(1 + ((as.integer(input$colorScheme2) - 1) * 6)):(6 + ((as.integer(input$colorScheme2) - 1) * 6))])+
            scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))
    })
    output$pie <- renderPlot({
        ggplot(borough_total, aes(x="", y=consumption_therms, fill=borough)) +
            geom_bar(stat="identity", width=1) +
            coord_polar("y", start=0)+
            geom_col(color = "white", size=2) +
            geom_text(aes(label = 
                if(input$total3)
                    paste0(borough, ": ", format(if (input$measurement3 == 1) consumption_therms else consumption_gj, big.mark=','))
                else
                    label_percent()(if (input$measurement3 == 1) consumption_therms/2763994876 else consumption_gj/291616935)
            ),color ="white", size=5, position = position_stack(vjust=0.5)) +
            scale_fill_manual(values=analogColorChoices[(1 + ((as.integer(input$colorScheme3) - 1) * 6)):(6 + ((as.integer(input$colorScheme3) - 1) * 6))])+
            theme_void()
    })
    output$citymap <- renderPlotly({
        ggmap(get_map(getbb("new york city"), zoom=11, source="stamen", maptype="terrain-background"))+
        {if (input$commercial) {
            geom_point(data=nyc_gas_commercial,
                   aes(x=long, y=lat, Building="Commercial", zip=zip_code, 
                       text=paste0("Consumption: ", if (input$measurement4 == 1) paste0(format(consumption_therms, big.mark=','), " Therms")
                                        else paste0(format(consumption_gj, big.mark=','), " GJs")
                       )
                   ),
                   size = if (input$measurement4 == 1) 
                       (nyc_gas_commercial$consumption_therms / 42747652) + (input$sizeCommercial * (nyc_gas_commercial$consumption_therms / 42747652)) 
                        else (nyc_gas_commercial$consumption_gj / 4510117) + (input$sizeCommercial * (nyc_gas_commercial$consumption_gj / 4510117)),
                   alpha=
                       if (input$measurement4 == 1)
                           (as.numeric(nyc_gas_commercial$consumption_therms) / 10000000) + input$opacityCommercial
                       else
                           (as.numeric(nyc_gas_commercial$consumption_gj) / 800000) + input$opacityCommercial
                   ,color=analogColorChoices[(1 + ((as.integer(input$colorScheme4) - 1) * 6))],
                )
        }else NULL}+
        {if (input$residential) {
            geom_point(data=nyc_gas_residential,
               aes(x=long, y=lat, Building="Residential", zip=zip_code, 
                   text=paste0("Consumption: ", if (input$measurement4 == 1) paste0(format(consumption_therms, big.mark=','), " Therms")
                               else paste0(format(consumption_gj, big.mark=','), " GJs")
                   )
               ),
               size = if (input$measurement4 == 1) 
                   (nyc_gas_residential$consumption_therms / 7945664) + (input$sizeResidential * (nyc_gas_residential$consumption_therms / 7945664)) 
               else (nyc_gas_residential$consumption_gj / 838312) + (input$sizeResidential * (nyc_gas_residential$consumption_gj / 838312)),
               alpha=
                   if (input$measurement4 == 1)
                       (as.numeric(nyc_gas_residential$consumption_therms) / 4500000) + input$opacityResidential
               else
                   (as.numeric(nyc_gas_residential$consumption_gj) / 450000) + input$opacityResidential
               ,color=analogColorChoices[(2 + ((as.integer(input$colorScheme4) - 1) * 6))],
            )
        }else NULL}+
        {if (input$l_residential) {
            geom_point(data=nyc_gas_l_residential,
                       aes(x=long, y=lat, Building="Large Residential", zip=zip_code, 
                           text=paste0("Consumption: ", if (input$measurement4 == 1) paste0(format(consumption_therms, big.mark=','), " Therms")
                                       else paste0(format(consumption_gj, big.mark=','), " GJs")
                           )
                       ),
                       size = if (input$measurement4 == 1) 
                           (nyc_gas_l_residential$consumption_therms / 38846378) + (input$sizeLResidential * (nyc_gas_l_residential$consumption_therms / 38846378)) 
                       else (nyc_gas_l_residential$consumption_gj / 4098510) + (input$sizeLResidential * (nyc_gas_l_residential$consumption_gj / 838312)),
                       alpha=
                           if (input$measurement4 == 1)
                               (as.numeric(nyc_gas_l_residential$consumption_therms) / 15000000) + input$opacityLResidential
                       else
                           (as.numeric(nyc_gas_l_residential$consumption_gj) / 1600000) + input$opacityLResidential
                       ,color=analogColorChoices[(3 + ((as.integer(input$colorScheme4) - 1) * 6))],
            )
        }else NULL}+
        {if (input$s_residential) {
            geom_point(data=nyc_gas_s_residential,
                       aes(x=long, y=lat, Building="Small Residential", zip=zip_code, 
                           text=paste0("Consumption: ", if (input$measurement4 == 1) paste0(format(consumption_therms, big.mark=','), " Therms")
                                       else paste0(format(consumption_gj, big.mark=','), " GJs")
                           )
                       ),
                       size = if (input$measurement4 == 1) 
                           (nyc_gas_s_residential$consumption_therms / 22219097) + (input$sizeSResidential * (nyc_gas_s_residential$consumption_therms / 22219097)) 
                       else (nyc_gas_s_residential$consumption_gj / 2344239) + (input$sizeSResidential * (nyc_gas_s_residential$consumption_gj / 2344239)),
                       alpha=
                           if (input$measurement4 == 1)
                               (as.numeric(nyc_gas_s_residential$consumption_therms) / 19500000) + input$opacitySResidential
                       else
                           (as.numeric(nyc_gas_s_residential$consumption_gj) / 2000000) + input$opacitySResidential
                       ,color=analogColorChoices[(4 + ((as.integer(input$colorScheme4) - 1) * 6))],
            )
        }else NULL}+
        {if (input$industrial) {
            geom_point(data=nyc_gas_industrial,
                       aes(x=long, y=lat, Building="Industrial", zip=zip_code,
                           text=paste0("Consumption: ", if (input$measurement4 == 1) paste0(format(consumption_therms, big.mark=','), " Therms")
                                       else paste0(format(consumption_gj, big.mark=','), " GJs")
                           )
                       ),
                       size = if (input$measurement4 == 1)
                           (nyc_gas_industrial$consumption_therms / 22219097) + (input$sizeIndustrial * (nyc_gas_industrial$consumption_therms / 22219097))
                       else (nyc_gas_industrial$consumption_gj / 2344239) + (input$sizeIndustrial * (nyc_gas_industrial$consumption_gj / 2344239)),
                       alpha=
                           if (input$measurement4 == 1)
                               (as.numeric(nyc_gas_industrial$consumption_therms) / 19500000) + input$opacityIndustrial
                       else
                           (as.numeric(nyc_gas_industrial$consumption_gj) / 2000000) + input$opacityIndustrial
                       ,color=analogColorChoices[(5 + ((as.integer(input$colorScheme4) - 1) * 6))],
            )
        }else NULL}+
        {if (input$institutional) {
            geom_point(data=nyc_gas_institutional,
                       aes(x=long, y=lat, Building="Institutional", zip=zip_code,
                           text=paste0("Consumption: ", if (input$measurement4 == 1) paste0(format(consumption_therms, big.mark=','), " Therms")
                                       else paste0(format(consumption_gj, big.mark=','), " GJs")
                           )
                       ),
                       size = if (input$measurement4 == 1)
                           (nyc_gas_institutional$consumption_therms / 22219097) + (input$sizeInstitutional * (nyc_gas_institutional$consumption_therms / 22219097))
                       else (nyc_gas_institutional$consumption_gj / 2344239) + (input$sizeInstitutional * (nyc_gas_institutional$consumption_gj / 2344239)),
                       alpha=
                           if (input$measurement4 == 1)
                               (as.numeric(nyc_gas_institutional$consumption_therms) / 19500000) + input$opacityInstitutional
                       else
                           (as.numeric(nyc_gas_institutional$consumption_gj) / 2000000) + input$opacityInstitutional
                       ,color=analogColorChoices[(5 + ((as.integer(input$colorScheme4) - 1) * 6))],
            )
        }else NULL}
    })
}


shinyApp(ui = ui, server = server)
