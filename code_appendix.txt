
############### MINIMAL REPRODUCIBLE CODE FOR CUSTOMER NEBULA MATRIX IN R ############### 

library(magrittr)
library(tidyr)
library(dplyr)
library(shiny)
library(plotly)
library(scales)


#### PART 1: DATA TRANSFORMATION ####


# Customer names, tonnes, gross_margin are made up. Any similarities to real companies are purely coincidental

df <- data.frame(customer=rep(c("SUPER SPARKLING WINE SARL","CHEER BEERS SA", "MOONSHOT SPIRITS LTD", "YOLOGURT CO."), each=4),
                 year=rep(c(2023, 2023, 2024, 2024), times=2),
                 tonnes=c(14, 6, 18, 7, 22, 9, 38, 4, 30, 26, 38, 77, 29, 34, 102, 56),
                 gross_margin = c(200,300,350,500,1200,1300,1350, 1500, 300,400,650,500,300,1300,1350, 1500))

# View original data dataframe
df

# Calculate volume growth for each customer

volume_growth <- df %>% group_by(customer, year) %>%
  summarise(tonnes = sum(tonnes, na.rm = T)) %>% 
  ungroup() %>%
  pivot_wider(names_from = year, values_from = tonnes, 
              names_prefix = "volume_", values_fill = 0) %>%
  mutate(volume_growth = (volume_2024/volume_2023 - 1)) 

# View invidual customer's volume growth
volume_growth

# Calculate volume growth for all customers
# Total volume for 2023
v2023 <- df %>%
  filter(year == 2023) %>%
  summarise(tonnes = sum(tonnes, na.rm = T)) %>%
  pull()
# Total volume for 2024
v2024<- df %>%
  filter(year == 2024) %>%
  summarise(tonnes = sum(tonnes, na.rm = T)) %>%
  pull()

volume_growth_all <- v2024/v2023 - 1

# Transform df into a new df with new columns with pivot wider
# New columns follow the format: keymetric_year e.g,  gmpt_2023, gmpt_2024 ...

transformed_df <- df %>%
  group_by(customer, year) %>%
  summarise(volume = sum(tonnes),
            gm = sum(gross_margin),
            current_revenue = sum(gross_margin)*3, 
            target_revenue = current_revenue * 10) %>%
  mutate(actual_ppt = current_revenue / volume,
         target_ppt = target_revenue / volume,
         price_quality = (actual_ppt / target_ppt) - 1,
         gmpt = gm / volume,
         price_quality_rating = ifelse(price_quality >= 0.05, "High (PQ >= 5%)", 
                                       ifelse(price_quality >= -0.05 & price_quality < 0.05, "Medium (-5% <= PQ < 5%)", "Low (PQ < -5%)") ),
         price_quality_rating_factor = factor(price_quality_rating, levels = c("High (PQ >= 5%)", "Medium (-5% <= PQ < 5%)", "Low (PQ < -5%)")))  %>%
  select(customer, year, volume, gmpt, price_quality_rating, price_quality_rating_factor) %>%
  pivot_wider(names_from = year, values_from = c(volume, gmpt, price_quality_rating, price_quality_rating_factor)) 

# View the transformed dataframe
transformed_df

# Use left join to append the volume_growth column to transformed dataframe
transformed_df <- left_join(transformed_df, volume_growth)


# view the final dataframe ready for visualization
transformed_df

#### PART 2: DATA VISUALIZATION ####

# Setting the label style for 
font <- list(
  size = 14,
  color = toRGB("grey"))
label <- list(align = "left",
              font = font,
              bordercolor = "transparent",
              namelength = 3)

# shiny
ui <- fluidPage(
  # selectInput("choice", "Choose", choices = df$year, selected = NULL),
  plotlyOutput("graph")
)

server <- function(input, output, session){
  
  output$graph <- renderPlotly({
    plot_ly(df_2, x = ~df_2$gmpt_2023, y = ~df_2$volume_growth, type = 'scatter', 
            mode = 'markers') %>% 
            #text = ~df_2$customer,
            #textfont = list(color = '#000000', size = 16)) 
    add_annotations(
      x =~df_2$gmpt_2023, y = ~df_2$volume_growth,
      showarrow = F,
      xref = "x",
      yref = "y",
      text = ~df_2$customer,
      xanchor = "left") %>%
      # horizontal
    add_segments(x = 30, xend = 300, 
                 y = sum(df_2$volume_2024)/sum(df_2$volume_2023) - 1,
                 yend = sum(df_2$volume_2024)/sum(df_2$volume_2023) - 1,
                 name = "average",
                 hoverlabel = label,
                 #showlegend = T,
                 #text = "average",
                 #mode = "lines + text",
                 #hoverinfo = "text",
                 line = list(color = toRGB("maroon"), dash = "dot")) %>%
      # vertical
    add_segments(x = sum(df_2$gmpt_2023),
                 xend = sum(df_2$gmpt_2023),
                 y = -1.1,
                 yend = 1.1,
                 hoverlabel = label,
                 line = list(color = toRGB("steelblue4"), dash = "dot")) 
  })
}

shinyApp(ui, server)
