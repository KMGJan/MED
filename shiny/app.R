library(colourpicker)
library(tidyverse)
library(ggplot2)
library(shiny)
library(PlanktonData)

df <- PlanktonData::phytoplankton |> mutate(Group = "Phytoplankton") |> 
  rbind(PlanktonData::zooplankton) |> 
  mutate(Group = ifelse(Group == "Phytoplankton", "phytoplankton", "zooplankton")) |> 
  group_by(Month_abb, Station, Taxa, Group) |> 
  summarise(average = mean(Biomass, na.rm = T)) |> 
  ungroup() |> 
  mutate(Month_abb= factor(Month_abb, levels=month.abb),
         Station = factor(Station, levels = c("BY31", "BY15", "BY5"))) |> 
  filter(Group == "phytoplankton") 

ui <- pageWithSidebar(
  headerPanel("ggplot explorator"), # header
  sidebarPanel(
    tabsetPanel(
      tabPanel("Step 1",
               selectInput(inputId = "df", label = "Select your dataset", choices = c("zooplankton", "phytoplankton")),
               textInput(inputId = "in_name", label = "Enter your name", value = "me"),
               textInput(inputId = "caption", label = "Enter your caption", value = "Caption"),
               textInput(inputId = "title", label = "Enter the plot title", value = "Title"),
               textInput(inputId = "x", label = "x-axis label", value = "Month"),
               textInput(inputId = "y", label = "y-axis label", value = "Biomass"),
               selectInput(inputId = "in_plot_legend_position", label = "Legend position", choices = c("bottom", "left", "top", "right")),
               selectInput(inputId = "in_plot_legend_direction", label = "Legend direction", choices = c("horizontal", "vertical"))),
      tabPanel("Step 2",
               colourInput(inputId = "in_plot_title", label = "Plot title", value = "#111111"),
               colourInput(inputId = "in_plot_axis_title", label = "Axis title", value = "#111111"),
               colourInput(inputId = "in_plot_axis_text", label = "Axis text", value = "#111111"),
               colourInput(inputId = "in_plot_strip_text", label = "Strip text", value = "#111111"),
               colourInput(inputId = "in_plot_panel_border", label = "Panel border", value = "#111111"),
               colourInput(inputId = "in_plot_subtitle", label = "Plot subtitle", value = "#111111"),
               colourInput(inputId = "in_plot_legend_title", label = "Legend title", value = "#552211"),
               colourInput(inputId = "in_plot_legend_text", label = "Legend text", value = "#D46444")),
      tabPanel("Step 3",
               colourInput(inputId = "in_plot_panel_background", label = "Panel background", value = "#EEEEEE"),
               colourInput(inputId = "in_plot_legend_background", label = "Legend background", value = "#EEEEEE"),
               colourInput(inputId = "in_plot_legend_box_background", label = "Legend box background", value = "#EEEEEE"),
               colourInput(inputId = "in_plot_strip_background", label = "Strip background", value = "#EEEEEE")))),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")),
      tabPanel("Code", verbatimTextOutput("code")) # Display code here
    )
  )
)

server <- function(input,output){
  
  output$plot <- renderPlot({
    ggplot(df, aes(x = Month_abb, y = average, fill = Taxa)) +
      geom_bar(stat = "identity") +
      facet_wrap(.~Station) +
      labs(title = input$title, caption = paste(input$caption,  "by", input$in_name, sep = " "), x = input$x, y = input$y) +
      theme_classic(base_size = 16) +
      theme(
        plot.title=element_text(color=input$in_plot_title),
        plot.subtitle=element_text(color=input$in_plot_subtitle),
        legend.title=element_text(color=input$in_plot_legend_title),
        legend.text=element_text(color=input$in_plot_legend_text),
        legend.position=input$in_plot_legend_position,
        legend.direction = input$in_plot_legend_direction,
        axis.title=element_text(color=input$in_plot_axis_title),
        axis.text=element_text(color=input$in_plot_axis_text),
        strip.text=element_text(color=input$in_plot_strip_text),
        strip.background = element_rect(fill = input$in_plot_strip_background),
        legend.background = element_rect(fill = input$in_plot_legend_box_background),
        legend.box.background = element_rect(color = input$in_plot_legend_box_background),
        panel.border = element_rect(fill = NA, color = input$in_plot_panel_border),
        panel.background = element_rect(fill = input$in_plot_panel_background),
        
        
        
      )
  })
  
  output$code <- renderPrint({
    code <- 
      paste(
        "
      library(tidyverse)
      library(PlanktonData)
      df <- PlanktonData::phytoplankton |> 
      group_by(Month_abb, Station, Taxa, Group) |> 
      summarise(average = mean(Biomass, na.rm = T)) |> 
      ungroup() |> 
      mutate(Month_abb= factor(Month_abb, levels=month.abb)))
      
      ggplot(df, aes(x = Month_abb, y = average, fill = Taxa)) +
      geom_bar(stat = 'identity') +
      facet_wrap(. ~ Station) +
      labs(
        title =", input$title,",
        caption = ", paste(input$caption, 'by', input$in_name, sep = ' '),",
        x = ",input$x,",
        y = ",input$y,") +
      theme(
        plot.title = element_text(color =",input$in_plot_title,"),
        plot.subtitle = element_text(color =",input$in_plot_subtitle,"),
        legend.title = element_text(color =",input$in_plot_legend_title,"),
        legend.text = element_text(color =",input$in_plot_legend_text,"),
        legend.position =",input$in_plot_legend_position,",
        legend.direction =",input$in_plot_legend_direction,",
        axis.title = element_text(color =",input$in_plot_axis_title,"),
        axis.text = element_text(color =",input$in_plot_axis_text,"),
        strip.text = element_text(color =",input$in_plot_strip_text,"),
        strip.background = element_rect(fill =",input$in_plot_strip_background,"),
        legend.background = element_rect(fill =",input$in_plot_legend_box_background,"),
        legend.box.background = element_rect(color =", input$in_plot_legend_box_background,"),
        panel.border = element_rect(color =", input$in_plot_panel_border,"),
        panel.background = element_rect(fill =", input$in_plot_panel_background, "))", sep = " ")
    
    cat(code)
  })
  
}

shinyApp(ui=ui,server=server)