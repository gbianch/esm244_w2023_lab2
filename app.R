library(shiny)
library(tidyverse)
library(palmerpenguins)

### Create user interface
ui <- fluidPage(
  titlePanel("I am adding a title"),
  sidebarLayout(
    sidebarPanel("Put my widgets here",
                 radioButtons(inputId = "penguin_species",
                              label = "Choose penguin species!",
                              choices = c("Adelie", "Gentoo", "Cool Chinstrap" = "Chinstrap")), # left value is displayed
                ### select input box
                  "Choice a color",
                 selectInput(inputId = 'pt_color',
                             label = "choose your favorite color!",
                             choices = c("Awesome red" = "red",
                                         "Pretty purple" = "purple",
                                         "cornflowerblue",
                                         "orange"))
                 ), ### end sidebarPanel

    mainPanel("put my graph here!",
              plotOutput(outputId = "penguin_plot"),
              tableOutput(outputId = "penguin_table")
              ) ### end mainPanel

  ) ### end sidebarLayout

) ### end fluidPage

### Create the server function
server <- function(input, output) {
  penguin_select <- reactive({ ### whatever is in reactive function()
    penguins %>%
      filter(species == input$penguin_species) # penguin species comes from inputId in radio buttons
  })

  penguin_table <- reactive({
    penguins %>%
      filter(species == input$penguin_species) %>%
      group_by(sex) %>%
      summarize(mean_flip = mean(flipper_length_mm),
                mean_mass = mean(body_mass_g))
  }) ### end penguin table

  output$penguin_plot <- renderPlot({
    ggplot(data = penguin_select(),
           aes(x = flipper_length_mm, y = body_mass_g )) +
      geom_point(color = input$pt_color) + ### change color based on selection
      theme_minimal()
  })

  output$penguin_table <- renderTable({
    penguin_table()
  })
}


### Combine these into an app
shinyApp(ui = ui, server = server)
