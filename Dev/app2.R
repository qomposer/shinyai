
library(openai)

library(shiny)
library(shiny.fluent)
#library(httr)
#library(jsonlite)

# Set up OpenAI API key
Sys.setenv(OPENAI_API_KEY = "")

# Shiny app UI
ui <- fluidPage(
    titlePanel("Shiny AI"),
    sidebarLayout(
        sidebarPanel(
            textInput("prompt", "Enter your prompt:", ""),
            actionButton("run", "Run")
        ),
        mainPanel(
            verbatimTextOutput("completion")
        )
    )
)

# Shiny app server
server <- function(input, output) {
    output$completion <- renderPrint({
        # Wait for the run button to be clicked
        input$run
        
        # Get the prompt from the text input
        prompt <- input$prompt
        
        
        ai_resp <- openai::create_chat_completion(
            model = "gpt-3.5-turbo",
            messages = list(
                list('role' = 'system', 'content' = 'You are an AI assistant'),
                list('role' = 'user', 'content' = prompt)
            ),
            temperature = 0.1,
            max_tokens = 3000
        )
        
        ai_resp$choices$message.content
        
    })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)