# ui.R
# User interface for the chat gpt app
library(shiny)
library(shiny.fluent)

ui <- fluentPage(
    
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "system_input", label = "System Message:"),
            textInput(inputId = "user_input", label = "System Message:")
        ),

        mainPanel(
            h3("Bot's Response"),
            textAreaInput(inputId = "bot_response", label = "Bot Response", value = "", rows = 5, placeholder = "")
        )
    )
)