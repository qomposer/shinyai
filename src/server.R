# server.R
# The app server
library(shiny)
library(shiny.fluent)
source("chat_class.R")

server <- function(input, output) {
    # Instantiate the ChatBot
    ChatBot <- ChatCompletion$new();

    # Update ChatBot properties based on user inputs
    observeEvent(input$system_input, {
        ChatBot$set_msg_system(input$system_input);
    })

    observeEvent(input$user_input, {
        ChatBot$set_msg_user(input$user_input);
    })

    # Chat with the bot when the user interacts
    observeEvent(c(input$system_input, input$user_input), {
        bot_response <- ChatBot$chat();
        
        updateTextAreaInput(session, "bot_response", value = bot_response);
    })
}