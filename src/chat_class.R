library(openai)
library(R6)

# messages: Messages takes a list of lists. Role can be System, User and Assistant
# temperature: Sampling temp. min:0 max:2. Low=Focused and deterministic. High=Random.
# presence_penalty: min:-2 max:2. Positive=penalize new tokens based on whether 
#   they appear in the text so far. Increases likelihood of model
#   discussing new topics.
# frequency_penalty: min:-2 max:2. Positive=penalize new tokens based on their existing
#   frequency in the text so far. Decreases likelihood of model.
#   repeating the same line verbatim.
ChatCompletion <- R6::R6Class("ChatCompletion",
    private = list(
        model = "gpt-3.5-turbo",
        messages = list(
            list('name'=NULL, 'role'='user','content'='say hi'),
            list('name'=NULL, 'role'='system','content'='you are a bot'),
            list('name'=NULL, 'role'='assistant','content'=NULL)),
        temperature = 0.2,
        max_tokens = 2048,
        presence_penalty = 0,
        frequency_penalty = 0,
        
        msg_system = "",
        msg_user = "",
        msg_assistant = "",
        chat_history = list(),
        msg_info = "",
        full_history = tibble::tibble(created=NULL, 
            id=NULL, model=NULL, object=NULL, prompt_tokens=NULL, completion_tokens=NULL, total_tokens=NULL, 
            finish_reason=NULL, index=NULL, message.role=NULL, message.content=NULL),

        parse_response_ = function(response, tzone="America/Chicago") {
            choices <- tibble::tibble(response$choices);
            usage <- tibble::tibble(data.frame(response$usage));
            meta <- tibble::tibble(created=lubridate::with_tz(lubridate::as_datetime(response$created), tzone=tzone), 
                                id=response$id, 
                                model=response$model, 
                                object=response$object);
            history <- dplyr::bind_cols(meta, usage, choices);
            return(history);
        },
        get_message_ = function(response_frame) {
            return(response_frame$message.content);
        },
        
        save_msg_ = function(response_frame) {
            private$msg_info <- response_frame;
        },
        get_message_info_ = function() {
            return(private$msg_info);
        },
        
        save_history_ = function(history_frame) {
            private$full_history <- dplyr::bind_rows(private$full_history, history_frame);
        },
        get_full_history_ = function() {
            return(private$full_history);
        },
        get_chat_history_ = function() {
            return(private$chat_history);  
        },
        
        get_model_ = function() {
            return(private$model);
        },
        get_messages_ = function() {
            return(private$messages);
        },
        get_temperature_ = function() {
            return(private$temperature);
        },
        get_max_tokens_ = function() {
            return(private$max_tokens);
        },
        get_presence_penalty_ = function() {
            return(private$presence_penalty);
        },
        get_frequency_penalty_ = function() {
            return(private$frequency_penalty);
        },

        set_model_ = function(model) {
            private$model <- model;
        },
        set_messages_ = function(msg_user='', msg_system='', msg_assistant='') {
            # What we pass to the API
            private$messages <- list(
                list(
                    role='system', content=msg_system
                ),
                list(
                    role='user', content=msg_user
                ),
                list(role='assistant', content=msg_assistant)
            );
        },
        set_temperature_ = function(temperature) {
            private$temperature <- temperature;
        },
        set_max_tokens_ = function(max_tokens) {
            private$max_tokens <- max_tokens;
        },
        set_presence_penalty_ = function(presence_penalty) {
            private$presence_penalty <- presence_penalty;
        },
        set_frequency_penalty_ = function(frequency_penalty) {
            private$frequency_penalty <- frequency_penalty;
        },
        
        set_msg_user_ = function(msg_user) {
            private$msg_user <- msg_user;
        },
        set_msg_system_ = function(msg_system) {
            private$msg_system <- msg_system;  
        },
        set_msg_assistant_ = function(msg_assistant) {
            private$msg_assistant <- msg_assistant;  
        },
        
        handle_chat_history_ = function(msg) {
            private$chat_history <- append(private$chat_history, msg);
        },
        configure_messages_ = function() {
            # Configure the structure that gets passed to the API
            latest_msg <- list(
                list(role='system', content=private$msg_system),
                list(role='user', content=private$msg_user),
                list(role='assistant', content=private$msg_assistant)
            );   
            
            # What we pass to the API
            private$messages <- latest_msg
            
            private$handle_chat_history_(latest_msg);
        },
        handle_input_msg_ = function(msg_user='', msg_system='', msg_assistant='') {

            # If the user message is new
            if (stringr::str_count(msg_user) > 0) {
                private$set_msg_user_(msg_user);
            }
            if (stringr::str_count(msg_system) > 0) {
                private$set_msg_system_(msg_system)
            }
            if (stringr::str_count(msg_assistant) > 0) {
                private$set_msg_system_(msg_assistant)
            }
            
            private$configure_messages_();
        },
        handle_output_msg_ = function(msg) {
            private$handle_chat_history_(msg);
        }    
    ),
    public = list(
        initialize = function(model="gpt-3.5-turbo", msg_user='', msg_system='', msg_assistant='', temperature=0.2, 
        max_tokens=1000, presence_penalty=0, frequency_penalty=0) {
            private$set_model_(model);
            private$set_temperature_(temperature);
            private$set_max_tokens_(max_tokens);
            private$set_presence_penalty_(presence_penalty);
            private$set_frequency_penalty_(frequency_penalty);
            private$handle_input_msg_(msg_user=msg_user, msg_system=msg_system, msg_assistant=msg_assistant)
        },
        chat = function(msg_user='', msg_system='', msg_assistant='', temperature=0.2, 
        max_tokens=1000, presence_penalty=0, frequency_penalty=0) {
            if ((all(
                stringr::str_count(msg_user),
                stringr::str_count(msg_system),
                stringr::str_count(msg_assistant)) == TRUE) && temperature==0.2 && max_tokens==1000 && presence_penalty==0 && frequency_penalty==0) {
                    response <- openai::create_chat_completion(
                        model = private$get_model_(),
                        messages = private$get_messages_(),
                        temperature = private$get_temperature_(),
                        max_tokens = private$get_max_tokens_(),
                        presence_penalty = private$get_presence_penalty_(),
                        frequency_penalty = private$get_frequency_penalty_()
                    )
                    response_frame <- private$parse_response_(response, "America/Chicago");
                    private$save_history_(response_frame);
                    return(private$get_message_(response_frame));
            }
            private$handle_input_msg_(msg_user, msg_system, msg_assistant);
            private$set_temperature_(temperature);
            private$set_max_tokens_(max_tokens);
            private$set_presence_penalty_(presence_penalty);
            private$set_frequency_penalty_(frequency_penalty);

            response <- openai::create_chat_completion(
                model = private$get_model_(),
                messages = private$get_messages_(),
                temperature = private$get_temperature_(),
                max_tokens = private$get_max_tokens_(),
                presence_penalty = private$get_presence_penalty_(),
                frequency_penalty = private$get_frequency_penalty_()
            )
            response_frame <- private$parse_response_(response, "America/Chicago");
            private$save_msg_(response_frame);
            private$save_history_(response_frame);
            msg <- private$get_message_(response_frame);
            private$handle_output_msg_(msg)
            return(msg);
        },

        get_message_info = function() {
            return(private$get_message_info_());
        },
        get_history = function() {
            return(private$get_full_history_());
        },
        
        set_temperature = function(temperature) {
            private$set_temperature_(temperature);
        },
        set_max_tokens = function(max_tokens) {
            private$set_max_tokens_(max_tokens);
        },
        set_presence_penalty = function(presence_penalty) {
            private$set_presence_penalty_(presence_penalty);
        },
        set_frequency_penalty = function(frequency_penalty) {
            private$set_frequency_penalty_(frequency_penalty);
        },

        change_model = function(model_name) {
            private$set_model_(model_name);
        },
        
        get_messages = function() {
            private$get_messages_();
        },
        get_chat_history = function() {
            return(private$get_chat_history_());
        }
    )
)

