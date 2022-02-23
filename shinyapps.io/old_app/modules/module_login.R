# UI component
login_ui <- function(id, title) {
  
  ns <- NS(id) # namespaced id
  
  # define ui part
  div(
    id = ns("login"),
    style = "width: 500px; max-width: 100%; margin: 0 auto;",
    
    div(
      class = "well",
      
      h4(class = "text-center", title),
      p(class = "text-center", 
        tags$small("Second approach login form")),
      
      textInput(
        inputId     = ns("ti_user_name_module"),
        label       = tagList(icon("user"), 
                              "User Name"),
        placeholder = "Enter user name"
      ),
      
      passwordInput(
        inputId     = ns("ti_password_module"), 
        label       = tagList(
          icon("unlock-alt"), 
          "Password"
        ), 
        placeholder = "Enter password"
      ), 
      
      div(
        class = "text-center",
        actionButton(
          inputId = ns("ab_login_button_module"), 
          label   = "Log in",
          class   = "btn-primary"
        )
      )
    )
  )
}


# SERVER component
validate_pwd <- function(input, output, session, 
                         data, user_col, pwd_col) {
  
  # get user and pwd from data/ user_col/ pwd_col information
  user <- data %>% pull({{ user_col }}) 
  pwd  <- data %>% pull({{ pwd_col }}) 
  
  # check correctness
  eventReactive(input$ab_login_button_module, {
    
    validate <- FALSE
    
    if (input$ti_user_name_module == user &&
        input$ti_password_module == pwd) {
      validate <- TRUE
    }
    
    # hide login form when user is confirmed
    if (validate) {
      shinyjs::hide(id = "login")
    }
    
    validate
  })
}