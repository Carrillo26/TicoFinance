library(R6)
library(shiny)
source("ui_manager.R")
source("server_manager.R")

MainApp <- R6Class("MainApp",
                   public = list(
                     ui_manager = NULL,
                     server_manager = NULL,
                     
                     initialize = function() {
                       self$ui_manager <- UIManager$new()
                       self$server_manager <- ServerManager$new()
                     },
                     
                     run_app = function() {
                       shinyApp(ui = self$ui_manager$get_ui(), server = self$server_manager$get_server())
                     }
                   )
)

app <- MainApp$new()
app$run_app()

