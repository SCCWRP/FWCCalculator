library(shiny)
library(shinyvalidate)


# local = TRUE puts the object 'in-line' wherever the source function is called,
# i.e. the scope is limited to that location
# objects in this files are shared across all sessions in the same R process
# objects within the server function are defined each session
# objects within a render function in the server file are defined each time the
# function is called
source('server.R', local = TRUE)
source('ui.R', local = TRUE)


# Run the application
shinyApp(ui = ui, server = server)
