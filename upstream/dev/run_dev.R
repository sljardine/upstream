
# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Update this if you want the app to be served on a port other than 8000
options(shiny.port = 8000)

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
# Add profiling from profvis, wrapping run_app in print statement per golem issue #146
profvis::profvis({
  print(run_app())
})
