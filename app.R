# load all dependencies, functions, data and application server and ui
pkgload::load_all(".")

# launch the app by running :
fabryka()

# or:
shiny::runApp(fabryka())