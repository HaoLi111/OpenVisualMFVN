#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

contour.function<-function(x,xbase=seq(from = -5,to=5,by=.1),ybase=seq(from = -5,to=5,by=.1),add=F){
  contour(x=xbase,y=ybase,z=Mapply(xbase,ybase,x),add = add)
}
#input: x,y: vector (1D), independent variable
#Output:
Mapply<-function(x = get("x", envir = .GlobalEnv),
                 y = get("y",envir = .GlobalEnv),
                 f = get("f",envir = .GlobalEnv)){
  if (is.list(x) | is.data.frame(x)){
    y<-x$y
    x<-x$x
  }
  re<-matrix(nrow=length(x),ncol=length(y))
  if(is.function(f)){
    for(i in 1:length(x)){
      re[i,]=f(x[i],y)
    }
  }else{
    if(is.character(f)) f<-parse(text = f)
    xl<-x
    for(i in seq_along(xl)){
      x<-xl[i]
      re[i,]<-as.vector(eval(f))
    }
  }
  return(re)
}


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("OpenVisualMFVN_implicit_plot based on MFVN"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textInput("con",label = "Condition",value = 'abs(sin(x)+tan(y))<.5')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      contour.function(x=input$con)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

