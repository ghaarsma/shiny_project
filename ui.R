library(shiny)



shinyUI(fluidPage(
  title = "NYC: Age distribution at time of arrest",
  plotOutput('agedist',height = 600),
  hr(),
  fluidRow(
    column(2,
           checkboxGroupInput('AnalyzeId',label =  'Analyze', choices = c('Gender','Race'),selected = NULL)
    ),
    column(2,
           #           radioButtons('PlotId',label = 'Figure Type',choices = c('Plot Lines','Heat Map','Slider'))
           #           checkboxGroupInput('PlotId',label = 'Figure Type',choices = c('Plot Lines','Heat Map','Slider'),selected = 'Lines'),
           selectInput('PlotId',label = 'Figure Type',choices = c('Plot Lines','Heat Map','Animate Lines'),selected = 'Lines')
           #           sliderInput('YearId','Year',min=1977,max=2012,value=1977,step = 1,round = TRUE)
    ),
    htmlOutput("sliderInputUI"),
    if (FALSE) {column(4,offset = 2,
           sliderInput('YearId','Year',min=1977,max=2012,value=1977,step = 1,round = TRUE,animate = animationOptions(interval = 750))
    )}
  ),
  helpText("From 9.8 million crime records in New York City from 1977 through 2012, this interactive graphic shows the density estimation",
           "of defendants age at the time of arrest for a given year. This is also known as an age crime curve. Use the interactive menu",
           "to analyze the data based on gender and/or race and see how the age crime curves changes over time.")
))