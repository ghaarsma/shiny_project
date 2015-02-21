library(shiny)
library(ggplot2)


age_density <- function(stat,DBtitle,type,year) {
  theme <- theme_grey(base_size = 18)
  
  lim <- c(13,60)
  year.lim <- c(min(stat$Year),max(stat$Year))
  
  Mode <- 1
  if (!is.null(stat$Gender)) {Mode=2}
  if (!is.null(stat$Race)) {Mode=3}
  if (!is.null(stat$Race) && !is.null(stat$Gender)) {Mode=4}
  
  if (type=='Plot Lines') {
    p <- ggplot(data=stat,aes(x=Age,y=DensityCC)) +
      geom_path(aes(group = Year,colour=Year),stat = "identity", position = "identity",na.rm = TRUE) +
      #scale_colour_gradient(limits=c(1977,2012),low="red",high='blue', space = "rgb", na.value = "grey50", guide = "colourbar",name='Density') +
      scale_colour_gradient2(limits=c(year.lim[1],year.lim[2]),low="red",mid='green',high='blue',midpoint=mean(year.lim), space = "rgb", na.value = "grey50", guide = "colourbar",name='Year') +
      ylab('Density') +
      xlim(lim) +
      
      theme +
      ggtitle(paste(DBtitle,': Age at the time of arrest')) +
      
      switch(Mode,NULL,facet_wrap(~ Gender),facet_wrap(~ Race),facet_grid(Gender ~ Race)) 
  }
  
  if (type=='Heat Map') {
    p <- ggplot(data=stat,aes(x=Age, y=Year, fill = DensityCC)) +
      geom_tile() +
      scale_x_continuous(limits=lim,expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_gradientn(colours=c("#000080","#0000F1","#0063FF","#00D4FF","#47FFB8","#B8FF47","#FFD400","#FF6300","#F10000","#800000"), guide = "colourbar",name='Density') +
      
      theme +
      ggtitle(paste(DBtitle,': Age at the time of arrest')) +
      
      switch(Mode,NULL,facet_wrap(~ Gender),facet_wrap(~ Race),facet_grid(Gender ~ Race)) 
    
  }
  if (type=='Animate Lines') {
    p <- ggplot(data=stat[stat$Year==year,],aes(x=Age,y=DensityCC)) +
      switch(Mode,geom_path(na.rm=TRUE),geom_path(aes(colour=Gender),na.rm=TRUE),geom_path(aes(colour=Race),na.rm=TRUE),geom_path(aes(colour=Race),na.rm=TRUE)) +

      ylab('Density') +
      xlim(lim) +
      ylim(c(0,0.08)) +
      theme +
      ggtitle(paste(DBtitle,': Age at the time of arrest in',year))
      
    if (Mode==4) p <- p + facet_wrap(~ Gender)

  }
  
  return(p)
}

load('nyc_stat.RData')


shinyServer(
  function(input, output) {
    output$agedist <- renderPlot({
      indx <- c('Gender','Race') %in% input$AnalyzeId

      if (!indx[1] && !indx[2]) {stat=stat1}
      if ( indx[1] && !indx[2]) {stat=stat2}
      if (!indx[1] &&  indx[2]) {stat=stat3}
      if ( indx[1] &&  indx[2]) {stat=stat4}
      
      Type <- input$PlotId
      Year <- input$YearId
      gg <- age_density(stat,DBtitle = 'New York City',type = Type,year=Year)
      
      print(gg)
    })

    output$sliderInputUI <- renderUI({
      if (input$PlotId=='Animate Lines') {
        column(4,offset = 2,
               sliderInput('YearId','Year',min=1977,max=2012,value=1977,step = 1,round = TRUE,animate = animationOptions(interval = 750))
        )
      }
    })
    
  }
)


