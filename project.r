prep_data <- function(Data) {
  
  if (!is.factor(Data$Year)) {Data$Year <- factor(Data$Year)}
  
  # for Gender, only keep Male & Female, remove Unknown & Missing.
  # For Age, remove any NA's
  l <- c('Male','Female')
  indx <- Data$def.gender %in% l & !is.na(Data$def.age)
  Data <- Data[indx,]
  
  # Then re-factor the race.
  Data$def.gender <- factor(Data$def.gender)

  return(Data)
}

comp_stat <- function(Data,Census.Correction,Type) {
  library(plyr)
  library(stringr)

  Age <- 'def.age'
  Race <- 'def.race'
  Gender <- 'def.gender'
  Year <- 'Year'
  
  df_density <- function(df,Census.Correction) {
    Age <- df[[Age]]
    
    den <- density(Age,from=13,to=70,n=57*4+1)
    
    df.den <- data.frame(Age=den$x,Density=den$y)
    if (!is.null(Census.Correction)) {
      # Find the proper year
      Y <- df[[Year]][1]
      xint <- as.integer(colnames(Census.Correction))
      Indx <- match(floor(Age),xint)
      w <- 1/Census.Correction[as.character(Y),Indx]
      w <- w / sum(w)
      den.w <- density(x=Age,from=13,to=70,n=57*4+1,weights=w)
      
      df.den$DensityCC <- den.w$y
    }
    
    return(df.den)
  }
  
  Var <- Year
  if (Type==2) Var <- c(Var,Gender)
  if (Type==3) Var <- c(Var,Race)
  if (Type==4) Var <- c(Var,Gender,Race)
  
  stat <- ddply(.data=Data,.variables=Var,.fun=df_density,Census.Correction)
  
  stat[[Year]] <- as.numeric(levels(stat[[Year]]))[stat[[Year]]]
  
  names(stat)[str_detect(names(stat),Year)]<-'Year'
  names(stat)[str_detect(names(stat),Gender)]<-'Gender'
  names(stat)[str_detect(names(stat),Race)]<-'Race'
  
  return(stat)
}



if (FALSE) {
  cat('Loading NYC Data..\n')
  load('nyc_data.RData')
  
  cat('Preping NYC Data..\n')
  nyc <- prep_data(nyc)
  
  cat('Computing stats')
  stat1 <- comp_stat(nyc,Census.Correction = yint.nyc,1);cat('.')
  stat2 <- comp_stat(nyc,Census.Correction = yint.nyc,2);cat('.')
  stat3 <- comp_stat(nyc,Census.Correction = yint.nyc,3);cat('.')
  stat4 <- comp_stat(nyc,Census.Correction = yint.nyc,4);cat('\n')

  cat('Saving stats')
  save('stat1','stat2','stat3','stat4',file = 'shiny_project/nyc_stat.RData')
  
  cat('Done!\n')
  

  #age_density(stat,DBtitle = 'NYC',type = 1)
  
}


