# A script of functions for cleaning and analyzing educational data from Chile
# Author: Benjamin Ortiz Ulloa
# 2016-07-04


# Below function finds the RUT for students by parsing through Last Names in a RUT data.frame
# The student data.frame must have columns Appellido.Paterno, Appellido.Materno, Nombres, and rut_num1
# Must remove all NA values from Appellido.Paterno and Appellido.Materno in student file before using 
# Creates df 'ruts_found' and 'ruts_not_found'. Must remove any variables with similiar names before running
# Setting 'risky == T' returns the same two dfs, but we are now matching the first 5 characters in Nombres
# It is risky because 'JOSE ANDRES' becomes 'JOSE' and can Match with 'JOSEFINA', 'JOSE ANTONIO', etc

adding_rut <- function(missing_dv, rut_df, risky = F){
  require(stringr)
  require(dplyr)
  timestamp()
  
  # The rest of the function will not work if there are NA's in the Paterno or Maternal fields
  # Also guarentees that there is a rut_num1 field -------------------------------------------
  
  missing_dv          <- missing_dv %>% filter(is.na(Appellido.Paterno)==F & is.na(Appellido.Materno)==F)
  missing_dv          <- missing_dv %>% arrange(Appellido.Paterno, Appellido.Materno)
  missing_dv$rut_num1 <- NA
  
  # Will hold subsets of last names ----------------------------------------------------------
  
  indexp <- data.frame(Appellido.Paterno='name')
  indexm <- data.frame(Appellido.Materno='name')
  
  
  # If we are running the risky version of the function, then we are truncating the Nombres field
  # to it's first 5 letters and using them to find a match for a RUT ----------------------------
  
  if ( risky == T) {
    missing_dv$Nombres <- str_trim(str_sub(missing_dv$Nombres, 1, 5))
  }
  
  
  for(i in 1:nrow(missing_dv)) {
    
    # Last names of current observation ---------------------------------------------------------
    
    namep <- missing_dv$Appellido.Paterno[i]
    namem <- missing_dv$Appellido.Materno[i]
    
    # Prevents from iterating through the entire rut file unnecessarily -------------------------
    
    if(is.na(as.character(indexp$Appellido.Paterno[1])) == T | as.character(indexp$Appellido.Paterno[1]) != namep) {
      
      indexp <- rut_df %>% filter(Appellido.Paterno == as.character(missing_dv$Appellido.Paterno[i]))
      
    }
    
    # Function does not work when a name does not exist in the RUT file -------------------------
    
    if(is.na(indexp$Appellido.Paterno[1]) == F) {
      
      # Prevents from iterating through the Paternal subset unnecessarily ----------------------
      
      if(is.na(as.character(indexm$Appellido.Materno[1])) == T | as.character(indexm$Appellido.Materno[1]) != namem) {
        
        indexm <- indexp %>% filter(Appellido.Materno == as.character(missing_dv$Appellido.Materno[i]))
        
      }
      
      # Attach RUT if there is a partial string match for the first name -----------------------
      
      missing_dv$rut_num1[i] <- indexm$rut_num1[str_detect(indexm$Nombres, as.character(missing_dv$Nombres[i]))][1]
      
    }
    
    # Uncomment the below code to see how the function is progressing, may slow down the code, but
    # gives you piece of mind that the code is actually working. This 'adding_rut' function can run 
    # anywhere between 10 and 16 hours.------------------------------------------------------------
    #
    # if(i %% ceiling(nrow(missing_dv)/100) == 0) {
    #   cat(i, 'out of', nrow(missing_dv), 'observations proccessed', '\n')
    #   timestamp()
    # }
  }
  
  #creates a df for ruts found and not found
  ruts_found <<- missing_dv %>% filter(is.na(rut_num1)==F)
  ruts_not_found <<- missing_dv %>% filter(is.na(rut_num1)==T)
  timestamp()
  
  if(risky == T) {
    cat(" Please remember that the 'ruts_found' variable that this function returns may",'\n',
        "contain incorrect information. The ruts are found by partially matching the", '\n',
        "first 5 letters in the Alfabetico's 'Nombres' field with ANY 5 letters in the", '\n', 
        "RUT's 'Nombres' field.", '\n')
  }
}

# Below function identifies how many observations should be in a page of a book of data and identifies 
# which pages in a book do not have that number of observations
# The book file must have the column PAG

incomplete_pages <- function(x){
  
  require(dplyr)
  
  # Data.frame conversion of frequency table
  df <- data.frame(table(x$PAG))
  
  # Identify expected number of observations per page
  should_be <- median(df$Freq)
  
  # Identify which pages don't meet the expected observation count
  missing_pages <- df %>% filter(Freq != should_be)
  names(missing_pages) <- c('Page_Num', 'Observations')
  
  # Return a data frame of missing pages that shows expected number of observations
  missing_pages$Expected_Obs = should_be
  return(missing_pages)
}

missing_pages<- function(x){
  x$PAG <- as.integer(as.character.numeric_version(x$PAG))
  data <-data.frame(table(x$PAG))
  data$Var1 <- as.integer(as.character.numeric_version(data$Var1))
  missing_pages <- c()
  for(i in 1:max(data$Var1)){
    if(is.na(subset(x, x$PAG == i)[1,1]==T)){
      missing_pages[length(missing_pages)+1] <- i
    }
  }
  return(missing_pages)
}



#accepts an alfabetico csv(;) file (excel can be saved into csv) and returns a data.frame the last 6 columns of data 
#because serial number, folder, and file name are deemed unimportant
#set missing_dv to T if you only want to see observations with missing_dv numbers
alfabetico_file <- function(x, missing_dv = F){
  
  data <- read.csv2(x, na.strings = c('', ' '), stringsAsFactors = F)
  data <- data[,c(4,5, 6,7,8,9)]
  names(data) <- c('PAG', 'Numero.Inscripcion','Appellido.Paterno', 'Appellido.Materno', 'Nombres', 'dv')
  if(missing_dv == T){
    data <- data %>% filter(is.na(dv)==T)
  }
  return(data)
}



#accepts a vector of alfabetico file names to combine into a single data.frame
#set missing_dv to T if you only want the observations without dv numbers
combining_multiple_data <- function(x, missing_dv=F){
  
  data <- data.frame()
  for(i in 1:length(x)) {data <- rbind(data, alfabetico_file(x[i], missing_dv))}
  return(data)
  
}

