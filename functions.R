##### Contains custom functions

## Libraries
library(tidyverse)

## Custom function

func1<-function(df,n){
  
  tmp <- Filter(is.numeric, df) # we first filter the dataframe for numeric columns
  
  tmp + n # we then add the constant to all the numeric columns
}