R_to_Julia <- function(f){
  deparsed_f <- deparse(f)
  for(i in 1:length(deparsed_f)){
    if(grepl("for", deparsed_f[i])){
      deparsed_f[i] <- sub("\\((.*)\\)","\\1", deparsed_f[i]) # the forloop deals with for loops
    }
  }
  f_code <- paste(deparsed_f,collapse = ";") # turns the function into a single string with semi-colons
  f_code <- gsub("\\{", "", f_code) # gets rid of opening brackets
  f_code <- gsub("else if", "elseif", f_code) # translates elseif to julia 
  f_code <- gsub("\\};\\s*else",";else",f _code) # get rid of closing brackets next to else
  f_code <- gsub("\\}", "; end", f_code) # turns closing brackets to end
  f_code <- gsub("\\$", "\\.", f_code) # turns $ into . for accessing named lists
  f_code <- gsub("\\*"," \\.\\*", f_code) # adds . to make multiplication broadcast by default
  f_code <- gsub("% \\.\\*%"," \\*", f_code) # translates matrix multiplication
  f_code <- gsub("<\\-", "=", f_code) # translates <- to =
  f_code <- gsub("=\\s*","=", f_code)  # removes spaced before equal signs to get the next line to work 
  f_code <- gsub("[^\\^\\+\\*\\-\\/=)]\\-"," \\.\\-", f_code) # broadcasts subtraction unless it is creating a negative number  
  f_code <- gsub("\\+", " \\.\\+", f_code) # broads casts addition 
  f_code <- gsub("/", " \\./", f_code) # broadcasts division
  f_code <- gsub("\\^", " \\.\\^", f_code) # broadcasts exponentiation
  f_code <- gsub("c\\(([^()]*)\\)", "[\\1]", f_code) # converts c() to []
  return(f_code)
} 