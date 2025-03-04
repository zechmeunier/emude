julia_eval("include(\"src/helpers.jl\")")
R_to_Julia <- function(f, filepath = NULL){
  deparsed_f <- deparse(f)
  
  if(!is.null(filepath)) {
    deparsed_f[1] <- gsub("function ", "function derivs", deparsed_f[1])
  }

  for(i in 1:length(deparsed_f)){
    if(grepl("for", deparsed_f[i])){
      deparsed_f[i] <- sub("\\((.*)\\)","\\1", deparsed_f[i]) # the forloop deals with for loops
    }
  }
  f_code <- paste(deparsed_f,collapse = ";") # turns the function into a single string with semi-colons
  f_code <- gsub("\\{", "", f_code) # gets rid of opening brackets
  f_code <- gsub("else if", "elseif", f_code) # translates elseif to julia 
  f_code <- gsub("\\};\\s*else",";else",f_code) # get rid of closing brackets next to else
  f_code <- gsub("\\}", "; end", f_code) # turns closing brackets to end
  f_code <- gsub("\\$", "\\.", f_code) # turns $ into . for accessing named lists
  f_code <- gsub("\\*"," \\.\\*", f_code) # adds . to make multiplication broadcast by default
  f_code <- gsub("% \\.\\*%"," \\*", f_code) # translates matrix multiplication
  f_code <- gsub("<\\-", "=", f_code) # translates <- to =
  f_code <- gsub("=\\s*","=", f_code)  # removes space before equal signs to get the next line to work 
  f_code <- gsub("[^\\^\\+\\*\\-\\/=)]\\-"," \\.\\-", f_code) # broadcasts subtraction unless it is creating a negative number  
  f_code <- gsub("\\+", " \\.\\+", f_code) # broads casts addition 
  f_code <- gsub("/", " \\./", f_code) # broadcasts division
  f_code <- gsub("\\^", " \\.\\^", f_code) # broadcasts exponentiation
  f_code <- gsub("c\\(([^()]*)\\)", "[\\1]", f_code) # converts c() to []
  
  if(!is.null(filepath)) {
    fileConn<-file(paste0(filepath,".jl"))
    writeLines(sapply(strsplit(f_code, split=';;', fixed=TRUE),identity), fileConn)
    close(fileConn)
  }
  
  return(f_code)
} 

extract_model_parameters <- function(model){
  parameters <- julia_eval("retrieve_model_parameters(",model,")",
                           need_return = "R")
  return(parameters)
}