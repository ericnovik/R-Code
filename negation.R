library("stringr")

negate <- function(s) {
  if(str_detect(s, "not")) {
    new_s <- str_replace(s, "not ", "")
  } else {
    first_two <- word(s, 1, 2)
    third_person <- c(" he ", " she ", " it ", "He ", "She ", "It ")
    if(sum(str_detect(s, third_person)) == 0) {
      if(str_detect(s, "will")) { 
        new_s <- str_replace(s, first_two, str_c(first_two, " not"))
      } else {
        new_s <- str_replace(s, first_two, str_c(first_two, " do not"))
      }
    } else {
      first_two <- word(s, 1, 2)
      third <- word(s, 3)
      first_three <- str_c(first_two, " ", third)
      third <- str_sub(third, 1, str_length(third) - 1)
      new_s <- str_replace(s, first_three, str_c(first_two, " does not ", third))
    }
  }
  return(new_s)
}

a <- "If I write every day for a year"
negate(a)

b <- "I will improve my writing"
negate(b)

# counterpositive


TRUE %=>% FALSE



a <- "If I do not quit my job"
negate(a)

# fail
a <- "I will quit my job"
negate(a)

a <- "He often acts like an idiot"
negate(a)

a <- "She does not like to play"
negate(a)

a <- "If I love my country"
negate(a)

a <- "I will not go to war"
negate(a)



source("implication.R")
test_file("implication.R")

sentences <- c("Jane saw a cat", "Jane sat down")
word(sentences, 1, 2)
