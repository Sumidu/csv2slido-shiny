library(tidyverse)
library(here)
library(tokenizers)
files <- dir(path=here("data"), pattern = "*.csv")

files[[1]]

raw_data <- read_csv(here("data", files[[1]]))

prep_data <- raw_data |> 
  select("Gruppe", "Texteingabe online", Bewertung) |>
  mutate(Bewertung = 0) |> 
  unique() |> 
  na.omit()

submissions <- prep_data |> pull("Texteingabe online") 

submissions[1]


#' Title
#'
#' @param str 
#' @param seperator 
#'
#' @return
#' @export
#'
#' @examples
extract_question <- function(str, seperator = "\t") {
  str_split(str, seperator)[[1]][1]
}

extract_answers <- function(str, seperator = "\t") {
  str_split(str, seperator)[[1]][-1]
}

remove_junk <- function(str, pattern = " - "){
  str_remove(str, pattern) |> str_squish()
}

# function to show escaped special characters (e.g. \t, &nbsp, etc.) of a string in escaped form (e.g. \\t, \\&nbsp, etc.)
show_special_chars <- function(str) {
  stri_escape_unicode(str)
}


separators <- c("\\t", "\\u2060", "- ")
patterns <- c(" - [0-9].", "- ", "XXX")


result_df <- data.frame()
# try all combinations of separators and patterns
for(sub in submissions) {
  for (qasep in separators) {
    for (asep in separators) {
      for (pat in patterns) {
        #cat(paste("Separator: ", sep, "Pattern: ", pat, ":\t\t"))
        questions <- sub |> extract_question(seperator = qasep) |>
          remove_junk(pattern = pat) |>
          show_special_chars() 
        answers <- sub |> extract_answers(seperator = asep) |> remove_junk(pattern = pat) |> show_special_chars() 
        if (length(answers) == 4 & length(questions) == 1) {
          cat(
            paste("Separator: ", sep, "Pattern: ", pat, "Questions: ", length(questions), "Answers: ", length(answers), "\n",
              sep = " "
            )
          )
          result_df <- rbind(result_df,
                             data.frame(sub, qasep, asep, pat, questions, answers[1], answers[2], answers[3], answers[4]))
        } 
        else {
          result_df <- rbind(result_df,
                             data.frame(sub, qasep, asep, pat, questions, answers.1.="", answers.2.="", answers.3.="", answers.4.=""))
        }
      }
    }
  }
}


library(elmer)

chat <- chat_openai(model = "gpt-3.5-turbo", system_prompt = "You help me split badly formatted questions and answers cleanly into one question and four answers. You keep the input as close as possible, but you remove bullet points, item numbers, and other list markers")

# iterate of all submissions and create a dataframe with the results
res_frame <- data.frame()
for(sub in submissions){
  text <- sub
  res <- chat$chat(paste("Can you help me split this text into one question and four answers. Please do not format the text, except for a carriage return. Keep the text in German:", text), echo = "none")
  res[[1]] |> str_split("\n\n") |> unlist() -> qalist
  question <- qalist[1]
  answers <- qalist[2] |> str_split("\n") |> unlist() |> str_squish() 
  res_frame <- rbind(res_frame, data.frame(sub, question, answers[1], answers[2], answers[3], answers[4]))
}


# test stuff
qasep = separators[2]
asep = separators[2]
pat = patterns[2]
sub <- submissions[2]
sub |> extract_question(seperator = qasep) |> remove_junk(pattern = pat) |> show_special_chars()
sub |> extract_answers(seperator = asep) |> remove_junk(pattern = pat) |> show_special_chars()

