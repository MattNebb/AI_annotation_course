###STEP 0 
#install libraries (to run only the first time)
install.packages("dplyr")
install.packages("stringr")
install.packages("devtools")
install.packages("tidyllm")

#activate libraries
library(dplyr)
library(stringr)
library(devtools)
library(tidyllm)

##Set API Key (from your Mistral account: https://console.mistral.ai/api-keys/ )
Sys.setenv(MISTRAL_API_KEY = "")

##############################################

####STEP 1: Upload a dataset
#set your working folder (ideally, where you keep the dataset files and the R file)
working_folder <- ""
setwd(working_folder)
#select the name of the file
file_path <- 'data.csv'

##############################################

####STEP 2: Convert the dataset in a data frame
df <- read.csv(file_path, sep = ",")

#remove all variables but ID, ProfileName, Tone as coded by humans, and Review text
df <- df %>% select(Id, ProfileName, Score, Text)

# Select a random number of 100 rows 
set.seed(1234) # For reproducibility
random_rows <- sample(nrow(df), 100)
df <- df[random_rows, ]

##############################################

####STEP 3: write a prompt with the task
question <- c("
The following is a review written either by a user after buying a product sold on an online store.
Your task is to guess whether the review has a Positive, Neutral or Negative tone.
Just respond 'Positive', Neutral', or 'Negative'.")

##############################################

###STEP 4: combine the prompt with the text of interest and query it to the LLM

#Manage the loop variables.
#In this case: R will perform 5 loops, each of them queries 20 rows. Between each loop there is a pause of 60 seconds. The loop will start from the row n. 1.
#you should tailor it depending on ho long the prompts are and the API limits.

#number of loops
loops <- 5

#number of API queries in each loop
iterations <- 20

#seconds of pause between loops
pause_time <- 60

#the value of min is the row from which the process starts
min <- 1
max <- min+iterations

#use this command to clean the previous results:
#df$output_llm <- NA

###LOOP###

# Outer loop to repeat the process
for (loop in 1:loops) {  
    # Show the number of the current cycle
    cat("Loop n.", loop, "\n")
    
    # Inner loop to perform a number of queries
    for (i in min:max) {
        
        # Show the number of the current query
        cat("Query Row n.", i, "\n")
            
        #create the input by merging the question to the text of the current row [i]
        prompt <- paste(question, df$Text[i])
        
        #remove special characters from prompt
        prompt <- str_replace_all(prompt, "\\n", " ")
            
        #API query
        llmprompt <- llm_message(prompt,
                                 .system_prompt = "You are a helpful assistant that follows the instructions of the task assigned")
            
        answer <- chat(llmprompt, 
                           mistral,
                           .model="open-mistral-nemo",
                           .temperature = 0.1,
                           .top_p = 1)
        
        #extract the answer from the LLM output
        answer <- get_reply(answer)
        
        #remove text from output that is not belonging to the words contained in the vector "words"
        words <- c("Positive", "Neutral", "Negative")
        answer<- gsub("\n", " ", answer)

        #check if the text is not one of the words contained in the vector
        if (!answer %in% words) {
            pattern <- paste0("\\b(", paste(words, collapse = "|"), ")\\b")
            answer <- str_extract_all(answer, pattern) %>%
            unlist() %>%
            paste(collapse = " ")
        }
            
        #stores output in the data frame in the table
        df$output_llm[i] <- answer
        
        #end of inner loop
        }
    
    #sets at which row the the next loop will start and stop
    min <- min + iterations
    max <- max + iterations
    
    # Pause for the specified amount of time if there is a following loop 
    if (loop < loops) {
    cat("Pausing for", pause_time, "seconds...\n")
    Sys.sleep(pause_time) 
    }
}

#Save the data frame as a CSV file
# specify the file path
file_path <- file.path(working_folder, "output_data.csv")

#save the data frame as a CSV
write.csv(df, file = file_path, row.names = FALSE)

##############################################

###STEP 5: Validation

#the original dataset provides a human-made coding of the reviews' tone, I change the name of the variable
colnames(df)[colnames(df) == "Score"] <- "output_human"


#ACCURACY MEASURES
install.packages("yardstick")#only the first time

library(yardstick)

#transform columns into factor type (handles categorical variables) 
df$output_human <- factor(df$output_human, levels = c("Positive", "Neutral", "Negative")) #"True" values (Human)
df$output_llm <- factor(df$output_llm, levels = c("Positive", "Neutral", "Negative")) #"Predicted" values (LLM)

#calculating values with yardstick package
accuracy <- accuracy(df, truth = output_human, estimate = output_llm, estimator = "macro")
recall <- recall(df, truth = output_human, estimate = output_llm, estimator = "macro")
precision <- precision(df, truth = output_human, estimate = output_llm, estimator = "macro")
f1 <- f_meas(df, truth = output_human, estimate = output_llm, estimator = "macro")

cat("Precision:", precision$.estimate, "\n")
cat("Recall:", recall$.estimate, "\n")
cat("F1 Score:", f1$.estimate, "\n")
cat("Accuracy:", accuracy$.estimate, "\n")


#####################################

#INTERCODER RELIABILITY
install.packages("irr") #only the first time

library(irr)

#Inter-prompt stability (semantically similar prompts) (output_inter_p)

#ask the model to rephrase your prompt
rewrite_prompt <- paste("Please rewrite the following task with slightly different words:\n\n '", question, "'")
rewrite_prompt<- gsub("\n", "", rewrite_prompt)

llmprompt <- llm_message(rewrite_prompt,
                         .system_prompt = "You are a helpful assistant that follows the instructions of the task assigned")

answer <- chat(llmprompt, 
               mistral,
               .model="open-mistral-nemo",
               #set a significant temperature, at least 1 (value between 0 and 2)
               .temperature = 1,
               .top_p = 1)

new_question <- get_reply(answer)
print(new_question)

loops <- 5
iterations <- 20
pause_time <- 60
min <- 1
max <- min+iterations

for (loop in 1:loops) {  
    cat("Loop n.", loop, "\n")
    
    for (i in min:max) {
        
        cat("Query Row n.", i, "\n")
        #put the new question
        prompt <- paste(new_question, df$Text[i])
        prompt <- str_replace_all(prompt, "\\n", " ")
        
        #API query
        llmprompt <- llm_message(prompt,
                                 .system_prompt = "You are a helpful assistant that follows the instructions of the task assigned")
        
        answer <- chat(llmprompt, 
                       mistral,
                       .model="open-mistral-nemo",
                       .temperature = 0.1,
                       .top_p = 1)
        
        answer <- get_reply(answer)
        answer<- gsub("\n", "", answer)
        
        #remove text from output that is not belonging to the words contained in the vector "words"
        words <- c("Positive", "Neutral", "Negative")
        answer<- gsub("\n", " ", answer)
        
        #check if the text is not one of the words contained in the vector
        if (!answer %in% words) {
            pattern <- paste0("\\b(", paste(words, collapse = "|"), ")\\b")
            answer <- str_extract_all(answer, pattern) %>%
                unlist() %>%
                paste(collapse = " ")
        }   
        
        #store in another column
        df$output_inter_p[i] <- answer
    }
    
    #sets at which row the the next loop will start and stop
    min <- min + iterations
    max <- max + iterations
    
    # Pause for the specified amount of time if there is a following loop 
    if (loop < loops) {
        cat("Pausing for", pause_time, "seconds...\n")
        Sys.sleep(pause_time) 
    }
}

#depending on the new prompt, the results may need some cleaning. For instance, the removal of punctuation:
df$output_inter_p <- gsub("[[:punct:]]", "", df$output_inter_p)

##############################################

#Intra-prompt stability 

##Loop to repeat analysis with same prompt and model (output_intra_p)
loops <- 5
iterations <- 20
pause_time <- 60
min <- 1
max <- min+iterations

for (loop in 1:loops) {  
    cat("Loop n.", loop, "\n")
    
    for (i in min:max) {
        
        cat("Query Row n.", i, "\n")
        prompt <- paste(question, df$Text[i])
        prompt <- str_replace_all(prompt, "\\n", " ")
        
        #API query
        llmprompt <- llm_message(prompt,
                                 .system_prompt = "You are a helpful assistant that follows the instructions of the task assigned")
        
        answer <- chat(llmprompt, 
                       mistral,
                       .model="open-mistral-nemo",
                       .temperature = 0.1,
                       .top_p = 1)
        
        answer <- get_reply(answer)
        answer<- gsub("\n", "", answer)
        
        #remove text from output that is not belonging to the words contained in the vector "words"
        words <- c("Positive", "Neutral", "Negative")
        answer<- gsub("\n", " ", answer)
        
        #check if the text is not one of the words contained in the vector
        if (!answer %in% words) {
            pattern <- paste0("\\b(", paste(words, collapse = "|"), ")\\b")
            answer <- str_extract_all(answer, pattern) %>%
                unlist() %>%
                paste(collapse = " ")
        }   
        
        #store in another column
        df$output_intra_p[i] <- answer
            }
    
    #sets at which row the the next loop will start and stop
    min <- min + iterations
    max <- max + iterations
    
    # Pause for the specified amount of time if there is a following loop 
    if (loop < loops) {
        cat("Pausing for", pause_time, "seconds...\n")
        Sys.sleep(pause_time) 
    }
}

##############################################

#Inter-model stability 
##Loop to repeat analysis with a different model (output_model)
loops <- 5
iterations <- 20
pause_time <- 60
min <- 1
max <- min+iterations

for (loop in 1:loops) {  
    cat("Loop n.", loop, "\n")
    
    for (i in min:max) {
        
        cat("Query Row n.", i, "\n")
        prompt <- paste(question, df$Text[i])
        prompt <- str_replace_all(prompt, "\\n", " ")
        
        #API query
        llmprompt <- llm_message(prompt,
                                 .system_prompt = "You are a helpful assistant that follows the instructions of the task assigned")
        
        answer <- chat(llmprompt, 
                       mistral,
                       #pick a different model
                       .model="open-mistral-7b",
                       .temperature = 0.1,
                       .top_p = 1)
        
        answer <- get_reply(answer)

        #remove text from output that is not belonging to the words contained in the vector "words"
        words <- c("Positive", "Neutral", "Negative")
        answer<- gsub("\n", " ", answer)
        
        #check if the text is not one of the words contained in the vector
        if (!answer %in% words) {
            pattern <- paste0("\\b(", paste(words, collapse = "|"), ")\\b")
            answer <- str_extract_all(answer, pattern) %>%
                unlist() %>%
                paste(collapse = " ")
        }        
        
        #store in another column
        df$output_model[i] <- answer
    }
    
    #sets at which row the the next loop will start and stop
    min <- min + iterations
    max <- max + iterations
    
    # Pause for the specified amount of time if there is a following loop 
    if (loop < loops) {
        cat("Pausing for", pause_time, "seconds...\n")
        Sys.sleep(pause_time) 
    }
}

#Save the data frame as a CSV file
file_path <- file.path(working_folder, "benchmark_data.csv")
write.csv(df, file = file_path, row.names = FALSE)

##############################################
#Human-model stability 
#we use the human annotation from the dataset

##############################################

#upload already coded benchmark file
#file_path <- 'benchmark_data.csv'
#df <- read.csv(file_path, sep = ",")

#list of tests
tests <- c("output_inter_p", #inter-prompt stability (multiple runs with semantic variation of prompt)
           "output_intra_p", #intra-prompt stability (multiple runs with same prompt)
           "output_model", #inter-model stability (multiple runs on different models)
           "output_human") #model-human stability

#loop to calculate the Krippendorff alpha for different output stability measures
#create empty data frame for results
test_results <- data.frame()

for (test in tests) {  
    
 #create the rating table required for the Krippendorff alpha, containing the llm coding and the terms of comparison
 inter_df <- (df[, c(test, "output_llm")]) 
 inter_df <- as.data.frame(t(inter_df))
 inter_df <- as.matrix(inter_df)
 #select method according to the type of variable: nominal/ordinal etc.
 kripp_alpha <- kripp.alpha(inter_df, method = "ordinal") 
 
 #add the alpha value to a data frame
 new_row <- data.frame(test = test, value = kripp_alpha$value)
 test_results <- rbind(test_results, new_row)
 
}
