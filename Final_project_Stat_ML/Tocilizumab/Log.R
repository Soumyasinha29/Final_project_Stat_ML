log_model <- function(filtered_dataset) {

    library(caret)
    library(dplyr)
    library(ggplot2)

    filtered_dataset$gender <- factor(filtered_dataset$gender)
    filtered_dataset$gotTocilizumab24 <- factor(filtered_dataset$gotTocilizumab24)
    filtered_dataset$gotPE <- factor(filtered_dataset$gotPE)
    filtered_dataset$gotDVT <- factor(filtered_dataset$gotDVT)
    filtered_dataset$isDead <- factor(ifelse(filtered_dataset$isDead== 1, "Deceased", "Alive"))
    filtered_dataset$ethnicity <- factor(filtered_dataset$ethnicity)
    # Find the new level as race has 8 levels
    new_race_level <- unique(filtered_dataset$race)[!unique(filtered_dataset$race) %in% levels(filtered_dataset$race)]

    # Add the new level to the levels of the race factor
    levels(filtered_dataset$race) <- c(levels(filtered_dataset$race), new_race_level)

    
    # # Find the number of observations in each class
    # num_alive <- sum(filtered_dataset$isDead == "Alive")
    # num_deceased <- sum(filtered_dataset$isDead == "Deceased")
    
    # # Downsample the Deceased class to have the same number of observations as the Alive class
    # if (num_alive > num_deceased) {
    #     # find observations with the Alive class
    #     Deceased_obs <- filtered_dataset[filtered_dataset$isDead == "Deceased",]
        
    #     # downsample the Deceased class to have the same number of observations as the Alive class
    #     downsampled_Alive_obs <- filtered_dataset %>%
    #         filter(isDead == "Alive") %>%
    #         sample_n(size = num_deceased, replace = FALSE)
        
    #     # combine the downsampled Deceased class with the original data
    #     filtered_dataset <- rbind(Deceased_obs, downsampled_Alive_obs)
    # }

    # print(summary(filtered_dataset))

    train_ids <- sample(nrow(filtered_dataset), round(nrow(filtered_dataset) * 0.7))
    train_data <- filtered_dataset[train_ids, ]
    test_data <- filtered_dataset[-train_ids, ]


    set.seed(1234)
    
    log_model_cv <- train(x = filtered_dataset[,1:6], y = filtered_dataset$isDead, method = "glm", family = binomial(link = "logit"),
    trControl = trainControl(method = "cv", number = 10, classProbs = TRUE, savePredictions = TRUE))

    
    print(log_model_cv)
    
    #Train Accuracy
    train_accuracy <- log_model_cv$results$Accuracy
    print(paste0("Train Accuracy: ", train_accuracy))

    # Get predictions on the test data
    log_pred <- predict(log_model_cv, newdata = test_data, type = "raw")

    # Create a confusion matrix
    log_conf_matrix <- confusionMatrix(data = log_pred, reference = test_data$isDead)
    
    # Print the confusion matrix
    print(log_conf_matrix)
    
    # Test Accuracy
    test_accuracy <- log_conf_matrix$overall[1]
    print(paste0("Test Accuracy: ", test_accuracy))


    # Plot of the distribution of the response variable
    plot(ggplot(filtered_dataset, aes(x = isDead, fill = isDead)) +
    geom_bar() +
    xlab("Survival Status") +
    ylab("Count") +
    ggtitle("Distribution of Survival Status"))


    # Plot of the relationship between gender and the response variable
    plot(ggplot(filtered_dataset, aes(x = gender, fill = isDead)) +
    geom_bar(position = "fill") +
    xlab("Gender") +
    ylab("Proportion") +
    ggtitle("Relationship Between Gender and Survival Status"))

    # Plot of the relationship between gotTocilizumab24 and the response variable
    plot(ggplot(filtered_dataset, aes(x = gotTocilizumab24, fill = isDead)) +
    geom_bar(position = "fill") +
    xlab("Got Tocilizumab 24") +
    ylab("Proportion") +
    ggtitle("Relationship Between Got Tocilizumab 24 and Survival Status"))

    # Plot of the relationship between ethnicity and the response variable
    plot(ggplot(filtered_dataset, aes(x = ethnicity, fill = isDead)) +
    geom_bar(position = "fill") +
    xlab("Ethnicity") +
    ylab("Proportion") +
    ggtitle("Relationship Between Ethnicity and Survival Status"))

    # Plot of the relationship between race and the response variable
    plot(ggplot(filtered_dataset, aes(x = race, fill = isDead)) +
    geom_bar(position = "fill") +
    xlab("Race") +
    ylab("Proportion") +
    ggtitle("Relationship Between Race and Survival Status"))

    return (NULL)
    
}
