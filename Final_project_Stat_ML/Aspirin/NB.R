NaiveBayes_Model <- function(filtered_dataset) {
    
    suppressWarnings({library(dplyr)
    library(caret)
    library(klaR)
    library(e1071)
    library(tidyr)


    filtered_dataset$gender <- factor(filtered_dataset$gender)
    filtered_dataset$gotAspirin24 <- factor(filtered_dataset$gotAspirin24)
    filtered_dataset$gotPE <- factor(filtered_dataset$gotPE)
    filtered_dataset$gotDVT <- factor(filtered_dataset$gotDVT)
    filtered_dataset$isDead <- factor(ifelse(filtered_dataset$isDead== 1, "Deceased", "Alive"))
    filtered_dataset$ethnicity <- factor(filtered_dataset$ethnicity)
    filtered_dataset$race <- factor(filtered_dataset$race)
    
    # # To balance the dataset uncomment the below code

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

    # fit naive Bayes classifier using naiveBayes()
    # function from e1071 package; by default, each numerical
    # predictor is assumed to follow a normal distribution
    # within each class

    set.seed(1234)


    nb_fit <- train(isDead ~ ., data = train_data, method="nb", trControl= trainControl(method="cv", number=10, classProbs = TRUE,verboseIter = TRUE, savePredictions= TRUE))

    print(nb_fit)

    #Train Accuracy
    train_accuracy <- nb_fit$results[which.max(nb_fit$results$Accuracy),]
    print(paste0("Training Accuracy: ", train_accuracy$Accuracy))

    # get predicted classes from naive Bayes classifier for test data
    test_pred_class_nb <- predict(nb_fit, newdata = test_data)
    print(test_pred_class_nb[1:5])

    # Create a confusion matrix
    Nb_conf_matrix <- confusionMatrix(data = test_pred_class_nb, reference = test_data$isDead)
    
    # Print the confusion matrix
    print(Nb_conf_matrix)

    # get test error rate
    print(mean(test_pred_class_nb != test_data$isDead))

    # evaluate on test data
    predicted_labels <- predict(nb_fit, newdata = test_data)
    test_accuracy <- confusionMatrix(predicted_labels, test_data$isDead)$overall["Accuracy"]
    print(paste0("Test Accuracy: ", test_accuracy))


    return (NULL)

    })


}
