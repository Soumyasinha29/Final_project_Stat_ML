boosted_class_model <- function(filtered_dataset) {
    
    library(gbm)
    library(caret)
    library(dplyr)


    filtered_dataset$gender <- factor(filtered_dataset$gender)
    filtered_dataset$gotDexamethasone24 <- factor(filtered_dataset$gotDexamethasone24)
    filtered_dataset$gotPE <- factor(filtered_dataset$gotPE)
    filtered_dataset$gotDVT <- factor(filtered_dataset$gotDVT)
    filtered_dataset$isDead <- factor(ifelse(filtered_dataset$isDead== 1, "Deceased", "Alive"))
    filtered_dataset$ethnicity <- factor(filtered_dataset$ethnicity)
    filtered_dataset$race <- factor(filtered_dataset$race)

    # # Find the number of observations in each class
    # num_alive <- sum(filtered_dataset$isDead == "Alive")
    # num_deceased <- sum(filtered_dataset$isDead == "Deceased")
    
    # # Downsample the Deceased class to have the same number of observations as the Alive class
    # if (num_alive > num_deceased) {
    #     # find observations with the Alive class
    #     Deceased_obs <- filtered_dataset[filtered_dataset$isDead == "Deceased",]
        
    #     # downsample the Deceased class to have the same number of observations as the Alive class
    #     downsampled_Alive_obs <- filtered_dataset[filtered_dataset$isDead == "Alive",] %>% 
    #     slice_sample(n = num_deceased, replace = FALSE)
        
    #     # combine the downsampled Deceased class with the original data
    #     filtered_dataset <- rbind(Deceased_obs, downsampled_Alive_obs)
    # }

    # print(summary(filtered_dataset))

    train_ids <- sample(nrow(filtered_dataset), round(nrow(filtered_dataset) * 0.7))
    train_data <- filtered_dataset[train_ids, ]
    test_data <- filtered_dataset[-train_ids, ]
    
    set.seed(1234)
    
    # set up a simpler tuning grid because of bigger size of the dataset
    
    tg_boostclass <- expand.grid(n.trees = c(5, 10, 15), interaction.depth = 1:2, shrinkage = c(0.20, 0.25), 
    n.minobsinnode = 10)
                             
    set.seed(1234)
    
    # method reduced to 5 because 10 was crashing the environment as it provided excessive load on the model
    
    model_boostclass <- train(x = filtered_dataset[,1:6], y = filtered_dataset$isDead, verbose=FALSE,
    method = "gbm", bag.fraction = 0.50, tuneGrid = tg_boostclass, trControl = trainControl(method = "cv", number = 5, classProbs = TRUE, savePredictions = TRUE))
    
    print(model_boostclass$bestTune)
    
    train_accuracy <- model_boostclass$results[which.max(model_boostclass$results$Accuracy),]
    print(paste0("Training Accuracy: ", train_accuracy$Accuracy))

    # fit and assess logistic model with 0.50 threshold
    prob_thresh <- seq(0.10, 0.90, by = 0.10)

    train_logit_thresh <- thresholder(model_boostclass, threshold = prob_thresh, final = TRUE, 
    statistics = c("Sensitivity", "Specificity", "Accuracy", "Kappa"))
    
    print(train_logit_thresh)
    
    # evaluate on test data
    predicted_labels <- predict(model_boostclass, newdata = test_data)
    test_accuracy <- confusionMatrix(predicted_labels, test_data$isDead)$overall["Accuracy"]
    print(paste0("Test Accuracy: ", test_accuracy))
    
    # relative influence approach
    
    print(summary(model_boostclass$finalModel, method = relative.influence, normalize = TRUE, las = 2))
    
    print(plot(model_boostclass$finalModel, i = "gender"))
    print(plot(model_boostclass$finalModel, i = "race"))
    print(plot(model_boostclass$finalModel, i = "gotDexamethasone24"))
    
    return (NULL)
    
}
