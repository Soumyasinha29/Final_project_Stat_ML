classification_tree_model <- function(filtered_dataset) {

    library(caret)
    library(rpart)
    library(rpart.plot)
    library(dplyr)

    # making the categorical variables as factor
    
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
    #     downsampled_Alive_obs <- filtered_dataset[filtered_dataset$isDead == "Alive",] %>% 
    #     slice_sample(n = num_deceased, replace = FALSE)
        
    #     # combine the downsampled Deceased class with the original data
    #     filtered_dataset <- rbind(Deceased_obs, downsampled_Alive_obs)
    # }

    # print(summary(filtered_dataset))
    
    # splitting between training and test data

    train_ids <- sample(nrow(filtered_dataset), round(nrow(filtered_dataset) * 0.7))
    train_data <- filtered_dataset[train_ids, ]
    test_data <- filtered_dataset[-train_ids, ]
    
    # train the model
    
    set.seed(1234)
    
    ctree_01 <- rpart(isDead ~.,data = filtered_dataset, 
    method = "class", parms = list(split = "gini"),
    control = rpart.control(minsplit = 30, minbucket = 10), cp = 0)
    
    print(ctree_01)

    # get tuning grid for train function
    tg_clatr <- data.frame(cp = ctree_01$cptable[,1])
    
    # train the 10 CV model
    
    set.seed(1234)
    ctree_10cv <- train(x = filtered_dataset[,1:6], y = filtered_dataset$isDead,
    method = "rpart", parms = list(split = "gini"),
    control = rpart.control(minsplit = 30, minbucket = 10),
    tuneGrid = tg_clatr, trControl = trainControl(method = "cv", number = 10,selectionFunction = "oneSE"))
    
    print(ctree_10cv)

    #Accuracy
    print(paste0("Train accuracy: ", ctree_10cv$results$Accuracy))

    # evaluate the model on the test data
    predictions <- predict(ctree_10cv, newdata = test_data)
    confusion_matrix <- table(predictions, test_data$isDead)
    test_accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
    print(paste0("Test accuracy: ", test_accuracy))

    # plot the selected classification tree
    rpart.plot(ctree_10cv$finalModel)

    return (NULL)
    
}
