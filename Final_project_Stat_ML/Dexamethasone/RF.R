rf_model <- function(filtered_dataset) {

    # library(varImp)
    library(randomForest)
    library(caret)
    library(rpart)
    library(rpart.plot)

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
    
    rf_10cv <- train(x = filtered_dataset[,1:6], y = filtered_dataset$isDead, method = "rf", ntree = 200, 
    tuneGrid = data.frame(mtry = 1:15), trControl = trainControl(method = "cv", number = 10))
    
    print(rf_10cv)

    print(rf_10cv$finalModel)
    
    #OOB accuracy
    print("OOB Accuracy:")
    print(rf_10cv$results$Accuracy[1])

    # can plot OOB MSE vs. number of trees
    plot(x = 1:200, y = rf_10cv$finalModel$err.rate[,1], xlab = "Number of Trees B", ylab = "OOB MSE")

    # variable importance plot
    imp = varImp(rf_10cv$finalModel)
    imp$feature = row.names(imp)
    imp = imp[order(imp$Overall, decreasing= TRUE),]
    select_top = 15
    imp = imp[1:select_top, ]
    imp = imp[order(imp$Overall, decreasing= FALSE),]
    imp$feature = factor(imp$feature, levels = imp$feature)
    p = ggplot(imp, aes(x = feature, y = Overall))+
    geom_bar(stat = 'identity', fill='#033C5A')+
    ggtitle('Variable importance plot', subtitle = 'Random Forest')+
    coord_flip()+
    xlab('Variable importance')+
    theme_classic()
    plot(p)

    # predict on test data
    predicted_test <- predict(rf_10cv, newdata = test_data)

    # confusion matrix
    cm_test <- confusionMatrix(predicted_test, test_data$isDead)
    print("Test Data Confusion Matrix:")
    print(cm_test)
    

    return (NULL)
    
}
