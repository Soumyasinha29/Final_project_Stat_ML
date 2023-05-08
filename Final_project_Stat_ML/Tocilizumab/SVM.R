Balanced_SVM_Model <- function(filtered_dataset) {

    library(caret)
    library(dplyr)
    library(kernlab)

    filtered_dataset$gender <- factor(filtered_dataset$gender)
    filtered_dataset$gotTocilizumab24 <- factor(filtered_dataset$gotTocilizumab24)
    filtered_dataset$gotPE <- factor(filtered_dataset$gotPE)
    filtered_dataset$gotDVT <- factor(filtered_dataset$gotDVT)
    filtered_dataset$isDead <- factor(ifelse(filtered_dataset$isDead== 1, "Deceased", "Alive"))
    filtered_dataset$ethnicity <- factor(filtered_dataset$ethnicity)
    filtered_dataset$race <- factor(filtered_dataset$race)

    # Find the number of observations in each class
    num_alive <- sum(filtered_dataset$isDead == "Alive")
    num_deceased <- sum(filtered_dataset$isDead == "Deceased")
    
    # Downsample the Deceased class to have the same number of observations as the Alive class
    if (num_alive > num_deceased) {
        # find observations with the Alive class
        Deceased_obs <- filtered_dataset[filtered_dataset$isDead == "Deceased",]
        
        # downsample the Deceased class to have the same number of observations as the Alive class
        downsampled_Alive_obs <- filtered_dataset %>%
            filter(isDead == "Alive") %>%
            sample_n(size = num_deceased, replace = FALSE)
        
        # combine the downsampled Deceased class with the original data
        filtered_dataset <- rbind(Deceased_obs, downsampled_Alive_obs)
    }

    # print(summary(filtered_dataset))


    train_ids <- sample(nrow(filtered_dataset), round(nrow(filtered_dataset) * 0.7))
    train_data <- filtered_dataset[train_ids, ]
    test_data <- filtered_dataset[-train_ids, ]

    set.seed(1234)
    svm_rbf_C10 <- ksvm(isDead ~., data = train_data, kernel = "rbfdot",kpar = list(sigma = 1),C = 10, scale = FALSE, cross = 10)
    
    print(svm_rbf_C10)

    set.seed(1234)
    svm_rbf_C0.1 <- ksvm(isDead ~., data = train_data, kernel = "rbfdot", kpar = list(sigma = 1),C = 0.10,scale = FALSE, cross = 10)
    
    print(svm_rbf_C0.1)

    # first 5 indices for support vectors
    svm_rbf_C10@SVindex[1:5]

    # first 5 indices for support vectors
    svm_rbf_C0.1@SVindex[1:5]

    # create predictor matrix and response vector
    # since the predictors are a mix of numerical and categorical variables,
    # it is simplest to set up a model matrix that converts categorical variables
    # to sets of dummy variables
    f1 <- formula(isDead ~ gender + ethnicity + race + gotTocilizumab24 + gotPE + gotDVT)

    mf <- model.frame(f1, data = train_data)
    
    train_data_X <- model.matrix(mf, data = train_data)[,-1] # remove the intercept column
    train_data_Y <- train_data$isDead

    # set up tuning grid
    tg_svmrbk <- expand.grid(C = c(0.001, 0.01, 0.1 , 1, 10),sigma = seq(0.005, 0.10, by = 0.005))
    
    set.seed(1234)
    train_svmrbk <- train(x = train_data_X, y = train_data_Y, method = 'svmRadial', tuneGrid = tg_svmrbk, 
    trControl = trainControl(method = "cv", number = 10))

    train_svmrbk$results[which.max(train_svmrbk$results$Accuracy),]

    # have to create dummy variables for categorical variables in test set
    mftest <- model.frame(f1, data = test_data)
    data_test_X <- model.matrix(mftest, data = test_data)[,-1] # remove the intercept column

    # obtain predicted classes
    pred_test <- predict(train_svmrbk$finalModel, newdata = data_test_X)

    # construct confusion matrix
    table(obs = test_data$isDead, pred = pred_test) 

    return (NULL)
}
