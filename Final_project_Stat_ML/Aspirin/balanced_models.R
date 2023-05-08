balanced_models <- function(filtered_dataset) {
    library(ggplot2)

    # create a data frame with the model names and their performance metrics
    model_performance <- data.frame(
    model = c("logistic classification", "Decision tree", "Boosted Classification", "Random Forest", "Naive Bayes"),
    accuracy = c(0.5995, 0.5690, 0.5980, 0.5977, 0.5838),
    sensitivity = c(0.6384, 0.5426, 0.6284, 0.6294, 0.9144),
    specificity = c(0.5606, 0.7280, 0.5682, 0.5658, 0.2547),
    kappa = c(0.1989, 0.1464, 0.1967, 0.1952, 0.1688 ),
    precision = c(0.5921, 0.5572, 0.5920, 0.5931, 0.9144),
    auc = c(0.5995, 0.678, 0.7903, 0.5977, 0.5845),
    f1_score = c(0.6147, 0.6142, 0.6813, 0.5931, 0.6849)
    )

    # reshape the data frame to long format
    model_performance_long <- reshape2::melt(
    model_performance,
    id.vars = "model",
    variable.name = "metric",
    value.name = "value"
    )

    # create a bar plot for each metric
    plot(ggplot(model_performance_long, aes(x = model, y = value, fill = model)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~metric, ncol = 3, scales = "free") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(
        x = "Model metrics for Aspirin",
        y = "",
        fill = "Model"
    ))

    return(NULL)

}
