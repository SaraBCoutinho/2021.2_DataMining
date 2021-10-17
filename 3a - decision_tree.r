# Decision Tree -----

dt <- sort(sample(nrow(data_selected), nrow(data_selected)*.7))
train <- data_selected[dt,]
test <- data_selected[-dt,]

model <- rpart(DESEMPENHO ~., data = train)
par(xpd = NA) # otherwise on some devices the text is clipped
plot(model)
text(model, digits = 3)