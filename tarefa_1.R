# funcao objetivo perda L1:
min_MAE <- function(data, par) {
  with(data, sum( abs( Y - (par[1] + par[2] * X) ) ) )
}

# Lendo os dados:
file = 'C:/Users/alega/Documents/Mestrado Stats/ML/Tarefa_1/109779.txt'
data = read.csv2(file, header = TRUE, sep = " ", dec = ",")
# transform to numeric
data$X = as.numeric(as.character(data$X))
data$Y = as.numeric(as.character(data$Y))

# seed:
set.seed(0)
percent_train = .85
repeticoes = 100

int_lm = c()
int_opt = c()
coef_lm = c()
coef_opt = c()
mae_in_lm = c()
mae_in_opt = c()
mae_out_lm = c()
mae_out_opt = c()

for (i in seq(repeticoes))

{

# Selecionando randomicamente 85% para treinamento e 15% para teste:
sample <- sample.int(n = nrow(data), size = floor(percent_train*nrow(data)), replace = F)
train <- data[sample, ]
test  <- data[-sample, ]

# MQO:
linear_reg <- lm(Y ~ X, data = train)

int_lm = c(int_lm, coef(linear_reg)[1])
coef_lm = c(coef_lm, coef(linear_reg)[2])

test_pred <- predict(linear_reg, test)
mae_out_lm <- c(mae_out_lm, mean(abs(test$Y - test_pred)) )

train_pred <- predict(linear_reg, train)
mae_in_lm <- c(mae_in_lm, mean(abs(train$Y - train_pred)) )

# optimization with L1:
result <- optim(par = c(0, 1), fn = min_MAE, data = train )

int_opt <- c(int_opt, result[[1]][1])
coef_opt <- c(coef_opt, result[[1]][2])

test_pred <- result[[1]][1] + result[[1]][2] * test$X
train_pred <- result[[1]][1] + result[[1]][2] * train$X

mae_out_opt <- c(mae_out_opt, mean(abs(test$Y - test_pred)) )
mae_in_opt <- c(mae_in_opt, mean(abs(train$Y - train_pred)) )

}

results <- data.frame(
  int_lm,
  int_opt,
  coef_lm,
  coef_opt,
  mae_in_lm,
  mae_in_opt,
  mae_out_lm,
  mae_out_opt)

# histograms:
mapply(hist,results,main=colnames(results),xlab="",breaks=50, col='gray') 

# mean:
colMeans(results)

# std deviation:
sapply(results, sd)

# gráfico de dispersão dos dados:
ggplot(data, aes(x=X, y=Y)) + geom_point(size=2)
