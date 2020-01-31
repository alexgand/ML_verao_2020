# libraries:
library(glmnet)
library(ggplot2)

# seed:
set.seed(0)

# DGP:
n = 50 # Exercicio 1
# n = 200 # Exercicio 2
p = 100
repeticoes = 1000

# inicializacoes:
lambdas = c()
incluiu_variaveis_corretas = c()
acertou_exatamente = c()
coefs_acertou_exatamente = c()
lambdas_acertou = c()
lambdas_errou = c()

# manually select k-folds:
k = 5
# k = 10

for (i in seq(repeticoes))
  
  {
  X = matrix(ncol = p, nrow = n)
  for (i in seq(p))
    # Exercicio 1:
    { X[,i] = rnorm(n, mean = 0, sd = sqrt(i)) }
    # Exercicio 2:
    # { X[,i] = rexp(n, rate = sqrt(i)) }
  colnames(X)= paste("X", seq(p), sep = "")
  
  # Exercicio 1:
  erro = rnorm(n, mean = 0, sd = 1)
  # Exercicio 2:
  # erro = rexp(n, rate = 1)
  
  # modelo exercicios 1 e 2:
  y = 20 + 5 * X[,"X1"] + 5 * X[,"X10"] + 5 * X[,"X20"] + 5 * X[,"X50"] + 5 * X[,"X90"] + erro
  # modelo exercicio 3:
  # y = 20 + 5 * X[,"X1"] + 5 * X[,"X2"] + 5 * X[,"X3"] + 5 * X[,"X4"] + 5 * X[,"X5"] + erro
  
  CV = cv.glmnet(X, y, alpha=1, nfold = k) # alpha = 1 so the model is all LASSO, no RIDGE penalty.
  lambda = CV$lambda.min
  lambdas = c(lambdas,lambda) # salva o lambda otimo
  ajuste = glmnet(X, y, alpha = 1, lambda = lambda)
  cf = coef(ajuste)
  nm = rownames(cf)[cf[,1] != 0]
  #print(nm)
  # round(cf[nm,],4)
  
  vars = paste("X",c(1,10,20,50,90), sep = "")
  # vars = paste("X",c(1,2,3,4,5), sep = "") # Exercicio 3
  qtd_variaveis = sum(vars %in% nm)
  
  if (qtd_variaveis == 5)
    { incluiu_variaveis_corretas <- c(incluiu_variaveis_corretas, 1) }
  else
    { incluiu_variaveis_corretas <- c(incluiu_variaveis_corretas, 0) }
  
  if ( (length(nm) - 1) == length(vars) )
    {
    acertou_exatamente <- c(acertou_exatamente, 1)
    coefs_acertou_exatamente <- c(coefs_acertou_exatamente, cf[cf[,1] != 0])
    lambdas_acertou <- c(lambdas_acertou, lambda)
    }
  else
    {
    acertou_exatamente <- c(acertou_exatamente, 0)
    lambdas_errou <- c(lambdas_errou, lambda)
    }
  
  }

# transforma vetor em matriz a cada 6th observacao:
coeficientes = as.data.frame(matrix(coefs_acertou_exatamente, ncol = 6,  byrow = TRUE), stringsAsFactors = FALSE)
colnames(coeficientes) <- c('intercept',paste("X",c(1,10,20,50,90), sep = ""))
#colnames(coeficientes) <- c('intercept',paste("X",c(1,2,3,4,5), sep = "")) Exercicio 3