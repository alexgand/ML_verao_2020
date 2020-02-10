# reproducibility:
set.seed(0)

# loading the data:
data <- read.csv('C:/Users/alega/Documents/Mestrado Stats/ML/tarefa_3/dadosTarefa3.csv', sep=';')

# basic exploration:
head(data)
dim(data)
summary(data)

# variables classes:
sapply(data, class)

# variables types:
binnary <- c('Sex','FamilyHx','Married','remission')
factor <- c('DID','SmokingHx')
not_cont <- c(binnary, factor)

# selecting only continuous variables:
data_cont <- data[, -which(names(data) %in% not_cont)]
# data tem alguns valores faltantes, removendo eles:
sum(is.na(data_cont))
data_cont <- na.omit(data_cont)

library(ggbiplot)

# problema 1:

PCA <- prcomp(data_cont, center=TRUE, scale.=TRUE)
summary(PCA)

results = matrix(nrow=dim(data_cont)[2], ncol=2 )
colnames(results) <- c('PC1','PC2')
rownames(results) <- names(data_cont)

for (PC in c('PC1', 'PC2'))
{
  for (col in names(data_cont))
  {
    results[col,PC] <- cor(as.data.frame(PCA$x)[PC], data_cont[col])
  }
}

results[order(results[,1], decreasing = TRUE ),]
results[order(results[,2], decreasing = TRUE ),]

# problema 2:
ggbiplot(PCA, ellipse = TRUE, groups = na.omit(data)$SmokingHx) + theme_minimal()
ggbiplot(PCA, ellipse = TRUE, groups = na.omit(data)$DID) + theme_minimal()

# problema 3:

# k-means na dimensão do PCA
var_escolhida = 'SmokingHx'
qtd_grupos = dim(unique(data[var_escolhida]))[1]
kmeans_PCA <- kmeans(PCA$x[,1:2],centers = qtd_grupos)  
grupos = as.factor(kmeans_PCA$cluster)
data_cont['kmeans_PCA'] <- grupos
data_cont['SmokingHx'] <- na.omit(data)['SmokingHx']
classificacao <- table(kmeans_PCA$cluster, data_cont$SmokingHx)
print(classificacao)
# chart
kmeans_PCA$cluster <- as.factor(kmeans_PCA$cluster)
ggplot(as.data.frame(PCA$x), aes(PC1, PC2, color = data_cont$kmeans_PCA)) + geom_point()

# k-means na dimensão do dos dados, considerando somente as duas variáveis mais
# correlacionadas com PC1 (mobility) e PC2 (lungcapacity)
var_escolhida = 'SmokingHx'
qtd_grupos = dim(unique(data[var_escolhida]))[1]
kmeans_PCA <- kmeans(data_cont[,c('mobility','lungcapacity')],centers = qtd_grupos)  
grupos = as.factor(kmeans_PCA$cluster)
data_cont['kmeans_PCA'] <- grupos
data_cont['SmokingHx'] <- na.omit(data)['SmokingHx']
classificacao <- table(kmeans_PCA$cluster, data_cont$SmokingHx)
print(classificacao)
# chart
kmeans_PCA$cluster <- as.factor(kmeans_PCA$cluster)
ggplot(data_cont[,c('mobility','lungcapacity')], aes(mobility, lungcapacity, color = data_cont$kmeans_PCA)) + geom_point()


# hierarchical clustering:

library(RColorBrewer)
library(dplyr)
library(uclust)

# nas duas primeiras dimensões do PCA:

# resetando o data_cont:
# selecting only continuous variables:
data_cont <- data[, -which(names(data) %in% not_cont)]
# data tem alguns valores faltantes, removendo eles:
sum(is.na(data_cont))
data_cont <- na.omit(data_cont)

# matriz de distâncias:
d <- dist(PCA$x[,1:2], method = "euclidean")
# hc clustering:
hc <- hclust(d, method = "single" )
plot(hc, hang = -1)

# enfeitando o dendograma
cores = c("red", "green","blue")
clrs <- cores[as.factor(data$SmokingHx)]
dend <- as.dendrogram(hc, center = T)
dend %>% set("labels_col", clrs) %>% # change color
  set("labels_cex", 1) %>% # Change size
  set("branches_k_color", k = 3) %>% 
  set("labels_colors", clrs[hc$order]) %>%
  plot() # plot

grupos = cutree(hc, k = 3)
data_cont['grupos'] <- grupos
data_cont['SmokingHx'] <- na.omit(data)['SmokingHx']
classificacao <- table(data_cont$grupos, data_cont$SmokingHx)
print(classificacao)

# nas dimensão das duas variáveis mais correlacionadas com PC1 e PC2:

# resetando o data_cont:
# selecting only continuous variables:
data_cont <- data[, -which(names(data) %in% not_cont)]
# data tem alguns valores faltantes, removendo eles:
sum(is.na(data_cont))
data_cont <- na.omit(data_cont)

# matriz de distâncias:
d <- dist(data_cont[,c('mobility','lungcapacity')], method = "euclidean")
# hc clustering:
hc <- hclust(d, method = "single" )
plot(hc, hang = -1)

# enfeitando o dendograma
cores = c("red", "green","blue")
clrs <- cores[as.factor(data$SmokingHx)]
library(RColorBrewer)
library(dplyr)
library(uclust)
dend <- as.dendrogram(hc, center = T)
dend %>% set("labels_col", clrs) %>% # change color
  set("labels_cex", 1) %>% # Change size
  set("branches_k_color", k = 3) %>% 
  set("labels_colors", clrs[hc$order]) %>%
  plot() # plot

grupos = cutree(hc, k = 3)
data_cont['grupos'] <- grupos
data_cont['SmokingHx'] <- na.omit(data)['SmokingHx']
classificacao <- table(data_cont$grupos, data_cont$SmokingHx)
print(classificacao)

# problema 4:

# k-means na dimensão do PCA
var_escolhida = 'DID'
qtd_grupos = dim(unique(data[var_escolhida]))[1]
kmeans_PCA <- kmeans(PCA$x[,1:2],centers = qtd_grupos)  
grupos = as.factor(kmeans_PCA$cluster)
data_cont['kmeans_PCA'] <- grupos
data_cont['DID'] <- na.omit(data)['DID']
classificacao <- table(kmeans_PCA$cluster, data_cont$DID)
print(classificacao)
# chart
kmeans_PCA$cluster <- as.factor(kmeans_PCA$cluster)
ggplot(as.data.frame(PCA$x), aes(PC1, PC2, color = data_cont$kmeans_PCA)) + geom_point()

var_escolhida = 'remission'
qtd_grupos = dim(unique(data[var_escolhida]))[1]
kmeans_PCA <- kmeans(PCA$x[,1:2],centers = qtd_grupos)  
grupos = as.factor(kmeans_PCA$cluster)
data_cont['kmeans_PCA'] <- grupos
data_cont['remission'] <- na.omit(data)['remission']
classificacao <- table(kmeans_PCA$cluster, data_cont$remission)
print(classificacao)
# chart
kmeans_PCA$cluster <- as.factor(kmeans_PCA$cluster)
ggplot(as.data.frame(PCA$x), aes(PC1, PC2, color = data_cont$kmeans_PCA)) + geom_point()

# problema 5:
data[,c('remission','Sex','FamilyHx','Married','DID')]

X = na.omit(data[,c('Sex','FamilyHx','Married','DID')])

d <- dist(X, method = "euclidean")
# hc clustering:
hc <- hclust(d, method = "complete" )
plot(hc, hang = -1)

# enfeitando o dendograma
cores = c("red", "green")
clrs <- cores[as.factor(data$remission)]
dend <- as.dendrogram(hc, center = T)
dend %>% set("labels_col", clrs) %>% # change color
  set("labels_cex", 1) %>% # Change size
  set("branches_k_color", k = 3) %>% 
  set("labels_colors", clrs[hc$order]) %>%
  plot() # plot

grupos = cutree(hc, k = qtd_grupos)
X['grupos'] <- grupos
X['remission'] <- na.omit(X)['remission']
classificacao <- table(X$grupos, data$remission)
print(classificacao)

table(data$remission, X$grupos)
