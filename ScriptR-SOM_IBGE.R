################################################################################
# Criando um "Self Organizing Map" com dados obtidos pelo IBGE. 
# Utilizou-se medidas de trabalho e rendimento, desigualdade, saúde e 
# educação coletadas nas UFs do Brasil em 2022.
#
# A base de dados contem as seguintes características, para cada UF:
# desocupacao: Taxa de desocupação (%)
# rendimento: Rendimento médio do trabalho (R$)
# gini: Índice de Gini
# leitos: Número de leitos de internação complementares para adultos 
#         por 10.000 habitantes
# analfabetismo: Taxa de analfabetismo das pessoas de 15 anos ou mais de 
#                idade (%)
# anos_estudo: Número médio de anos de estudo das pessoas de 15 anos ou mais
################################################################################


Dados_IBGE_2022 <- read.csv("C:/Users/malut/Google Drive/Capacitacoes/Cientista_de_dados_UDEMY/Self-organizing maps_IBGE/DadosIBGE-2022.csv", sep=";")
View(Dados_IBGE_2022)
head(Dados_IBGE_2022)

###############################################################
## Analise usando o pacote aweSOM
# Referência no link: https://cran.r-project.org/web/packages/aweSOM/vignettes/aweSOM.html

library(aweSOM)

#treinando o SOM:
#################
full.data <- Dados_IBGE_2022
## Select variables
train.data <- full.data[, c("desocupacao", "rendimento", "gini", "leitos",
                            "analfabetismo", "anos_estudo")]
### Scale training data
train.data <- scale(train.data)

### RNG Seed (for reproducibility)
set.seed(1465)
### Initialization (PCA grid)
init <- somInit(train.data, 3, 3)
## Train SOM
IBGE.som <- kohonen::som(train.data, grid = kohonen::somgrid(3, 3, "hexagonal"), 
                         rlen = 100, alpha = c(0.05, 0.01), radius = c(2.65,-2.65), 
                         dist.fcts = "sumofsquares", init = init)

#avaliando a qualidade do mapa:
###############################
somQuality(IBGE.som, train.data)

#superclasses de SOM:
#####################
superclust_pam <- cluster::pam(IBGE.som$codes[[1]], 3)
superclasses_pam <- superclust_pam$clustering

superclust_hclust <- hclust(dist(IBGE.som$codes[[1]]), "complete")
superclasses_hclust <- cutree(superclust_hclust, 3)

#plotando a informação do mapa:
###############################

#Observations Cloud: visualize the observations as points inside their cell. Points can optionally be colored according to a variable of the data, as here by the Species of the iris (a variable not used during training). Other variables can be displayed in an interactive title box for each observation.
aweSOMplot(som = IBGE.som, type = "Cloud", data = full.data, 
           variables = c("UF", "desocupacao", "rendimento", "gini", "leitos",
                         "analfabetismo", "anos_estudo"), 
           superclass = superclasses_pam)

#Hitmap or population map: visualizes the number of observation per cell. The areas of the inner shapes are proportional to their population. The background colors indicate the superclasses. The palette of these colors can be adapted using the palsc argument.
aweSOMplot(som = IBGE.som, type = "Hitmap", superclass = superclasses_pam)

#UMatrix is a way to explore the topography of the map. It shows the average distance between each cell and its neighbors’ prototypes, on a color scale. On this map, the darker cells are close to their neighbors, while the brighter cells are more distant from their neighbors.
aweSOMplot(som = IBGE.som, type = "UMatrix", superclass = superclasses_pam)

# The aweSOMsmoothdist function can further be used to produce a smooth representation of the U-Matrix. Note, however, that the result representation is biased when using hexagonal maps (the smoothing function coerces the grid to a rectangular shape).
aweSOMsmoothdist(IBGE.som)

# Plotting numeric variables on the map: by default the means of the chosen variables are displayed within each cell. 
aweSOMplot(som = IBGE.som, type = "Circular", data = full.data, 
           variables = c("desocupacao", "rendimento", "gini", "leitos",
                         "analfabetismo", "anos_estudo"), 
           superclass = superclasses_pam)

aweSOMplot(som = IBGE.som, type = "Barplot", data = full.data, 
           variables = c("desocupacao", "rendimento", "gini", "leitos",
                         "analfabetismo", "anos_estudo"), 
           superclass = superclasses_pam)

# Plotting a categorical variable on the map: neste caso usaremos a variavel
# Região, que nao foi utilizada no treinamento.
aweSOMplot(som = IBGE.som, type = "CatBarplot", data = full.data, 
           variables = "Região", superclass = superclasses_pam)


