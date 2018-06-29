data(iris)

View(iris)

iris.f <- iris

iris.f$Species <- NULL

# kmeans(dataset,how many custers[in this data we already know 3 species ,so 3])

results <- kmeans(iris.f,3)  
results

results$size

results$cluster

table(iris$Species,results$cluster)

# comparison with results to iris data set . this k means is pretty good.

plot(iris[c("Petal.Length","Petal.Width")],col=results$cluster)

plot(iris[c("Petal.Length","Petal.Width")],col=iris$Species)


# comparison with results to iris data set . this k means is pretty good.


plot(iris[c("Sepal.Length","Sepal.Width")],col=results$cluster)

plot(iris[c("Sepal.Length","Sepal.Width")],col=iris$Species)




