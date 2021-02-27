#tapply() dla pogrupowanych danych - test normalności
#levene.test() z pakietu lawstat, problem
#do jednorodnosci wariancji z wykorztsraniem testu Levene'a lub Browna-Forsythe'a
#Ograniczyć się do dwu i trzypokojowych mieszkań bo problem z outlierami w mieszkaniach
#kurtoza dodatnia - zle
#Dodatnia wartość kurtozy (rozkład leptokurtyczny) wskazuje na istnienie wielu wartości bliskiej średniej, 
#ujemna wartość kurtozy (rozkład platykurtyczny) wskazuje na większe rozproszenie wyników wokół średniej.
#complete.cases() do oulierow

#install.packages("lsr")

#### Example 1: one-way ANOVA ####

outcome <- c( 1.4,2.1,3.0,2.1,3.2,4.7,3.5,4.5,5.4 )  # data
treatment1 <- factor( c( 1,1,1,2,2,2,3,3,3 ))        # grouping variable
anova1 <- aov( outcome ~ treatment1 )                # run the ANOVA
summary( anova1 )                                    # print the ANOVA table
etaSquared( anova1 )                                 # effect size                            

#### Example 2: two-way ANOVA ####

treatment2 <- factor( c( 1,2,3,1,2,3,1,2,3 ))      # second grouping variable
anova2 <- aov( outcome ~ treatment1 + treatment2 ) # run the ANOVA
summary( anova2 )                                  # print the ANOVA table
etaSquared( anova2 )      

library(gplots)
library(dplyr)
library(onewaytests)
library(psych)
library(nortest)
library(readxl)
library(lsr)

setwd("C:/Users/An/Desktop/SAD")
df <- read_xls("mieszkania23.xls")
df$dzielnica <- as.factor(df$dzielnica)
df$typ_budynku <- as.factor(df$typ_budynku)
df$pokoi <- as.factor(df$pokoi)

tapply(df$cena_m2, df$typ_budynku, summary)

str(df)
head(df)

CV <- function(.df){
  w <- c(1:ncol(.df))
  for (i in 1:ncol(.df)) {
    w[i] <- sd(.df[[i]], na.rm=TRUE)/mean(.df[[i]], na.rm=TRUE)*100
  }
  return(w)
}

describeBy(df$cena_m2, df$dzielnica, mat = TRUE, digits = 2)
describeBy(df$cena_m2, df$typ_budynku, mat = TRUE, digits = 2)
describeBy(df$cena_m2, df$pokoi, mat = TRUE, digits = 2)

boxplot(df[,1],horizontal = TRUE)
boxplot(df[,2],horizontal = TRUE)
boxplot(df[,6],horizontal = TRUE)

boxplot(cena_m2 ~ dzielnica, data = df)
boxplot(cena_m2 ~ typ_budynku, data = df)
boxplot(cena_m2 ~ pokoi, data = df)


tapply(df$cena_m2, df[,c(2,5)], length)

tapply(df$cena, df$dzielnica, mean)
# tapply(df$cena, df$typ_budynku, mean)
# tapply(df$cena, df[,4:5], mean)


plotmeans(cena_m2 ~ dzielnica, data = df)
plotmeans(cena_m2 ~ typ_budynku, data = df, add=TRUE)

tapply(df$cena, df$dzielnica, var)
tapply(df$cena, df$typ_budynku, var)
tapply(df$cena, df[,4:5], sd)

cor(tapply(df$cena_m2, df$dzielnica, var),tapply(df$cena_m2, df$dzielnica, mean))
cor(tapply(df$cena_m2, df$typ_budynku, mean),tapply(df$cena_m2, df$typ_budynku, var))
cor(tapply(df$cena_m2, df$pokoi, mean),tapply(df$cena_m2, df$pokoi, var))

tapply(df$cena_m2, df$pokoi, ad.test)
tapply(df$cena_m2, df$pokoi, cvm.test)
tapply(df$cena_m2, df$pokoi, sf.test)

tapply(df$cena_m2, df$dzielnica, ad.test)
tapply(df$cena_m2, df$dzielnica, cvm.test)
tapply(df$cena_m2, df$dzielnica, sf.test)

tapply(df$cena_m2, df$typ_budynku, ad.test)
tapply(df$cena_m2, df$typ_budynku, cvm.test)
tapply(df$cena_m2, df$typ_budynku, sf.test)

#wariancja
bartlett.test(df$result~df$hand,df)
bf.test(result~hand, data = df)

#rozne liczby grup!!

res.aov <- aov(cena_m2 ~ dzielnica*pokoi, data = df)
# Summary of the analysis
summary(res.aov)
etaSquared(res.aov, type = 2, anova = TRUE)

res.aov <- aov(cena_m2 ~ pokoi*typ_budynku, data = df)
# Summary of the analysis
summary(res.aov)
etaSquared(res.aov, type = 2, anova = FALSE)

# kruskal.test(result~hand,df)
