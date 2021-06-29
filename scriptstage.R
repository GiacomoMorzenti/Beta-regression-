library(tidyverse)
library(dplyr)
library(corrplot)
library(betareg)
library(glmmTMB)
library(FlexReg)
library(rstan)
library(loo)
library(MCMCpack)
library(lmtest)
library(olsrr)

install.packages('TMB', type = 'source')


ed=dataset_education
dim(ed)
anyNA(ed)
sum(complete.cases(ed))
educ=ed[complete.cases(ed),]
anyNA(educ)

#eliminati gli NA tolgo le osservazioni duplicate (createsi a causa di city-county)
anyDuplicated(educ$chiave)
anyDuplicated(ed$chiave)

sum(duplicated(educ$chiave))


doppionied=which(duplicated(educ$chiave))
doppionied
education=educ[-doppionied,]
dim(education)
anyDuplicated(education$chiave)

#eliminati i doppioni è pronto ad essere unito agli altri

pop=dataset_population
dim(pop)
anyNA(pop)
popul=pop[complete.cases(pop),]
anyNA(popul)
anyDuplicated(popul$chiave)
dop.popul=which(duplicated(popul$chiave))
dop.popul
population=popul[-dop.popul,]
anyDuplicated((population$chiave))
dim(population)


pov=dataset_poverty
dim(pov)
anyNA(pov)
anyDuplicated(pov$chiave)
dop.pov=which(duplicated(pop$chiave))
dop.pov
poverty=pov[-dop.pov,]
dim(poverty)

unem=dataset_unemployment
dim(unem)
anyNA(unem)
anyDuplicated(unem$chiave)
dop.unem=which(duplicated(unem$chiave))
unemployment=unem[-dop.unem,]
anyDuplicated(unemployment$chiave)
dim(unemployment)

#IMPORT BIDEN METTENDOLA COME NUMERIC

perc=percentuali_biden
dim(perc)
anyNA(perc)
anyDuplicated(perc$chiave)
dop.perc=which(duplicated(perc$chiave))
percentuali=perc[-dop.perc,]
anyDuplicated(percentuali$chiave)
dim(perc)



race=dataset_race
etnia=filter(race, year==2019)
dim(etnia)
anyDuplicated((etnia$chiave))
dop.etnia=which(duplicated(etnia$chiave))
dop.etnia
origin=etnia[-dop.etnia,]
dim(origin)
anyDuplicated(origin$chiave)
originni=mutate(origin, perc_white=white_pop/pop, perc_black=black_pop/pop, perc_hisp=hisp_pop/pop, perc_asian=asian_pop/pop, perc_indian=indian_pop/pop)
summary(originni)

origini=dplyr::select(originni, chiave, perc_white, perc_black, perc_hisp, perc_asian, perc_indian)


#puliti tutti i dataset procedo alla unione di questi

dim(poverty)
dim(population)
dim(unemployment)
dim(education)
dim(perc)
dim(origini)


unione=inner_join(poverty, population, by = c("chiave" = "chiave"))
dim(unione)

unione2=inner_join(unione, unemployment, by = c("chiave" = "chiave"))
dim(unione2)

unione3=inner_join(unione2, education,  by = c("chiave" = "chiave"))
dim(unione3)

unione5=inner_join(unione3, origini, by= c("chiave" = "chiave"))
dim(unione5)


unione4=inner_join(unione5, percentuali,  by = c("chiave" = "chiave"))
dim(unione4)


anyNA(unione4)
anyDuplicated(unione4$chiave)
dop=which(duplicated(unione4$chiave))
prova=unione4[-dop,]
anyDuplicated(prova)
dim(prova)


summary(prova)

data=prova[,c(1,5,3,4,8,9,10,11,13,14,17,18,19,20,21,22,23,24,25, 27)]
summary(data)
dim(data)

dati=mutate(data,percBiden = BIDEN*0.01)

hist(dati$percBiden,breaks=100)
summary(dati$percBiden)

dati2=mutate(dati, trasfBiden= log((percBiden)/(1-percBiden)))
summary(dati2)
dim(dati2)


summary(dati2)
str(dati2)
mean(dati2$bachelors_degree_or_higher)


M=cor(dati2[,3:19])
M
corrplot(M)


hist(dati2$percBiden, breaks=50,xlab= "Percentuali Biden" )
hist(dati2$trasfBiden, breaks=50, xlab= "Percentuali Biden")

datifixed=mutate(dati2, income = householdincome / 1000 , net_migration = NET_MIG / 1000, population = POP_ESTIMATE / 100000)
summary(datifixed)
datifixed=datifixed[,-c(2,4,5,6,18)]


datifixed=mutate(datifixed, ind_educ=(-3)*less_diploma + (-1)*diploma_only + college_or_associates_degree + 3*bachelors_degree_or_higher)

datiscaled=mutate(datifixed, household_income=scale(datifixed$income),  
                  population = scale(datifixed$population),
                  poverty_percent=scale(datifixed$povertypercent),
                  R_birth = scale(datifixed$R_birth),
                  R_net_migration = scale(datifixed$R_NET_MIG),
                  Med_HH_income_percent_ = scale(datifixed$Med_HH_Income_Percent_of_State_Total),
                  unemployment_rate = scale(datifixed$Unemployment_rate),
                  less_diploma = scale(datifixed$less_diploma),
                  diploma_only = scale(datifixed$diploma_only),
                  college_or_associates_degree = scale(datifixed$college_or_associates_degree),
                  perc_black = scale(datifixed$perc_black),
                  perc_hisp = scale(datifixed$perc_hisp),
                  perc_asian = scale(dati2$perc_asian),
                  perc_indian = scale(dati2$perc_indian),
                  ind_educ= scale(datifixed$ind_educ))

summary(datiscaled)

datiscaled=datiscaled[,-c(2,4,5,6,10,11,15,18,19)]


###### ANALISI ESPLORATIVA DELLE VARIABILI   ########


hist(scale(dati2$povertypercent), breaks=20, xlab = "poverty_percent", ylab="frequenza")
summary(dati2$povertypercent)
boxplot(dati2$povertypercent, xlab="povertypercent")
sd(dati2$povertypercent)

hist(dati2$householdincome, breaks=20, xlab = "householdincome", ylab="frequenza")
boxplot(dati2$householdincome,xlab="householdincome")
summary(dati2$householdincome)
sd(dati2$householdincome)

hist(dati2$Med_HH_Income_Percent_of_State_Total, breaks=20, xlab = "Med_HH_income_percent", ylab="frequenza")
boxplot(dati2$Med_HH_Income_Percent_of_State_Total,xlab="Med_HH_income_percent")
summary(dati2$Med_HH_Income_Percent_of_State_Total)
sd(dati2$Med_HH_Income_Percent_of_State_Total)

hist(dati2$POP_ESTIMATE, breaks=200, xlab = "population", ylab="frequenza", xlim=c(0,1000000))
boxplot(dati2$POP_ESTIMATE,xlab="population")
summary(dati2$POP_ESTIMATE)
sd(dati2$POP_ESTIMATE)

hist(dati2$R_NET_MIG, xlab="R_net_migration", breaks=30)
boxplot(dati2$R_NET_MIG, xlab="R_net_migration")

summary(dati2$R_NET_MIG)
sd(dati2$R_NET_MIG)


hist(dati2$R_birth, breaks=20, xlab = "R_birth", ylab="frequenza")
boxplot(dati2$R_birth,xlab="R_birth")
summary(dati2$R_birth)
sd(dati2$R_birth)

hist(dati2$Unemployment_rate, breaks=20, xlab = "Unemployment_rate", ylab="frequenza")
boxplot(dati2$Unemployment_rate,xlab="Unemployment_rate")
summary(dati2$Unemployment_rate)
sd(dati2$Unemployment_rate)

hist(dati2$less_diploma, breaks=30, xlab = "less_diploma", ylab="frequenza")
boxplot(dati2$less_diploma,xlab="less_diploma")
summary(dati2$less_diploma)
sd(dati2$less_diploma)

hist(dati2$diploma_only, breaks=30, xlab = "diploma_only", ylab="frequenza")
boxplot(dati2$diploma_only,xlab="diploma_only")
summary(dati2$diploma_only)
sd(dati2$diploma_only)

hist(dati2$perc_asian, breaks=40, xlab = "perc_asian", ylab="frequenza")
boxplot(dati2$perc_asian,xlab="perc_asian")
summary(dati2$perc_asian)
sd(dati2$perc_asian)

hist(dati2$perc_indian, breaks=40, xlab = "perc_indian", ylab="frequenza")
boxplot(dati2$perc_indian,xlab="perc_indian")
summary(dati2$perc_indian)
sd(dati2$perc_indian)
str(dati2)

dati2=mutate(dati2, perc_asian= perc_asian*100, perc_indian=perc_indian*100)

summary(dati2)

par(mfrow=c(1,1))

dati2finali=dati2[,-c(6,14,15,20)]
summary(dati2finali)
dati2finali=rename(dati2finali, povery_percent=povertypercent, household_income=householdincome, Med_HH_income_percent=Med_HH_Income_Percent_of_State_Total)
dati2finali=rename(dati2finali, Med_HH_income_percent=Med_HH_Income_Percent_of_State_Total)
dati2finali=rename(dati2finali, population=POP_ESTIMATE)
dati2finali=rename(dati2finali, Med_HH_income_percent=population, population=Med_HH_income_percent)
dati2finali=rename(dati2finali, poverty_percent=povery_percent)
dati2finali=rename(dati2finali, R_net_migration=R_NET_MIG)



population=dati2$POP_ESTIMATE
Med_HH_income_percent=dati2$Med_HH_Income_Percent_of_State_Total
dati2finali[,5]=Med_HH_income_percent
dati2finali[,9]=population  
poverty_percent=dati2$povertypercent

dati2finali[,5]

M=cor(dati2finali[,3:16])
M
corrplot(M)

par(mfrow=c(1,1))


hist(dati2$percBiden, breaks=40, xlab = "perc_Biden", ylab="frequenza")
boxplot(dati2$percBiden,xlab="perc_Biden")
summary(dati2$percBiden)
sd(dati2$percBiden)
str(dati2)


summary(dati2finali)


##### LINEAR MODEL #####



linearmodel1=lm(trasfBiden  ~ . -STATE.x - percBiden - less_diploma - diploma_only - college_or_associates_degree, datiscaled )
summary(linearmodel1)
stepmodel1=stepAIC(linearmodel1, direction="both")
summary(stepmodel1)

#elimina R_net_migration per la selezione del modello migliore secondo AIC


bestlmAIC1=lm(trasfBiden ~ . -STATE.x - percBiden - R_net_migration , datiscaled)
summary(bestlmAIC1)
summary(fitted(bestlmAIC1))
plot(fitted.values(bestlmAIC1))

bptest(bestlmAIC1)
dwtest(bestlmAIC1)
par(mfrow=c(2,2))
plot(bestlmAIC1)
boxplot(residuals(bestlmAIC1))

ols_test_normality(bestlmAIC1)




### BETA REGRESSION ###

summary(datiscaled)

?step

step(betamod, direction="both")
betamod=betareg(percBiden ~ . -STATE.x - trasfBiden - R_net_migration -ind_educ, datiscaled, link = "logit")
summary(betamod)


summary(dati2)

summary(betamod3)

AIC(betamod)

n=length(datifixed$povertypercent)

par(mfrow=c(2,3))
plot(betamod, which=1:6, type="pearson")


## le 3 osservazioni con cook distance sopra la soglia sono Los Angeles della California, Oglala Lakota tribu di nativi americani e 	
## TX.Kenedy che è lo stato con il più alto tasso di persone senza nemmeno il diploma (73%)


#provo a togliere i 3 stati con una cook distance elevata, ma ottengo un miglioramento trascurabile del R2 (0.02)

b=which(cooks.distance(betamod)>0.03)

datifixed3=datiscaled[-b,]
betamod3=betareg(percBiden ~ . -STATE.x - trasfBiden - R_net_migration, datifixed3, link = "logit")
summary(betamod3)


#provo a togliere tutte quelle sopra la soglia di cook (4/n), che risultano essere circa 200, e ottengo un miglioramento di 0.07 nel R quadro

a=which(cooks.distance(betamod)>0.001)
datifixed2=datiscaled[-a,]
betamod2=betareg(percBiden ~ . -STATE.x - trasfBiden - R_net_migration - ind_educ, datifixed2, link = "logit")
summary(betamod2)

plot(betamod2,which=1:6)


betamod=betareg(percBiden ~ . -STATE.x - trasfBiden - R_net_migration -ind_educ, datiscaled, link = "loglog")
AIC(betamod)


residualsP=residuals(betamod,type="pearson")

predlin=predict(betamod, type="link")
fitmean=predict(betamod, type="response")
plot(predlin, residualsP)

#possibili link: "logit", "probit", "cloglog" , "loglog"

#migliore link a livello di grafico è il "logit"

#diagnostica

fitted=predict(modbeta, type="response")
residui=residuals(modbeta,type="standardized")
predittlin=predict(modbeta, type="link")


plot(fitted,residui, xlab="fitted values", ylab="standardized residuals")
plot(predittlin,residui, xlab="linear predictor", ylab="standardized residuals")
plot(datiscaled$percBiden,fitted,xlab ="observed values", ylab="fitted values")

summary(residuals(modbeta, type="standardized"))


### FLEXREG ###

modbeta=flexreg(percBiden ~ poverty_percent + R_birth + R_net_migration + Med_HH_income_percent + less_diploma + diploma_only + college_or_associates_degree + perc_black + perc_hisp + household_income + population + unemployment_rate + perc_asian + perc_indian , data=datiscaled )
modbeta1=flexreg(percBiden ~ poverty_percent + R_birth + R_net_migration + Med_HH_income_percent + less_diploma + diploma_only + college_or_associates_degree + perc_black + perc_hisp + household_income + population + unemployment_rate + perc_asian + perc_indian  , data=datiscaled, type="Beta")

summary(modbeta)
summary(modbeta1)

WAIC(modbeta)
WAIC(modbeta1)


###      STAN       ####


n=length(datifixed$povertypercent)
y=datifixed$percBiden

X=cbind(rep(1,n),
        income=scale(datifixed$income),  
        population = scale(datifixed$population),
        povertypercent=scale(datifixed$povertypercent),
        R_birth = scale(datifixed$R_birth),
        R_NET_MIG = scale(datifixed$R_NET_MIG),
        Med_HH_Income_Percent_of_State_Total = scale(datifixed$Med_HH_Income_Percent_of_State_Total),
        unemployment_rate = scale(datifixed$Unemployment_rate),
        less_diploma = scale(datifixed$less_diploma),
        diploma_only = scale(datifixed$diploma_only),
        college_or_associates_degree = scale(datifixed$college_or_associates_degree),
        perc_black = scale(datifixed$perc_black),
        perc_hisp = scale(datifixed$perc_hisp),
        perc_asian = scale(dati2$perc_asian),
        perc_indian = scale(dati2$perc_indian))



livelli=dati$STATE.x
livelli=as.factor(livelli)
levels(livelli)=seq(from=1, to=49, by=1 )
str(livelli)
livelli=as.numeric(livelli)
?seq

FB_Mixed <- rstan::stan_model("FB_mixed.stan")
Beta_Mixed <- rstan::stan_model("Beta_Mixed.stan")


# creo una lista con i dati necessari per la stima
data.stan <- list(
  N = n,  y = y,
  K = ncol(X),  X = X,
  sd_prior = 10, g=0.001,
  J = 49, subject = livelli
)



n.iter <- 5000 # lunghezza della catena

fit.FB <- rstan::sampling(
  object = FB_Mixed,   # Stan program
  data = data.stan,       # named list of data
  chains = 1,             # number of Markov chains
  warmup = 0.5*n.iter,          # number of warmup iterations per chain
  iter = n.iter,            # total number of iterations per chain
  cores = 1,              # number of cores (using 2 just for the vignette)
  thin=1,
  control = list(adapt_delta = .95),
  refresh = n.iter/100           # show progress every 'refresh' iterations
)
print(fit.FB, pars=c("beta", "phi", "p", "w", "sigma_u", "U"))



fit_Beta <- rstan::sampling(
  object = Beta_Mixed,   # Stan program
  data = data.stan,       # named list of data
  chains = 1,             # number of Markov chains
  warmup = 0.5*n.iter,          # number of warmup iterations per chain
  iter = n.iter,            # total number of iterations per chain
  cores = 1,              # number of cores (using 2 just for the vignette)
  thin=1,
  #control = list(adapt_delta = .8),
  refresh = n.iter/100           # show progress every 'refresh' iterations
)
print(fit_Beta, pars=c("beta", "phi", "sigma_u", "U"))


# WAIC: Misura simile all'AIC per confrontare modelli. Piu' basso e', meglio e'
waic(extract_log_lik(fit_Beta))
waic(extract_log_lik(fit.FB))
loo(fit_Beta)
loo(fit.FB)


