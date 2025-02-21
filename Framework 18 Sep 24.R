################################################
#	Projeto ecologia térmica mosquitos
#		by Gabriel e Guilherme
#		ssantos.gabriel@gmail.com
#			12 setembro 2024
################################################

library(openxlsx)
library(car)
library(performance)
library(tidyverse)
library(MuMIn)
library(performance)
library(lme4)
library(see)
library(DHARMa)
library(sjPlot)
library(ggplot2)
library(ggeffects)

rm(list=ls())

options(na.action=na.fail)	#Define na action to run dredge function from MuMIn

#==========================================================
#	Define diretório
#==========================================================

#setwd(choose.dir())	#Usar esse Guilherme!
#setwd("C:/Artigos e resumos publicados submetidos ideias/Notes/1 - Projetos/01 - Colaborações em projetos e artigos em desenvolvimento/Projetos Jeronimo - Fiocruz")
#dir("./Dados e scripts")
#dados<-read.xlsx("./Dados e scripts/Book1.xlsx",sheet="Goias_CLEAN_2")
#dados_eclosion<-read.xlsx("./Dados e scripts/Experimento 1 (Temperatura).xlsx",sheet="Ovitrampa Goiás ")	#Tem um "espaço a mais na aba ovitrampa Goiais, cuidado!

dados<-read.xlsx("Book1.xlsx",sheet="Goias_CLEAN_2")
dados_eclosion<-read.xlsx("Experimento 1 (Temperatura).xlsx",sheet="Ovitrampa Goiás ")	#Tem um "espaço a mais na aba ovitrampa Goiais, cuidado!


dados%>%glimpse()
dados<-dados%>%select(-L1_IMERSION)

dados_eclosion%>%glimpse()

#==========================================================
#	ECLOSION MODEL
#==========================================================
dados_eclosion$Temperatura<-c(25,30,30,20,20,20,20,15,15,15,15)	#Necessário para tirar o "ºC"

dados_eclosion%>%select(Total.de.ovos,Ovos.eclodidos,Temperatura)
dados_eclosion$Temperatura<-as.numeric(dados_eclosion$Temperatura)

dados_eclosion%>%
group_by(Temperatura)%>%
summarise(Eclosion_rate=mean((Ovos.eclodidos/Total.de.ovos)*100),
sd=sd((Ovos.eclodidos/Total.de.ovos)*100),
N=n())

eclosion_mod<-glm(cbind(Ovos.eclodidos,Total.de.ovos)~
              Temperatura+I(Temperatura^2),family = binomial("logit"),data=dados_eclosion)


dredge(eclosion_mod)

#-------------------------------------------------------------------------------
# Modelo validation
#-------------------------------------------------------------------------------
simulateResiduals(eclosion_mod, plot =F)%>%testResiduals()
#-------------------------------------------------------------------------------
ggpredict(eclosion_mod,se=TRUE)
eclosion_mod_pred<-ggpredict(eclosion_mod, terms = c("Temperatura[20:30,by=.5]"))
eclosion_mod_pred[[1]]

eclosion_mod_pred%>%as.tibble()%>%
	slice_max(predicted,n = 5)%>%arrange(x)

plot_model(eclosion_mod, type = "pred", terms = "Temperatura[15:30]",show.data = T,colors = "bw")+
xlab("Temperature")+ylab("Egg hatching probability")+
labs(title=NULL)+
theme_bw(base_size=16)


#==========================================================
#	ECLOSION MODEL
#==========================================================

surv <- data.frame(
  Vivos = c(7, 109, 80),
  Mortos = c(3, 19, 14),
  Temperatura = c(20, 25, 30))%>%
mutate(Total=Vivos+Mortos)

surv 

mod_surv<-glm(cbind(Mortos,Total)~
        Temperatura+I(Temperatura^2),family = binomial("logit"),data=surv)

dredge(mod_surv)

#-------------------------------------------------------------------------------
# Modelo validation
#-------------------------------------------------------------------------------
simulateResiduals(mod_surv, plot =F)%>%testResiduals()
#-------------------------------------------------------------------------------

mod_surv%>%parameters::parameters()


plot_model(mod_surv, type = "pred", terms = "Temperatura[20:30]",show.data = T,colors = "bw")+
xlab("Temperature")+ylab("Survival probability")+
labs(title=NULL)+
theme_bw(base_size=16)


#==========================================================
#	DEVELOPMENT
#==========================================================
dados_long<-dados%>%
	select(SEXO,temp,L2_L1:ADULTO_IMERSION)%>%
		pivot_longer(!c(SEXO,temp),
			names_to="Transition",values_to="Days")%>%
filter(Transition!="ADULTO_IMERSION")	# Need to be removed to avoid being double accounted


#----------------------------------------------------------
#	Data summary and standardization
#----------------------------------------------------------
dados%>%
	select(SEXO,temp,L2_L1:ADULTO_IMERSION)%>%
		pivot_longer(!c(SEXO,temp),
			names_to="Transition",values_to="Days")%>%
#	group_by(SEXO,Transition,temp)%>%	#Use this for a complete summary
	group_by(temp)%>%filter(Transition=="ADULTO_IMERSION")%>%	
		summarise(Mean_Days=round(mean(Days),2),
			SD_Days=round(sd(Days),2),
			Min=min(Days),
			Max=max(Days),
			low95=quantile(Days,0.025),
			high95=quantile(Days,0.975))%>%
							data.frame()
#----------------------------------------------------------
#	Global model
#----------------------------------------------------------
# Fit the full model
model_full<-glm(Days~temp+SEXO*Transition+I(temp^2)*SEXO*Transition,family="poisson",data=dados_long)

# Extract best model according to AIC
best_full<-model_full%>%
	dredge%>%
		get.models(., subset = 1)


bestfull_glm<-glm(formula(best_full[[1]]),data=dados_long)

bestfull_glm%>%summary()
bestfull_glm%>%anova()

plot_model(bestfull_glm, type = "pred", terms = c("temp[20:30]","Transition"),show.data = F)

#Full model did not pass model assumptions
simulateResiduals(bestfull_glm, plot =F)%>%testResiduals()

#----------------------------------------------------------
#	DATA TRANSFORMATION
#----------------------------------------------------------

# Log transformation to reduce dispersion
dados_log<-dados %>%
  mutate(across(L2_L1:ADULTO_IMERSION,log))

#z-score transformation to make models among different stages comparable
dados_std<-dados %>%
  mutate(across(L2_L1:ADULTO_IMERSION,scale))

#----------------------------------------------------------
#	MODEL - Full development
#----------------------------------------------------------
modeloADULTO_IMERSAO <- glm(ADULTO_IMERSION ~ SEXO+temp+I(temp^2), data = dados_log)


#Multi model selectio
dredge_out<-dredge(modeloADULTO_IMERSAO)
dredge_out

best_alldev<-get.models(dredge_out, subset = 1)

best_alldev[[1]]%>%parameters::parameters()
best_alldev[[1]]%>%anova()

simulateResiduals(best_alldev[[1]],plot=T)
#Warning message "Context: nKcase - wrong family"
#This message appear because values still looking like discretized after log and poisson


best_alldev_glm<-glm(ADULTO_IMERSION ~ SEXO + temp + I(temp^2),data=dados_log)

plot_model(best_alldev_glm, type = "pred", terms = c("temp[20:30]","SEXO"),show.data = T)

plot_model(best_alldev_glm, show.data = T , jitter = .3,
	type = "pred", terms = c("temp[20:30]","SEXO"),transform)+
labs(x="Temperature",
	y="log(Days)",
	color="Sex")+
#xlab("Temperature")+
#ylab("log(Days)")+
labs(title=NULL)+
theme_bw(base_size=16)+ 
  theme_bw(base_size = 16)+
#  scale_y_continuous(
#    breaks = seq(log(1),log(40),by=log(log(5))),  # Intervalos de 5 em 5 de 1 a 40
#    labels = function(x) scales::label_number()(exp(x)))+
  scale_x_continuous(
    breaks = seq(20,30,by=5))


ggpredict(best_alldev_glm,,terms = c("temp[20,25,30]"))%>%
  data.frame()%>%
  mutate(across(c(predicted, conf.low, conf.high), exp))  
  
best_alldev_pred<-ggpredict(best_alldev_glm,,terms = c("temp[20:30,by=.5]","SEXO"))%>%
data.frame()%>%
  mutate(across(c(predicted, conf.low, conf.high), exp)) %>%
  mutate(SE = (conf.high - conf.low) / (2 * 1.96))

best_alldev_pred%>%
group_by(group)%>%
  mutate(
    rank_min = min_rank(predicted),  # Classifica os valores por 'predicted'
    rank_max = dense_rank(desc(predicted))  # Classifica em ordem decrescente
  ) %>%
  filter(rank_min <= 3 | rank_max <= 3) %>%  # Mantém valores com rank <= 3 em ambas as classificações
  ungroup()  # Desagrupa após a operação


ggpredict(best_alldev_glm,,terms = c("temp[20:30,by=.5]","SEXO"))%>%
data.frame()%>%
  mutate(across(c(predicted, conf.low, conf.high), exp)) %>%
  mutate(SE = (conf.high - conf.low) / (2 * 1.96))%>%
group_by(group)%>%
  slice_min(order_by = predicted, n = 3)%>%
relocate(group,x)

#----------------------------------------------------------
#	PIECEWISE MODEL FITTING
#----------------------------------------------------------
#	Acessory function to help validate all models with one hit
#---
Stand_residout<-function(X){
 simul<-DHARMa::simulateResiduals(X)
	temp<-DHARMa::testResiduals(simul)
 out<-data.frame(
		Var=c("Uniformity(KS(D))","Dispersion","Outliers"),
			Stat=c(as.numeric(temp$uniformity[[1]]),
				as.numeric(temp$dispersion[[2]]),
				paste(as.numeric(temp$outliers[[1]]),"of",as.numeric(temp$outliers[[2]]))),
		p.value=c(temp$uniformity[[2]],
				temp$dispersion[[5]],
				paste(temp$outliers[[3]])))
return(out)}
#-----------------------------------------------------------

# Model fitting
modL2_L1 <- glm(L2_L1 ~ temp+I(temp^2)+SEXO, data = dados_std)
modL3_L2 <- glm(L3_L2 ~ temp+I(temp^2)+SEXO, data = dados_std)
modL4_L3 <- glm(L4_L3 ~ temp+I(temp^2)+SEXO, data = dados_std)
modPUPA_L4 <- glm(PUPA_L4 ~ temp+I(temp^2)+SEXO, data = dados_std)
modADULTO_PUPA <- glm(ADULTO_PUPA ~ temp+I(temp^2)+SEXO, data = dados_std)




mod_trans<-list(
  modL2_L1,
  modL3_L2,
  modL4_L3,
  modPUPA_L4,
  modADULTO_PUPA)


lapply(mod_trans,Stand_residout)
lapply(mod_trans,performance::r2)
lapply(mod_trans,parameters::parameters)



plot_model(modL2_L1 , show.data = T , jitter = .3,
	type = "pred", terms = c("temp[20:30]","SEXO"),transform)

plot_model(modL3_L2 , show.data = T , jitter = .3,
	type = "pred", terms = c("temp[20:30]","SEXO"))

#-------------------------------------------------------------------------------
#	Quantifying effect size of each parameter
#-------------------------------------------------------------------------------
stats<-c("Intercept","Temp","Temp_quad","SEXO")

effect_temp<-lapply(
			lapply(
			  mod_trans,
					parameters::parameters),
						data.frame,stats=stats)

names(effect_temp)<-c("L2_L1","L3_L2","L3_L4","Pupa_L4","Adulto_Pupa")

effect_temp<-effect_temp%>%bind_rows(.,.id="Stages")%>%as_tibble()%>%
select( Stages,stats,Coefficient,SE,CI,CI_low,CI_high,p)%>%
filter(stats!="Intercept")%>%
mutate(p.value=ifelse(p>0.05,"Non-sig","sig"))

effect_temp
#---------------------------
#Bonferroni no longer used
 group_by(stats) %>%  # Agrupa por 'Stages' e 'stats'
  mutate(p_adjusted = p.adjust(p, method = "bonferroni"))%>%
mutate(p.value=ifelse(p_adjusted>=0.05,"Non-sig","sig"))
#---------------------------

effect_temp%>%
filter(stats!="Intercept")%>%
mutate(stats=ifelse(stats=="SEXO","diff F/M",stats))%>%
#filter(Stages!="Adulto_imersion")%>%
mutate(Stages=factor(Stages,levels=unique(effect_temp$Stages)))%>%
ggplot(.,aes(x=Stages,y=Coefficient,group=stats))+
#geom_line()+
geom_pointrange(aes(ymin = CI_low, ymax = CI_high,color=p.value))+
facet_wrap(.~stats,scale="free_y")+
theme_bw(base_size=16)+
geom_hline(yintercept=0, linetype="dashed", color = "red",linesize=.5)+
scale_color_manual(values=c("grey70","black"))+
ylab("Estimated effect \non development")+xlab(NULL)+
#ylab(paste0("Estimated effect of temperature \n on development ",bquote(beta[max](mu~mol ~CO[2]~ m^-2~s^-1))))+
#ylab(expression(atop("Estimated effect of temperature", "on development ( -1* "~beta[Temp]~")"))) +
#theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))
theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1))+
facet_wrap(.~stats,scales="free_y")



