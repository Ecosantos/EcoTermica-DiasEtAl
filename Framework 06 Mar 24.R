################################################
#	Projeto ecologia térmica mosquitos
#		by Gabriel e Guilherme
#		ssantos.gabriel@gmail.com
#			06March2024
################################################

library(openxlsx)
library(car)
library(performance)
library(visreg)
library(tidyverse)
library(MuMIn)
library(performance)
library(lme4)
library(see)
library(DHARMa)
library(sjPlot)
library(ggplot2)
library(ggeffects)

#==========================================================
#	Define diretório
#==========================================================

#setwd(choose.dir())	#Usar esse Guilherme!
setwd("C:/Artigos e resumos publicados submetidos ideias/Notes/4 - Projects & Organization/0 - Projetos em andamento/Projetos Jeronimo - Fiocruz")
dir("./Dados e scripts")
dados<-read.xlsx("./Dados e scripts/Book1.xlsx",sheet="Goias_CLEAN_2")
dados_eclosion<-read.xlsx("./Dados e scripts/Experimento 1 (Temperatura).xlsx",sheet="Ovitrampa Goiás ")	#Tem um "espaço a mais na aba ovitrampa Goiais, cuidado!

dados%>%glimpse()
dados_eclosion%>%glimpse()


#==========================================================
#	ECLOSION MODEL
#==========================================================
dados_eclosion$Temperatura<-c(25,30,30,20,20,20,20,15,15,15,15)	#Necessário para tirar o "ºC"

dados_eclosion%>%select(Total.de.ovos,Ovos.eclodidos,Temperatura)%>%

dados_eclosion

dados_eclosion%>%
group_by(Temperatura)%>%
summarise(Eclosion_rate=mean((Ovos.eclodidos/Total.de.ovos)*100),
sd=sd((Ovos.eclodidos/Total.de.ovos)*100))

eclosion_mod<-glm(cbind(Ovos.eclodidos,Total.de.ovos)~
              Temperatura+I(Temperatura^2),family = binomial("logit"),data=dados_eclosion)
visreg::visreg(eclosion_mod, "Temperatura")

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
xlab("Temperature")+ylab("Eclosion probability")+
labs(title=NULL)+
theme_bw(base_size=16)

#==========================================================
#	DEVELOPMENT MODEL
#==========================================================
#----------------------------------------------------------
#	Temperature only
#----------------------------------------------------------
modeloL1_IMERSION <-glm(L1_IMERSION ~ temp+I(temp^2),family = poisson, data=dados)
modeloL2_L1 <-glm(L2_L1 ~ temp+I(temp^2),family = poisson, data=dados)
modeloL3_L2 <-glm(L3_L2 ~ temp+I(temp^2),family = poisson, data=dados)
modeloL4_L3 <-glm(L4_L3 ~ temp+I(temp^2),family = poisson, data=dados)
modeloPUPA_L4 <-glm(PUPA_L4 ~ temp+I(temp^2),family = poisson, data=dados)
modeloADULTO_PUPA <-glm(ADULTO_PUPA ~ temp+I(temp^2),family = poisson, data=dados)
modeloADULTO_IMERSÃO <-glm(ADULTO_IMERSION ~ temp+I(temp^2),family = poisson, data=dados)

modeloL2_L1 %>%summary()
modeloL3_L2 %>%summary()
modeloL4_L3 %>%summary()
modeloPUPA_L4%>%summary()
modeloADULTO_PUPA %>%summary()
modeloADULTO_IMERSÃO%>%summary()


#----------------------------------------------------------
#		Full model simple 
#----------------------------------------------------------
dados_long<-dados%>%select(SEXO,temp,L1_IMERSION:ADULTO_IMERSION,-c(PUPA_L4,"L1_IMERSION"))%>%
pivot_longer(!c(SEXO,temp),names_to="Transition",values_to="Days")
		
dados_long

#Check basic
simple_fullmod<-dados_long%>%
	glm(Days~SEXO+Transition+temp,family="poisson",data=.)

simple_fullmod%>%	check_model()

#check_model and DHARMa detected problems:
	# low dispersion
	# Outliers (Leverage test)
	# QQplot 
#but check fitted values against true observations.		
	#Model is fair, problably small number of individuals in 20C	
plot(cbind(dados_long$Days,fitted(simple_fullmod)),
	xlim=c(0,30),ylim=c(0,30),
	xlab="Observed",ylab="Fitted")
abline(a=0,b=1)
cor.test(dados_long$Days,fitted(simple_fullmod))
#--------------------------------------------------------------------
#	CHECK REMOVING OUTLIERS
#--------------------------------------------------------------------
#Influential observations accordingly Check_model()
dados_long[c(503,961,505,975,25),]

# Estimate 95% interval confidence for mean values
dados_long%>%
	group_by(SEXO,Transition,temp)%>%
		summarise(Mean_Days=mean(Days),
			SD_Days=sd(Days),
			low95=quantile(Days,0.025),
			high95=quantile(Days,0.975))%>%
							data.frame()

# Re-estimate parameters without potential outliers
outliers_out<-dados_long[-c(503,961,505,975,25),]%>%
	glm(Days~SEXO+Transition+temp,family="poisson",data=.)

outliers_out%>%check_model()
simulateResiduals(outliers_out, plot = T)

# Problem still persist but fitting is fair
plot(dados_long[-c(503,961,505,975,25),]$Days,fitted(outliers_out),
	xlim=c(0,30),ylim=c(0,30),
	xlab="Observed",ylab="Fitted")
abline(a=0,b=1)

#----------------------------------------------------------
#		Full model complete
#----------------------------------------------------------
full_mod<-dados_long%>%
glm(Days~SEXO*Transition+temp+I(temp^2),data=.,family="poisson")

full_mod%>%summary()

full_mod%>%parameters::parameters()

simple_fullmod_valid<-simulateResiduals(simple_fullmod, plot = F)
simple_fullmod_valid%>%testResiduals()

full_valid<-full_mod%>%simulateResiduals(., plot = F)
full_valid%>%testResiduals()

cor.test(fitted(full_mod),dados_long$Days)
plot(fitted(full_mod)~dados_long$Days,
	xlim=c(0,30),ylim=c(0,30),
	xlab="Observed",ylab="Fitted")
abline(a=0,b=1)

#-------------------------------------------------------------------------------
#	Quantifying effect size
#-------------------------------------------------------------------------------
stats<-c("Intercept","Temp","Temp_quad")

effect_temp<-lapply(
			lapply(
	list(modeloL2_L1,
		modeloL3_L2,
		modeloL4_L3,
		modeloPUPA_L4,	
		modeloADULTO_PUPA,
		modeloADULTO_IMERSÃO),
			parameters::parameters),
					data.frame,stats=stats)

effect_temp
names(effect_temp)<-c("L2_L1","L3_L2","L3_L4","Pupa_L4","Adulto_Pupa","Adulto_imersion")

effect_temp<-effect_temp%>%bind_rows(.,.id="Stages")%>%as_tibble()%>%
select( Stages,stats,Coefficient,SE,CI,CI_low,CI_high,p)

effect_temp

#-------------------------------------------------------------------------------
#	Quantifying Optimum temperature
#-------------------------------------------------------------------------------

opt_temp<-lapply(
		lapply(
	list(modeloL2_L1,
		modeloL3_L2,
		modeloL4_L3,
		modeloPUPA_L4,	
		modeloADULTO_PUPA,
		modeloADULTO_IMERSÃO),
			ggpredict,terms = c("temp[20:30,by=.5]")),
					data.frame)


names(opt_temp)<-c("L2_L1","L3_L2","L3_L4","Pupa_L4","Adulto_Pupa","Adulto_imersion")

opt_temp<-opt_temp%>%bind_rows(.,.id="Stages")%>%as_tibble()


opt_temp%>%group_by(Stages)%>%
mutate(Stages=factor(Stages,levels=unique(effect_temp$Stages)))%>%
	slice_min(predicted,n = 1)


#================================================================
#	Gráficos
#================================================================
dados%>%select(SEXO,temp,L1_IMERSION:ADULTO_IMERSION,-c(PUPA_L4,"L1_IMERSION"))%>%
pivot_longer(!c(SEXO,temp),names_to="Transition",values_to="Days")%>%
ggplot(.,aes(x=temp,y=Days,group=Transition,color=Transition,fill=Transition))+
    geom_smooth(method = "glm",formula = y~x+I(x^2), se = T, 
        method.args = list(family = "poisson"))+
geom_point(position = position_jitterdodge(.4),alpha=.7)+
ylab("Days between stages")+xlab("Temperature")+
theme_minimal()+
facet_wrap(.~Transition,scale="free_y")



dados%>%select(SEXO,temp,L1_IMERSION:ADULTO_IMERSION,-c(PUPA_L4,"L1_IMERSION"))%>%
pivot_longer(!c(SEXO,temp),names_to="Transition",values_to="Days")%>%
ggplot(.,aes(x=temp,y=Days,group=SEXO,color=SEXO,fill=SEXO))+
    geom_smooth(method = "glm",formula = y~x+I(x^2), se = T, 
        method.args = list(family = "poisson"))+
geom_point(position = position_jitterdodge(.4),alpha=.7)+
ylab("Days between stages")+xlab("Temperature")+
theme_minimal()+
facet_wrap(.~Transition,scale="free_y")


effect_temp%>%
filter(stats=="Temp")%>%
#filter(Stages!="Adulto_imersion")%>%
mutate(Stages=factor(Stages,levels=unique(effect_temp$Stages)))%>%
ggplot(.,aes(x=Stages,y=-Coefficient,group=stats))+
geom_line()+
geom_pointrange(aes(ymin = -Coefficient-SE, ymax = -Coefficient+SE))+
#facet_wrap(.~stats,scale="free_y")+
theme_bw(base_size=16)+
geom_hline(yintercept=0, linetype="dashed", color = "red",linesize=.5)+
#ylab("Estimated effect on development")+
#ylab(paste0("Estimated effect of temperature \n on development ",bquote(beta[max](mu~mol ~CO[2]~ m^-2~s^-1))))+
ylab(expression(atop("Estimated effect of temperature", "on development ( -1* "~beta[Temp]~")"))) +
#theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust=1))
theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust=1))
















