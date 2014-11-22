---
layout:     post
title:      Table de maintien en incapacité
date:       2014-11-21 23:50:18
summary:    Procédure de construction des taux d'expérience
categories: modèle de durée
published : true
---

# lxt #

La fonction *KapMei_2* calcule la fonction de survie brute et en déduit les lxt

{% highlight R %}

KapMei_2 <- function( df, cause=c("Acc"),DebObs, FinObs, anc_min=0, anc_max=1095 ){

df1<- df[df$cause2 %in% cause,]
df1$anc_ent_jour <- with(df1, as.numeric(ifelse(debut < DebObs, difftime(DebObs,debut,unit='days'),0)))
df1$anc_sor_jour <- with(df1,as.numeric(difftime(pmin(fin, FinObs), debut,unit="days")))

df1<- df1[df1$anc_sor_jour > df1$anc_ent_jour, ]

w <- survfit(Surv(time = anc_ent_jour, time2 = anc_sor_jour,event= non_censure,type="counting")~1, data=df1,  type="kaplan-meier",conf.type="plain",conf.int=0.95)

 S   <-c(1, rep(0,1095))
   
  for(x in anc_min:anc_max)
  {
    S[x+1]=min(w$surv[w$time<=x])
  }
  
df2 <- data.frame(t=(anc_min : anc_max ), l_xt = S * 10000)
  return(df2)	
}

{% endhighlight %}

On applique la fonction à la base de données

{% highlight R %}

base1 <- base0[base0$age %in% age_vec,]

taux_ac<- by(base1, base1[,c("tr_age")], KapMei_2, cause= "Acc",DebObs= debut_obs, FinObs= fin_obs,anc_max=anciennete_max)

taux_ma <- by(base1, base1[,c("tr_age")], KapMei_2, cause= "Mal",DebObs= debut_obs, FinObs= fin_obs,anc_max=anciennete_max)


taux 	<- list(taux_ac, taux_ma)
names(taux)  <- c("Accident","Maladie")

taux1 	<- lapply(taux, function(element) { 
	df 		<- rbindlist(element)
	df$x  	<- rep(tr_age_vec, each= anciennete_max+1)
	return(df)
	})

save(taux1, file="Processed/Data/taux1.RData")

#Matrice des lxt bruts de maintien : accident / maladie 
taux1_hor <- lapply(taux1, function(df) { dcast(df, t ~ x, , value.var="l_xt")})

save(taux1_hor, file="Processed/Data/taux1_hor.RData")

{% endhighlight %}

Le data frame de lxt est récupéré à la sortie de la fonction KapMei_2(). Il est également sauvegardé dans *Processed/Data/taux1_hor.RData*

Il est de format suivant : 

* en ligne : ancienneté de maintien en jours
* en colonne : tranche d'âges d'entrée en incapacité

{% highlight R %}

> head(lx_acc)
  t        18        35        40        45        50        55
1 0 10000.000 10000.000 10000.000 10000.000 10000.000 10000.000
2 1 10000.000  9993.987  9983.962 10000.000 10000.000  9982.100
3 2  9972.863  9987.974  9971.933  9985.668  9995.826  9964.200
4 3  9966.079  9981.960  9955.881  9967.753  9987.479  9934.332
5 4  9938.942  9957.907  9951.868  9957.001  9974.958  9922.384
6 5  9925.373  9927.823  9943.839  9946.244  9949.917  9886.520

{% endhighlight %}


# DRT #

Calculer la DRT(x,t) à partir des l_xt

On écrit la fonction générique *rolling_sum()* pour calculer la somme glissante d'un vecteur *vec_in* de *sf* pas (sf pour steps forward) 

{% highlight R %}

rolling_sum <- function( vec_in , sf )
{ 
   vec <- c(vec_in, rep(0,sf-1))
   vec_out 	<-  rowSums(embed(vec,sf))
   return(vec_out)
}

test_vec <- (18:65) 
rolling_sum(test_vec,20)

{% endhighlight %}

On peut en déduire la fonction *drt_vec()* qui prend un vecteur de lt pour un âge donné et donne à la sortie un vecteur drt.

{% highlight R %}

drt_vec <- function(lxt_vec, pas )
	{
	lxt_rs <- rolling_sum(lxt_vec, sf = pas )
	drt <- ifelse(lxt_vec == 0, 0, lxt_rs / lxt_vec - 0.5)
	return(drt)
	}

{% endhighlight %}

* Pour la méthode 1, i.e à l'horizon de 3 ans, la variable *pas = 1095* 
* Pour la méthode 2, i.e à l'horizon de 3 ans, la variable *pas = 365* 

Finalement, en utilisant *lapply()*, on peut transformer un dataframe lxt en drt.

{% highlight R %}

drt3a_acc<- data.frame(anc = lx_acc$t, lapply(lx_acc[,-1], function(col) drt_vec(col, pas= 1095)))
drt3a_mal<- data.frame(anc = lx_mal$t, lapply(lx_mal[,-1], function(col) drt_vec(col, pas= 1095)))

drt1a_acc<- data.frame(anc = lx_acc$t, lapply(lx_acc[,-1], function(col) drt_vec(col, pas= 365)))
drt1a_mal<- data.frame(anc = lx_mal$t, lapply(lx_mal[,-1], function(col) drt_vec(col, pas= 365)))

{% endhighlight %}
