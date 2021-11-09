require(OurTools)

JenaR::Table(dfBL$geschl,dfBL$klasse)

# geschlecht ohne Angabe raus
# divers zu männlich

# ohne klasse raus

JenaR::Table(dfBL$alter,dfBL$klasse)
# ZUordnung KLasse bezüglich Alter: eine 15 jähriger in Klasse 8. 



class(dfBL$schlaf02)

dhms <- function(t){
  paste(t %/% (60*60*24)
        ,paste(formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0"),
          formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0")
          ,formatC(t %% 60, width = 2, format = "d", flag = "0")
          ,sep = ":"
        )
  )
}
hms <- function(t){
  #paste(#t %/% (60*60*24) ,
        x <- paste(formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0")
          ,formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0")
          ,formatC(t %% 60, width = 2, format = "d", flag = "0")
          ,sep = ":"
        )
        x <- na_if(x,"NA:NA:NA")
        return(x)
  #)
}

#install.packages("chron")
#library(chron)
#dfBL$schlaf02a<-chron(times = hms(dfBL$schlaf02))#
dfBL$schlaf02a<-hms(dfBL$schlaf02)
class(strptime(dfBL$schlaf02a,"%H:%M"))
dfBL$schlaf02a<-factor(hms(dfBL$schlaf02))
class(as.time(dfBL$schlaf02a))
Table(strptime(dfBL$schlaf02a,"%H:%M"))

hist(as.numeric(dfBL$schlaf02a))
hms(dfBL$schlaf02)


require(stats)


dfBL2 <- dfBL %>%
  filter(alter < 9999) %>%
  filter(chronotyp < 43) %>%
  filter(geschl %in% c("männlich", "weiblich")) %>%
  mutate(klassef = factor(klasse),
         klassef2= relevel(klassef, "7"),
         geschlf = factor(geschl),
         alterf = factor(alter),
         alter14 = as.numeric(alter > 13),
         alter15 = as.numeric(alter > 14))

table(dfBL2$alterf)

fit <- glm(chronotyp ~ klassef + geschlf + klassef:geschlf, data = dfBL2)
summary(fit)

fit <- glm(chronotyp ~ alterf + geschlf + alterf:geschlf, data = dfBL2)
summary(fit)

fit <- glm(chronotyp ~ alter14  + geschlf, data = dfBL2)
summary(fit)
fit <- glm(chronotyp ~ alter15  + geschlf, data = dfBL2)
summary(fit)



fit <- glm(chronotyp ~ geschlf, data = dfBL2)
summary(fit)


fit2 <- lm(chronotyp ~ klassef2, data = dfBL2)
summary(fit2)


# weiteres Analyse: schlafzeit in Minuten mit in Modell


