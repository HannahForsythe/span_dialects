library(dplyr)
library(plyr)
library(pryr)
library(ggplot2)
library(stringr)
library(lme4)
library(reshape)

################
#Data prep - Mexico City corpus (Mexican Spanish)
################
#citation: Forsythe, H., D. Greeson and C. Schmitt (to appear). "How preschoolers acquire the null-overt contrast in Mexican Spanish: Evidence from production," Colomina-Almiñana, J. & S. Sessarego (eds.). Patterns in Spanish: Structure, Context and Development. Amsterdam/Philadelphia: John Benjamins. 
#link to paper: https://hannahforsythe.weebly.com/uploads/3/0/9/7/30974925/frosytheetal-2019-hlsproceedings-revised.pdf
#This data is private, pending anonymization

JGAV <- read.csv("CDMX/nullovert123-JGAV-hcf.csv")
JGAV$dyad <- as.factor("JGAV")

KDP <- read.csv("CDMX/nullovert123-KDP-hcf.csv")
KDP$dyad <- as.factor("KDP")

KUC <- read.csv("CDMX/nullovert123-KUC-hcf.csv")
KUC$dyad <- as.factor("KUC")

YGSZ <- read.csv("CDMX/nullovert123-YGSZ-hcf.csv")
YGSZ$dyad <- as.factor("YGSZ")

EAMR <- read.csv("CDMX/nullovert123-EAMR-hcf.csv")
EAMR$dyad <- as.factor("EAMR")

YBM <- read.csv("CDMX/nullovert123-YBM-hcf.csv")
YBM$dyad <- as.factor("YBM")  

PLG <- read.csv("CDMX/nullovert123-PLG-hcf.csv")
PLG$dyad <- as.factor("PLG")

SLV <- read.csv("CDMX/nullovert123-SLV-hcf.csv")
SLV$dyad <- as.factor("SLV")

ACC <- read.csv("CDMX/nullovert123-ACC-hcf.csv")
ACC$dyad <- as.factor("ACC")

OMJ <- read.csv("CDMX/nullovert123-OMJ-hcf.csv")
OMJ$dyad <- as.factor("OMJ")

JRC <- read.csv("CDMX/nullovert123-JRC-dan.csv")
JRC$dyad <- as.factor("JRC")

#combine all data into one df
CDMX <- rbind.fill(EAMR, JGAV, PLG, SLV, YGSZ, ACC, JRC, KDP, KUC, OMJ, YBM)

#replace the problematic column names
names(CDMX) <- revalue(names(CDMX), c("."="empty", "..1"="empty1", "..2"="empty2", "..3"="empty3"))

#assign dialect variable
CDMX$dialect <- as.factor("Mexican")

#assign an ID for each speaker (concatenate the dyad and participant labels)
CDMX$speaker <- as.factor(paste0(CDMX$participant, "_", CDMX$dyad))

#for the purposes of this project, we will only look at adult speech main lines (no comments or grammatical codes) 
CDMX <- CDMX %>% filter(participant %in% c("MOT", "FAT") & tier %in% "%mor:") %>% 
  select(file, line, participant, speaker, environment, dyad, dialect)


################
#Data prep - Villa21 corpus (mixed Paraguayan/Argentinian Spanish)
################
#citation: This data is private for the moment
LI <- read.csv("Villa21/nullovert123-Lucero-grk.csv")
LI$dyad <- as.factor("LI")

ODOG <- read.csv("Villa21/nullovert123-Oscar-grk.csv")
ODOG$dyad <- as.factor("ODOG")

AG <- read.csv("Villa21/nullovert123-Araceli-hcf.csv")
AG$dyad <- as.factor("AG")

GG <- read.csv("Villa21/nullovert123-Gaston-hcf.csv")
GG$dyad <- as.factor("GG")

AC <- read.csv("Villa21/nullovert123-Angel-hcf.csv")
AC$dyad <- as.factor("AC")

BB <- read.csv("Villa21/nullovert123-Barbara-hcf.csv")
BB$dyad <- as.factor("BB")

DDZF <- read.csv("Villa21/nullovert123-Dante-hcf.csv")
DDZF$dyad <- as.factor("DDZF")

ABB <- read.csv("Villa21/nullovert123-Alexandra-hcf.csv")
ABB$dyad <- as.factor("ABB")

EGC <- read.csv("Villa21/nullovert123-Elias-hcf.csv")
EGC$dyad <- as.factor("EGC")

Villa21 <- rbind.fill(AG, ABB, EGC, GG, AC, BB, LI, DDZF, ODOG)

#replace the problematic column names
names(Villa21) <- revalue(names(Villa21), c("."="empty", "..1"="empty1", "..2"="empty2", "..3"="empty3"))

####NOTE:
####parents in this corpus speak Paraguayan Spanish, investigators speak Argentinian Spanish, and kids speak a mix
####speaker is coded differently for investigators, so we will prepare data SEPARATELY for each dialect  

#As with the other files, select only adult speech main lines (no comments or grammatical codes)
PS <- Villa21 %>% filter(participant %in% c("MOT", "MOT2", "FAT") & tier %in% "%mor:") %>% 
  select(file, line, participant, environment, dyad)
  #assign dialect variable:
  PS$dialect <- as.factor("Paraguayan")
  #assign speaker variable
  PS$speaker <- as.factor(paste0(PS$participant, "-", PS$dyad))

ArgS <- Villa21 %>% filter(participant %in% c("INV") & tier %in% "%mor:") %>% 
  select(file, line, participant, environment, dyad)
  #assign dialect variable:
  ArgS$dialect <- as.factor("Argentinian")
  #assign speaker variable: Detect which investigator code appears in the filename and use it as speaker ID
    inv_codes <- c("-EB|-SDS|-MDLR|-ALC|-MMU|-MM")
    ArgS$speaker <- as.factor(str_extract(ArgS$file, inv_codes))
    #drop lines where filename improperly left out the investigator code
    ArgS <- subset(ArgS, !is.na(ArgS$speaker))
    #correct improper tag -MM to -MMU
    ArgS$speaker <- revalue(ArgS$speaker, c("-MM"="-MMU"))
    #concatenate with participant code, to be formatted like the other speaker IDs
    ArgS$speaker <- as.factor(paste0(ArgS$participant, ArgS$speaker))

    
################
#Data prep - Diez Itza corpus (Spain (Oviedo) - 2 adult speakers, 20 children)
################
#citation: Diez-Itza, E. (1995). Procesos fonológicos en la adquisición del español como lengua materna. In J.M. Ruiz, P. Sheerin, & E. González-Cascos (Eds.), Actas del XI Congreso Nacional de Linguistica Aplicada. Valladolid:Universidad de Valladolid.
#https://childes.talkbank.org/access/Spanish/DiezItza.html
  
diezitza <-read.csv("DiezItza/DiezItza_all.csv")

#As with the other files, select only adult speech main lines (no comments or grammatical codes)
Spain1 <- diezitza %>% filter(participant %in% c("MOT", "INV") & tier %in% "%mor:") %>% 
  select(file, line, participant, environment)
  #assign dialect variable:
  Spain1$dialect <- as.factor("Spain")
  #assign speaker variables
  Spain1$speaker <- as.factor(paste0(Spain1$participant, "-DZIZ"))
      
################
#Data prep - Linaza corpus (Spain (Madrid) - 2 adult speakers, 1 child)
################
#citation: Linaza, J., Sebastián, M. E., & del Barrio, C. (1981). Lenguaje, comunicación y comprensión. La adquisición del lenguaje. Monografía de Infancia y Aprendizaje, 195-198.
#https://childes.talkbank.org/access/Spanish/Linaza.html
  linaza <-read.csv("Linaza/Linaza_all.csv")
  #check participant codes
  levels(linaza$participant)
  
  #As with the other files, select only adult speech main lines (no comments or grammatical codes)
  Spain2 <- linaza %>% filter(participant %in% c("ADU", "ANA", "CUI", "MAD", "PAD") & tier %in% "%mor:") %>% 
    select(file, line, participant, environment)
  #assign dialect variable:
  Spain2$dialect <- as.factor("Spain")
  #assign speaker variables
  Spain2$speaker <- as.factor(paste0(Spain2$participant, "-LIN"))
  
################
#Data prep - OreaPine corpus (Spain (Madrid) - 2 adult speakers, 2 children)
################
#citation: Aguado-Orea, J., & Pine, J. M. (2015). Comparing different models of the development of verb inflection in early child Spanish. PloS one, 10(3), e0119613.
#https://childes.talkbank.org/access/Spanish/OreaPine.html
################
#Data prep - FernAguado corpus (Spain (Pamplona) - 40 adult speakers, 40 children)
################
#citation: not given
#https://childes.talkbank.org/access/Spanish/FernAguado.html
  
    
################
#Data prep - Jackson Thal corpus (Mexican Spanish from Querétaro)
################
#ciation: Jackson-Maldonado, D. & Thal, D. (1993). Lenguaje y Cognición en los Primeros Años de Vida. Project funded by the John D. and Catherine T. MacArthur Foundation and CONACYT, Mexico

#Hess corpus (one adult w/ 24 kids, Mexico City)
#citation
        
################
#Data prep - Shiro corpus (Venezuelan Spanish)
################
################
#primary dataset
################
#combine all dialects into one df
df <- rbind.fill(CDMX, ArgS, PS,  Spain1, Spain2)

#measure size in rows and MB
  nrow(df)  
  object_size(df)
  
#include only the talkative speakers (>=200 utterances)
  spkrs <- ddply(df, .(speaker), summarise, utterances = length(environment)) 
  loud_ones <- spkrs$speaker[spkrs$utterances>=200]
  df <- df %>% filter(speaker %in% loud_ones)
  
################
#rate of pronominal subjects
################
    
#Define lexical items to search for in each line.
  #'vos' (Later, also include 'vo(s)' and punctuation afterward)
  vos <- c(" vos ")
  #'tú'
  tu <- c(" tú ")
  #'usted/ustedes' (Later, also include 'ustede(s)')
  ustedes <- c(" usted | ustedes ")
  #'yo'
  yo <- c(" yo ")
  #'nosotros/as'
  nosotros <- c(" nosotr")
  #él/ellos/ella/ellas (Later, also include 'ello(s) and ella(s')
  ellos <- c(" él | ella | ellos | ellas ")
  
#For each lexical item, define a variable counting its occurrences in each line . (Later, turn this into a for-loop)
  
  df$vos <- str_count(df$environment, vos)
  df$tu <- str_count(df$environment, tu)
  df$ustedes <- str_count(df$environment, ustedes)
  df$yo <- str_count(df$environment, yo)
  df$nosotros <- str_count(df$environment, nosotros)
  df$ellos <- str_count(df$environment, ellos)

#Define a variable summing over all pronoun types
  df$pronoun_count <- df$vos + df$tu + df$ustedes + df$yo + df$nosotros + df$ellos
  
#For the denominator, count how many verb tags are in each ine
  verbtag <- c(" v\\|| aux\\|| cop\\|")
  df$verb_count <- str_count(df$environment, verbtag)
  
#eliminate lines without verbs in them
  verbs <- subset(df, df$verb_count>0)

#For each speaker and dialect, get the proportion of each kind of pronoun. 
  #(This is a proxy for the proportion of verbs that have these pronouns as subjects.)
  dialect_pro <- ddply(verbs, .(dialect, speaker), summarise, 
                       pronoun_prop=mean(pronoun_count/verb_count),
                       vos_prop=mean(vos/verb_count),
                       tu_prop=mean(tu/verb_count),
                       ustedes_prop=mean(ustedes/verb_count),
                       yo_prop=mean(yo/verb_count),
                       nosotros_prop=mean(nosotros/verb_count),
                       ellos_prop=mean(ellos/verb_count))
#calculate the average proportion for each dialect and append that "average speaker" to the previous dataframe
  dialect_avg <- ddply(verbs, .(dialect), summarise, 
                       pronoun_prop=mean(pronoun_count/verb_count),
                       vos_prop=mean(vos/verb_count),
                       tu_prop=mean(tu/verb_count),
                       ustedes_prop=mean(ustedes/verb_count),
                       yo_prop=mean(yo/verb_count),
                       nosotros_prop=mean(nosotros/verb_count),
                       ellos_prop=mean(ellos/verb_count))
  dialect_avg$speaker <- as.factor("AVG")
  
  dialect_pro_avg <- rbind(dialect_pro, dialect_avg)

#For each speaker and dialect, plot the proportion of each kind of pronoun.
  #melt the data for plotting 
  melt_dialect_pro <- melt(dialect_pro, id.vars=c("dialect", "speaker"))
  #fix the column names
  names(melt_dialect_pro) <- revalue(names(melt_dialect_pro), c("variable"="pronoun", "value"="proportion"))
  #fix the pronoun names
  levels(melt_dialect_pro$pronoun) <- c("all pronouns", "vos", "tu", "ustedes", "yo", "nosotros", "ellos")
  #plot. Looks good as landscape 6"h x 9"w
  pro.plot <- ggplot(melt_dialect_pro, aes(x=dialect, y=proportion, color = dialect)) +
    facet_grid(.~pronoun) +
    ylim(0,.165) +
    geom_point(shape=1) +
    theme_bw() +
    theme(legend.position="top", axis.text.x=element_blank()) +
    ggtitle("Estimated proportion of different types of subject pronouns across dialects of Spanish") +
    ylab("")
  
  ggsave("Asset_1-prop_subject_pronouns.pdf", dpi=900, dev='pdf', height=6, width=9, units="in")
  