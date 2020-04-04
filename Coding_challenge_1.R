##########
#about
##########

#available at: https://github.com/HannahForsythe/span_dialects
#author: Hannah Forsythe
#date: 3-April-2020
#description: Exploratory analysis of dialectal differences across Spanish. 
###Figure 1 explores the utility of two potential features for characterizing dialectal differences: (i) overall rates subject pronoun use, and (ii) rates of different types of subject pronouns
###Figure 2 explores the utility of a using certain verb stems for the same purpose
###Future goals: produce a classifier capable of identifying a dialect, given an utterance of reasonable length. 
#data: transcripts of parent-child spoken interactions in 4 different dialects, compiled from 3 corpora available at https://childes.talkbank.org/ and 2 private corpora (anticipated release: Sept. 2020)

####################
#libraries
####################
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
CDMX$speaker <- as.factor(paste0(CDMX$participant, "-", CDMX$dyad))

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
#prep each kid separately, so "FAT" and "MOT" participants can be identified as different speakers
  
  #####Lucia
  lucia <-read.csv("OreaPine/Lucia/OreaPine-Lucia.csv")

  #check participant codes
  levels(lucia$participant)
  
  #As with the other files, select only adult speech main lines (no comments or grammatical codes)
  lucia <- lucia %>% filter(participant %in% c("FAT", "MOT") & tier %in% "%mor:") %>% 
    select(file, line, participant, environment)
  #assign dialect variable:
  lucia$dialect <- as.factor("Spain")
  #assign speaker variables
  lucia$speaker <- as.factor(paste0(lucia$participant, "-LUC"))
  
  #####Juan
  #Juan has 2 files, neither with headers:  
  juan1 <-read.csv("OreaPine/Juan/OreaPine-Juan011021_020207.csv")
  juan2 <-read.csv("OreaPine/Juan/OreaPine-Juan020211_020529b.csv")
  
  #insert headers for the crucial variables so that the datasets can be bound together
  names(juan1) <- revalue(names(juan1), c("X011021.cha"="file", "X13"="line", "FAT"="participant", "Juan...mor..n.prop.Juan.."="environment"))
  names(juan2) <- revalue(names(juan2), c("X020211.cha"="file", "X13"="line", "MOT"="participant", "mira.qué.guay...mor..co.mira.look.pro.int.qué.what.co.guay.cool.."="environment"))
  juan12 <- rbind.fill(juan1, juan2)
  
  #Juan files have duplicate rows; remove them
  juan12 <- distinct(juan12)
  
  #check participant codes
  levels(juan12$participant)
  
  #As with the other files, select only adult speech main lines (no comments or grammatical codes)
  juan <- juan12 %>% filter(participant %in% c("FAT", "MOT", "AB2", "ABU", "TI1", "TI2", "TI3", "VEC")) %>% 
    select(file, line, participant, environment)
  #assign dialect variable:
  juan$dialect <- as.factor("Spain")
  #assign speaker variables
  juan$speaker <- as.factor(paste0(juan$participant, "-JUA"))
  
  ######Juan and Lucia
  #join kids into a single dataframe
  Spain3 <- rbind(lucia, juan)
  
################
#Data prep - FernAguado corpus (Spain (Pamplona) - 40 adult speakers, 40 children)
################
#citation: not given
#https://childes.talkbank.org/access/Spanish/FernAguado.html
################
#Data prep - Jackson Thal corpus (Mexican Spanish from Querétaro)
################
#ciation: Jackson-Maldonado, D. & Thal, D. (1993). Lenguaje y Cognición en los Primeros Años de Vida. Project funded by the John D. and Catherine T. MacArthur Foundation and CONACYT, Mexico
################
#primary dataset
################
#combine all dialects into one df
df <- rbind.fill(CDMX, ArgS, PS,  Spain1, Spain2, Spain3)

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
    
#Define pronoun strings to search for in each line.
  #'vos' (Later, also include 'vo(s)' )
  vos <- c(" vos | vo\\(s\\) ")
  #'tú'
  tu <- c(" tú ")
  #'usted/ustedes' (Later, also include 'ustede(s)')
  ustedes <- c(" usted | ustedes | usted\\(s\\) ")
  #'yo'
  yo <- c(" yo ")
  #'nosotros/as'
  nosotros <- c(" nosotr")
  #él/ellos/ella/ellas (also include 'ello(s) and ella(s)')
  ellos <- c(" él | ella | ellos | ellas | ello\\(s\\) | ella\\(s\\) ")
  
#For each pronoun, define a variable counting its occurrences in each line . 
  #Note: Ideally I would find some way to do this for any arbritrary vector of words.
  
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
    ylim(0,.17) +
    geom_point(shape=1) +
    theme_bw() +
    theme(legend.position="top", axis.text.x=element_blank()) +
    ggtitle("Estimated proportion of different types of subject pronouns across dialects of Spanish") +
    ylab("")
  
  #uncomment the following line to save this figure as a pdf
  #ggsave("Asset_1-prop_subject_pronouns.pdf", dpi=900, dev='pdf', height=6, width=9, units="in")
  
################
#verb exploration - proportion of ser to estar
################
#for this part of the analysis, we drop speaker EAMR-MOT, due to issues with her part-of-speech tags
  df <- subset(df, speaker!="MOT-EAMR")
#Define verb tags to search for in each line.
  #two high-frequency verbs:
  #'ser'
  ser <- c(" cop\\|se-")  
  #'estar'
  est <- c(" cop\\|esta-")  
  #two mid-frequency verbs:
  #'manejar' vs 'conduci' - two different forms for 'drive'
  maneja <- c(" v\\|maneja-")  
  conduci <- c(" v\\|conduci-")  
  #'venir' and 'ir' - come and go 
  veni <- c(" v\\|veni-")  
  i <- c(" v\\|i-")  
  
#For each verb, define a variable counting its occurrences in each line . 
  #Future step: find a way to do this for any arbritrary vector of words.
  
  df$ser_count <- str_count(df$environment, ser)
  df$est_count <- str_count(df$environment, est)
  df$maneja_count <- str_count(df$environment, maneja)
  df$conduci_count <- str_count(df$environment, conduci)
  df$veni_count <- str_count(df$environment, veni)
  df$i_count <- str_count(df$environment, i)
  
#For speaker, and dialect get the average rate of occurrence of each of these verbs out of all verbs:
  #first, eliminate lines without verbs in them.
  verbs <- subset(df, df$verb_count>0)
  #then, divide each of the verb counts by the total verb count, within each speaker/dialect combo
  dialect_verbs <- ddply(verbs, .(dialect, speaker), summarise, 
                         ser_prop=mean(ser_count/verb_count),
                         est_prop=mean(est_count/verb_count),
                         maneja_prop=mean(maneja_count/verb_count),
                         conduci_prop=mean(conduci_count/verb_count),
                         veni_prop=mean(veni_count/verb_count),
                         i_prop=mean(i_count/verb_count))
  
  
#For each speaker and dialect, plot the proportion of each verb stem
  #melt the data for plotting 
  melt_dialect_verbs <- melt(dialect_verbs, id.vars=c("dialect", "speaker"))
  #fix the column names
  names(melt_dialect_verbs) <- revalue(names(melt_dialect_verbs), c("variable"="verb", "value"="proportion"))
  #fix the verb names
  levels(melt_dialect_verbs$verb) <- c("ser","estar","manejar","conducir","venir","ir")
  #plot high-frequency verbs, mid-frequency, and low-frequency separately
  hifrq <- subset(melt_dialect_verbs, verb %in% c("ser","estar"))
  midfrq <- subset(melt_dialect_verbs, verb %in% c("venir","ir"))
  lowfrq <- subset(melt_dialect_verbs, verb %in% c("manejar","conducir"))
  
  hifrq.plot <- ggplot(hifrq, aes(x=dialect, y=proportion, color = dialect)) +
    facet_grid(.~verb) +
    ylim(0,.3) +
    geom_point(shape=1) +
    theme_bw() +
    theme(legend.position="top", axis.text.x=element_blank()) +
    ggtitle("Estimated proportion of different types of verbs across dialects of Spanish") +
    ylab("")
  
  midfrq.plot <- ggplot(midfrq, aes(x=dialect, y=proportion, color = dialect)) +
    facet_grid(.~verb) +
    ylim(0,.25) +
    geom_point(shape=1) +
    theme_bw() +
    theme(legend.position="top", axis.text.x=element_blank()) +
    ggtitle("Estimated proportion of different types of verbs across dialects of Spanish") +
    ylab("")
  
  lowfrq.plot <- ggplot(lowfrq, aes(x=dialect, y=proportion, color = dialect)) +
    facet_grid(.~verb) +
    ylim(0,.01) +
    geom_point(shape=1) +
    theme_bw() +
    theme(legend.position="top", axis.text.x=element_blank()) +
    ggtitle("Estimated proportion of different types of verbs across dialects of Spanish") +
    ylab("")

  #follow-up plot. Since ser and estar mean similar things,
  #it makes sense to ask if some dialects rely on them to different degrees
  #i.e., the ratio between one and the other may be different
  #The same goes for 'manejar' and 'conducir'
  ratios <- dialect_verbs %>% select(dialect, speaker, ser_prop, est_prop, maneja_prop, conduci_prop)
  #calculate ratio of estar to ser
  ratios$estar_over_ser <- ratios$est_prop / ratios$ser_prop
  #calculate ratio of conducir to manejar. but first, smooth out zeros by adding a tiny amount
  ratios$maneja_prop <- (ratios$maneja_prop)+.01
  ratios$conduci_prop <- (ratios$conduci_prop)+.01
  ratios$conduci_over_maneja <- ratios$conduci_prop / ratios$maneja_prop
  
  #Check: this estar:ser graph gives us a little more separation between dialects than we saw in hifrq.plot
  ggplot(ratios, aes(x=dialect, y=estar_over_ser, color = dialect)) +
    geom_point(shape=1) +
    theme_bw() +
    theme(legend.position="top", axis.text.x=element_blank()) +
    ggtitle("Estimated ratio of 'estar' to 'ser' across dialects of Spanish") +
    ylab("")
  
#Final plot (asset 2): For each speaker and dialect, plot the ratio of each verb-meaning pair
  #drop unnecessary columns
  verbpairs <- ratios %>% select(dialect, speaker, estar_over_ser, conduci_over_maneja)
  #melt the data for plotting 
  melt_verbpairs <- melt(verbpairs, id.vars=c("dialect", "speaker"))
  #fix the column names and levels
  names(melt_verbpairs) <- revalue(names(melt_verbpairs), c("variable"="verb_pair", "value"="ratio"))
  levels(melt_verbpairs$verb_pair) <- c("estar:ser","conducir:manejar")
  #Plot. Looks good as portrait 6"h x 7"w
  verbpairs.plot <- ggplot(melt_verbpairs, aes(x=dialect, y=ratio, color = dialect)) +
    facet_grid(.~verb_pair) +
    ylim(0,2) +
    geom_point(shape=1) +
    theme_bw() +
    theme(legend.position="top", axis.text.x=element_blank()) +
    ggtitle("Ratios between similar-meaning verb pairs across dialects of Spanish") +
    ylab("")
  #uncomment the following line to save this figure as a pdf
  #ggsave("Asset_2-ratio_verb_pairs.pdf", dpi=900, dev='pdf', height=6, width=7, units="in")
  