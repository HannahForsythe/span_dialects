# span_dialects
Exploratory analysis of dialectal differences in Spanish, using data from https://childes.talkbank.org/

	Not everyone who speaks the same language speaks it in the same way. In England, folds are currently passing the COVID-19 crisis at home in their 'pants' getting 'pissed' while here in the U.S. we are in our 'undies' getting 'wasted'. Differences like this can pose a challenge to humans and machines, especially when it comes to processing speech with different accents. 
	One piece of knowledge that could make accent detection more tractable is what I call dialect detection. That is, detecting the individual words ('pissed' vs. 'wasted') that characterize a particular dialect, as opposed to the pronunciations of those words. Take a simple illustration: if you pick up the telephone and hear a foreign accent, it might be difficult to understand. But if the phone rings and you notice that the call is coming from Australia, you might go into the call prepared to rely on whatever past experiences with that accent you have had, and the call might go more smoothly. I other words, the top-down information about which dialect you are about to hear primes you to better process the bottom-up information of the accent you are about to hear. 
	This project takes one step towards identifying dialects in Spanish. In this initial analysis, I explore how useful it is to use (i) subject pronouns, and (ii) specific verb stems to distinguish between dialects. 
	For the larger project, I propose to build a classifier that can identify someoneÕs dialect, given an utterance of reasonable length. The exploratory analysis done here shows that useful features for this classifier to use may include: (i) types and overall rates of subject pronoun use, and (ii) types of  
	I use a dataset compiled from free-speech corpora publicly available from https://childes.talkbank.org/access/Spanish/, supplemented with two corpora from my own research. Each corpus contains upwards of 20 text transcripts of children and parents interacting in naturalistic settings (playing, storytelling, games). Each utterance has been tagged for speaker and the words have been passed through the part-of-speech tagger designed for Spanish by Brian MacWhinney and described here: https://talkbank.org/manuals/MOR.pdf . An important feature of this tagger is that it identifies verb stems (ex. esta-) so that different inflections of the same verb (ex. estoy, Ô(I) amÕ, est‡s Ô(you) areÕ, etc.) can be easily grouped together.
	I chose to use free-speech data, since this is the kind of speech that systems like Alexa need to understand. In this project, I will only be analyzing adult speech, but future projects can take advantage of childrenÕs speech to answer clinical questions (ex. ÒIs this child using the kind of speech appropriate for their age?Ó) or general research questions (ex. ÒFor children exposed to multiple dialects, what proportion of their utterances can be classified as dialect 1 vs. dialect 2?Ó). 

Discussion of figure 1 (https://github.com/HannahForsythe/span_dialects/blob/master/Asset_1-prop_subject_pronouns.pdf): 

Figure 1 shows an estimate of how often each speaker produces pronouns in subject position. The left panel shows the overall proportion of pronominal subjects, while the other panels break this down by type (vos Ôyou-informalÕ, tœ Ôyou-informalÕ, usted/ustedes Ôyou-formal-SG/PLÕ, yo ÔIÕ, nosotros/nosotras Ôwe-masc/femÕ, Žl/ella/ellos/ellas Ôhe/she/they-masc/they-femÕ). This figure suggests the following ideas:

1) The overall rate of pronominal subjects (left panel) is highest for Paraguayan Spanish; and it is also the most variable for this dialect. This accords with it being a contact dialect Ð many speakers of this dialect speak other languages or are in contact with other dialects through immigration. Sociolinguistic researchers have found that pronoun rates rise at variable rates in this type of dialect.
2) The pronoun vos (2nd panel) s not used in Mexican or Spain Spanish, and tœ (3rd panel) is not used in Argentinian or Paraguayan Spanish, in agreement with grammatical descriptions. More interestingly, however, Argentinians use vos more often on average than Paraguayans Ð this is potentially a useful metric to distinguish the two dialects. 
3) The extreme variability of Paraguayan Spanish makes it tough to distinguish from the others. However, the use of pronouns Žl/ella/ellos/ellas (last panel) really seem to differentiate it from the other three dialects. This is another potentially useful feature for our proposed classifier!

Discussion of figure 2 (https://github.com/HannahForsythe/span_dialects/blob/master/Asset_2-ratio_verb_pairs.pdf)

Figure 2 shows the ratio between each speakerÕs production of verb pairs estar-ser (left panel) and conducir-manejar (right panel). Since these verb pairs mean similar things (estar/ser ÔbeÕ; conducir/manejar ÔdriveÕ) it makes sense to ask if some dialects solve the competition between them differently. That is the ratio between estar/ser and conducir/manejar may be different across dialects. This figure confirms that general idea. More specifically:

1) The ratio estar/ser affords a greater distinction between dialects than simply reporting the proportion of each verb separately. 
2) the ratio conducir/manejar is uniformly ³ 1 for Spain and ²1 for the other dialects, conforming to grammatical descriptions. This is a nice Òsanity checkÓ but doesnÕt seem to provide any additional insight from the individual verb proportions alone. 

In sum, the exploratory analysis shows that:
1) rates of individual pronoun subjects are a promising type of feature to use when distinguishing dialects.
2) in some cases, testing the ratio of similar-meaning words may provide additional insight, over and above simply reporting the proportion of words.

Future steps will involve a full-blown exploration of all words that may be distinctive for each dialect, plus targeted meaning pairs whose proportions may vary across dialects as well.  

