#Figures for Nature Communications Submission 
library(plyr)
library(patchwork)
library(ggplot2)
library(ggpubr)
library(dplyr)
library("UpSetR")
library("grid")
library("ggplotify")
library(Rtsne)
library(lsmeans)
library("forestmodel")
library(survival)
library(survminer)
library(dplyr)

#Data from the EPIC cohort 
setwd("")
epicCohort= read.table("./epicCohort.txt")

#Summary Statistics from SFSCode Simulations and EPIC cohort 
stats_epic = read.table("./stats_epic.txt")
stats_sims = read.table("./stats_sims.txt")

#Class predictions for each individual in the EPIC cohort 
class.m.se = read.table("./classPredictions.txt")

#Predictions on test set (class) and true 
preds.all = write.table(preds.all, "./testPredictions.txt")
all_test <- read.table("./test.txt")

#Files to infer mutation rate 
##Predictions for a population size of 10,000 (10 predictions per individual)
mut.rates = read.table("./mutRates.txt")
##Mutation rate predictions scaled across population sizes 
mut.rates.all = read.table("./mutRates.all.txt")
##Mutation rate predictions for held out set of simulated data 
mutPreds_Sims = read.table("./mutPreds_Simulations.txt")

#Supplementary Figure 1: plot the distribution of summary statistics for real data vs simulated data 
by_mod <-stats_sims %>% group_by(model)
by_mod = na.exclude(by_mod)
stats_sims.pca.l <- sample_n(by_mod, 5000)
stats_sims.pca = stats_sims.pca.l[,c(6:21)]
stats_epic.pca = stats_epic[,2:17]
stats_sims.pca$data = paste("simulated", stats_sims.pca.l$model, sep ="_")
stats_epic.pca$data = "epic"
stats.pca = rbind(stats_epic.pca, stats_sims.pca)
stats.data <- stats.pca[ ,1:16] 
stats.labels <- stats.pca[ ,17]
stats.labels = as.factor(stats.labels)
tsne_results <- Rtsne(stats.data, perplexity=30, check_duplicates = FALSE)
embedding <- as.data.frame(tsne_results$Y)
embedding$Data <- stats.labels
embedding$Data = as.character(embedding$Data)
embedding$Data[embedding$Data=="simulated_0"] <- "Simulated Neutral"
embedding$Data[embedding$Data=="simulated_1"] <- "Simulated Positive"
embedding$Data[embedding$Data=="simulated_2"] <- "Simulated Negative"
embedding$Data[embedding$Data=="simulated_3"] <- "Simulated Combination"
embedding$Data[embedding$Data=="epic"] <- "Observed"
embedding$Data = factor(embedding$Data, levels = c("Simulated Neutral", "Simulated Positive","Simulated Negative","Simulated Combination", "Observed"))
supp1 <-  ggplot(embedding %>% arrange(Data), aes(x=V1, y=V2, color=Data, shape = Data)) +  scale_color_manual(name = "Data Source", values = c("lightskyblue", "seagreen4", "red4", "salmon1", "black"))+
  geom_point(size=2, shape = 16) + guides(colour = guide_legend(override.aes = list(size=6))) +
  xlab("") + ylab("") + ggtitle("") + theme_light(base_size=20) + theme(strip.background = element_blank(),strip.text.x= element_blank(),
                                                                        axis.ticks = element_blank(), panel.border= element_blank())
supp1

#Figure 1e: Classification performance for simulated evolutionary classes. 
preds.all.m <- ddply(preds.all,.(sample),colwise(mean))
short.class <- preds.all.m[,2:5]
class.all <- colnames(short.class)[apply(short.class[,1:4],1,which.max)]
class.all <- cbind(class.all, preds.all.m$True)
colnames(class.all) <- c("Pred", "True")
class.all <- as.data.frame(class.all)
class <- as.data.frame(cbind(True=c(0,1,2,3), Desc = c("Neutral", "Positive", "Negative", "Combination")))
class$True <- as.factor(class$True)
class.all <- join(class.all, class)
class.all <- class.all[,c(1,3)]
conf.preds.all <- as.data.frame(class.all %>% group_by_all %>% count)
colnames(conf.preds.all) <- c("Pred","True", "freq")
conf.preds.all.e <- expand.grid(unique(conf.preds.all$Pred), unique(conf.preds.all$True))
colnames(conf.preds.all.e) <- c("Pred", "True")
conf.preds.all <- merge(conf.preds.all.e, conf.preds.all, by = c("Pred","True"), all =T)
conf.preds.all$freq[is.na(conf.preds.all$freq)] <- 0
totals <- as.data.frame(table(class.all$Desc))
colnames(totals) <- c("True", "totals")
conf.preds.all <- merge(conf.preds.all, totals, all = T)
conf.preds.all$freq[is.na(conf.preds.all$freq)] <- 0
fig1e <- conf.preds.all %>%
  data.frame() %>% mutate(Prediction_Desc = factor(Pred, levels = c("Positive", "Combination", "Negative", "Neutral"))) %>%
  mutate(Reference_Desc=factor(True, levels = c("Positive", "Combination", "Negative", "Neutral"))) %>%group_by(Reference_Desc) %>% mutate(total = totals,frac_fill = freq/totals) %>%
  ggplot(aes((Prediction_Desc), (Reference_Desc), fill = frac_fill)) + theme_bw()+geom_tile() +geom_text(aes(label = str_c(round(frac_fill * 100, digits = 1), "%", "\n", "n=", freq)), size = 6, colour = "black") +
  scale_fill_gradient(low = "lightblue", high = "red") + scale_x_discrete(position = "bottom") + geom_tile(color = "black", fill = "black", alpha = 0)+xlab("Predicted Class")+ylab("True Class")+
  theme(plot.title = element_text(hjust = 0.5, size =  20), text = element_text(size = 20, family = "Helvetica"))+
  theme(legend.title=element_blank(), legend.position = "none")+ggtitle("")+theme(plot.title = element_text(face = "bold"))
fig1e

#Standard error asssociated with each class prediction (Supplementary Figure2A)
supp2A <- ggplot(class.m.se, aes(x=reorder(Sample, Mean), y=Mean, colour = Desc)) +theme_bw()+
          geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se), colour = "lightblue", width=.8)+
          geom_point(size = 2, colour = "darkblue")+theme_pubr()+
          theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16, family = "Helvetica"), legend.position = "none")+ggtitle("")+
          theme(plot.title = element_text(face = "bold"))+xlab("EPIC Sample")+ylab("Probability of Maximum Class")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
          theme(axis.text.x=element_blank())+ylim(0,1)
supp2A

#Uncertainty associated with each class prediction(Supplementary Figure 2B)
supp2B <- ggplot()+geom_boxplot(data = class.m.se, aes(Desc, se, fill = Desc))+scale_fill_manual(values = c("orange", "red", "lightblue", "darkgreen"))+theme_pubr()+
          theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16, family = "Helvetica"), legend.position = "none")+ggtitle("")+
          theme(plot.title = element_text(face = "bold"))+xlab("Predicted Class")+ylab("Standard Error")
supp2B

#Supplementary Figure 3: Impact of parameters on predictive accuracy. 
all_test <- cbind(class.all, all_test)
all_test$correct <-ifelse(all_test$Pred==all_test$Desc, 1, 0)
all_test.incorrect <- all_test[all_test$Desc=="Negative"|all_test$Desc=="Neutral",]
melted.all_test <- all_test.incorrect[,c(1,2,3:6,25)]
melted.all_test <- reshape2::melt(melted.all_test, id.vars = c("Pred","Desc", "correct"))
melted.all_test <- melted.all_test[melted.all_test$variable!="meanGammaD" & melted.all_test$variable!="probBeneficial",]
melted.all_test2 <- plyr::count(melted.all_test, c("correct", "variable", "value"))
totals <- plyr::count(melted.all_test, c("variable", "value"))
colnames(totals) <- c("variable", "value", "Total")
melted.all_test <- join(melted.all_test2, totals)
melted.all_test$prop <- melted.all_test$freq/melted.all_test$Total
melted.all_test$variable <- ifelse(melted.all_test$variable=="mutRate", "Mutation Rate", "Negative Selection Coefficient")
melted.all_test$correct <- ifelse(melted.all_test$correct==0, "Incorrect", "Correct")

supp3 <-ggplot() + theme_pubr()+scale_fill_manual(name = "Prediction", values = c("darkgreen", "red"))+ylim(0,1)+
        geom_bar(data = melted.conf, aes(factor(value), prop, fill = factor(correct)), stat = "identity", position = "dodge", colour = "black")+facet_wrap(~variable, scales = "free")+
        xlab("")+ylab("Frequency")+theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16, family = "Helvetica"))+
        ggtitle("")+theme(plot.title = element_text(face = "bold"))+ theme(legend.position="bottom")
supp3

#Supplementary Figure 4. Impact of cumulative selective effect on accuracy. 
nonsyn.del <- conf.incorrect[,c(1,2,3:6,18,25)]
nonsyn.del$EffectiveDel <- nonsyn.del$nbNonSynAllSNPs*nonsyn.del$meanGammaP
nonsyn.del$correct <- ifelse(nonsyn.del$correct==0, "Incorrect", "Correct")
supp4 <-  ggplot(data = nonsyn.del, aes(x=factor(meanGammaP), y = EffectiveDel, fill = factor(correct)))+geom_boxplot()+
          scale_fill_manual(name = "Classification", values = c("darkgreen", "red"))+theme_pubr()+
          xlab("Negative Selection Coefficient")+ylab("Cumulative Selective Effect")+ggtitle("Impact of Cumulative Selective Effect on Accuracy")+
          theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18, family = "Helvetica"))+theme(plot.title = element_text(face = "bold"))+ theme(legend.position="bottom")
supp4

#Supplementary Figure 5: Impact of nonsynonymous mutation count on prediction uncertainty 
epic.uncertain <- class.m.se
epic.uncertain <- join(epic.uncertain, stats_epic)
epic.uncertain$correct <- ifelse(epic.uncertain$Mean>0.99,1,0)
epic.uncertain <- epic.uncertain[epic.uncertain$Desc=="Neutral"| epic.uncertain$Desc=="Negative",]
epic.uncertain <- epic.uncertain[epic.uncertain$nbNonSynAllSNPs<100,] #remove outliers 
supp5 <-  ggplot(epic.uncertain, aes(x=log10(nbNonSynAllSNPs), y=Mean)) + 
          geom_point()+ theme_pubr()+ xlim(0, 1.26)+ geom_smooth(method=lm, colour = "Red") + theme(text = element_text(size=16, family = "Helvetica"))+ylim(0.4,1)+
          theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16, family = "Helvetica"), legend.position = "none")+ggtitle("Impact of Nonsynonymous Mutation Count on Softmax Probability")+
          theme(plot.title = element_text(face = "bold"))+xlab("log10(#Nonsynonymous Mutations)")+ylab("Mean")
supp5

###Figure 2A: Evolutionary classes in preleukemic and healthy blood populations
c = plyr::count(class.m.se, c("developed_AML", "Desc"))
t = plyr::count(class.m.se, c("developed_AML"))
colnames(t) = c("developed_AML", "total")
ct = join(c,t)
ct$prop = ct$freq/ct$total
fig2A <-  ggplot()+theme_pubr()+scale_fill_manual(name = "Group", values = c("blue", "red"))+
          geom_bar(data = ct, aes(x=Desc, y = prop, fill = developed_AML), stat = "identity", position = "dodge", colour = "black")+
          geom_text(data = ct, aes(x = Desc, y = prop, label = scales::percent(prop, suffix = ""), group = developed_AML), stat = "identity", vjust = +1.5, position = position_dodge(0.8), colour = "white", size = 4)+
          theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12, family = "Helvetica"))+ggtitle("")+
          theme(plot.title = element_text(face = "bold"))+xlab("Evolutionary Class")+ylab("Proportion")+ theme(axis.text.x = element_text(angle = 30, hjust = 1))
fig2A

###Figure 2b: Age-associations across evolutionary class predictions
#plot age distribution of fit to different classes 
class.m.se$age_bins <- cut(class.m.se$age_START, breaks=c(30,40,50,60,70, 80), labels=c("30-40","40-50","50-60","60-70", "70-80"))
ageTotal <- plyr::count(class.m.se, c("developed_AML", "age_bins"))
epicTotal <- plyr::count(class.m.se, c("developed_AML", "age_bins", "Desc"))
epic_models <- merge(epicTotal, ageTotal, by = c("age_bins", "developed_AML"))
colnames(epic_models) <- c("age_bins", "developed_AML", "Prediction_Desc", "Freq", "Total")
#Get error bars for the proportions 
epic_models$prop <- epic_models$Freq/epic_models$Total
epic_models$se <- sqrt(epic_models$prop*(1-epic_models$prop)/epic_models$Total)
epic_models.e <- expand.grid(epic_models$age_bins, epic_models$developed_AML, epic_models$Prediction_Desc)
epic_models.e <- unique(epic_models.e)
colnames(epic_models.e) <- c("age_bins", "developed_AML", "Prediction_Desc")
epic_models<- merge(epic_models, epic_models.e, by = c("age_bins","developed_AML", "Prediction_Desc"), all =T)
#Age distributions
fig2b <-  ggplot()+theme_pubr()+scale_fill_brewer(name = "Age Group", palette="Blues")+
          geom_bar(data=epic_models, aes(x=Prediction_Desc ,y=prop, fill= factor(age_bins)),colour = "black", stat="identity", position = "dodge")+facet_grid(~developed_AML)+
          geom_errorbar(data = epic_models, aes(x = Prediction_Desc, ymin = prop-se, ymax = prop + se, group = factor(age_bins)), position = "dodge", width=0.91)+
          ylab("Proportion of Participants")+xlab("Evolutionary Class")+ggtitle("")+theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12, family = "Helvetica"))+ theme(axis.text.x = element_text(angle = 30, hjust = 1))+theme(plot.title = element_text(face = "bold"))
fig2b

#Proportion of individuals rejecting neutrality 
prop.neut <- class.m.se
prop.neut <- plyr::count(prop.neut, c("developed_AML", "Desc"))
prop.neut$Desc <- ifelse(prop.neut$Desc=="Neutral", "Neutral", "Other")
res <- prop.test(x = c(246, 73), n = c(385, 92))
res 

####Figure 2c: Range of mutation rate estimations across cohort of participants
ave.mutrates = aggregate(value~Sample, mut.rates, mean)
ave.mutrates$variable = "Mean"
ave.mutrates = ave.mutrates[,c(1,3,2)]
mut.rates.ave = rbind(mut.rates, ave.mutrates)
mut.rates.ave$grp = ifelse(mut.rates.ave$variable=="Mean", "Mean Estimate", "Single Estimate")
fig2c =   ggplot(mut.rates.ave, aes(reorder(Sample, -value), value, group = variable, colour = grp))+geom_line()+scale_color_manual(name = "", values = c("red", "lightgrey"))+theme_pubr()+
          theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14, family = "Helvetica Light"), legend.position = "top")+ggtitle("")+
          theme(plot.title = element_text(face = "bold"))+xlab("Sample")+ylab("Predicted Mutation Rate (log10)")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
          theme(axis.text.x=element_blank())+  theme(plot.title = element_text(hjust = 0), text = element_text(size = 14, family = "Helvetica"))
fig2c

####Figure 2d: Preleukemic blood populations have a higher mutation rate than healthy controls. 
mr = join(mut.rates, class.m.se)
serr <- function(x) sd(x)/sqrt(length(x))
mut.rates.10k =  mr %>% dplyr::group_by(Sample,age_bins, developed_AML) %>% dplyr::summarise(avg = mean(value), se = serr(value))
wilcox.test(avg~developed_AML, mut.rates.10k)
my_comparisons <- list(c("Control", "PreLeukemia"))
fig2d =   ggplot(data = mut.rates.10k, aes(developed_AML,  avg, fill = developed_AML))+geom_boxplot()+theme_pubclean()+scale_fill_manual(name = "Group", values = c("darkblue", "red"))+
          stat_compare_means(comparisons = my_comparisons, label = "p.signif",method = "wilcox.test", hjust = -3)+ 
          theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12, family = "Helvetica"), legend.position = "top")+ggtitle("")+
          theme(plot.title = element_text(face = "bold"))+xlab("")+ylab("Mutation Rate (log10)")+
          theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14, family = "Helvetica"))
fig2d

#Supplementary Figure 9. Mutation rate predictions across ensemble of DNNs. 
mr.e = expand.grid(unique(mut.rates.10k$Sample), unique(mut.rates.10k$age_bins))
colnames(mr.e) = c("Sample", "age_bins")
mr.ext = merge(mut.rates.10k,mr.e, all =T)
supp8 <-  ggplot(data = mr, aes(reorder(Sample, value), variable, fill = value))+geom_raster(interpolate = T)+
          scale_fill_gradient2(name = "Mutation Rate \n(log10)", low = "black", mid = "darkblue", high= "red", midpoint = -11)+theme_pubr()+
          theme(axis.text.y = element_blank(),axis.text.x=element_blank())+  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14, family = "Helvetica"), legend.position = "right")+ggtitle("")+
          theme(plot.title = element_text(face = "bold"))+xlab("Sample")+ylab("Ensemble Prediction")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
          theme(axis.text.x=element_blank())+  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14, family = "Helvetica"))
supp8

#Supplementary Figure 9. Impact of population size on mutation rate. 
mut.rates.all$N = as.factor(mut.rates.all$N)
supp9 =   ggplot(mut.rates.all, aes(x=reorder(Sample, -avg), y=avg, colour = N)) + scale_color_manual(name= "Population Size (N)", values = c("purple", "blue", "darkblue", "black"))+
          theme_pubr()+geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd, group = N), width=2, colour = "grey")+ geom_line(aes(group = N), size = 1)+ 
          theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12, family = "Helvetica Light"), legend.position = "right")+ggtitle("")+
          theme(plot.title = element_text(face = "bold"))+xlab("Sample")+ylab("Predicted Mutation Rate (log10)")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+
          theme(axis.text.x=element_blank())+  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14, family = "Helvetica"))
supp9

#Supplementary Figure 7. Distribution of mutation rate predictions for simulated data. 
supp7 =   ggplot(data = mutPreds_Sims,aes(Pred))+theme_pubr()+geom_density(fill = "lightblue", colour = "darkblue")+facet_wrap(~True)+
          geom_vline(data = mutPreds_Sims, aes(xintercept = True), colour ="red", linetype = "dashed")+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12, family = "Helvetica "), legend.position = "right")+ggtitle("")+
          theme(plot.title = element_text(face = "bold"))+xlab("Predicted Mutation Rate")+ylab("")+theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 14, family = "Helvetica"))
supp7

#Supplementary Figure 6. Mutational burden across evolutionary classes.
class.m.se$nbPassenger <- as.numeric(as.character(class.m.se$numSNPs))-as.numeric(as.character(class.m.se$nbBen))
meansSNPs <- class.m.se[,c(1,23,25)]
meansSNPs$nbBen = as.numeric(meansSNPs$nbBen)
meansSNPs$nbPassenger = as.numeric(meansSNPs$nbPassenger)
meansSNPs.m <- reshape2::melt(meansSNPs, id.vars="Sample")
fits <- mutBurden[,c(1,5,6)]
meansSNPs.m <- join(meansSNPs.m, fits)
wilcox.test(nbBen ~ developed_AML, data = class.m.se,exact = FALSE)
meansSNPs.m  <- meansSNPs.m %>% dplyr::group_by(variable, Desc, developed_AML) %>% dplyr::summarise(avg = mean(value), sd = serr(value))
meansSNPs.m$variable <- ifelse(meansSNPs.m $variable=="nbBen", "Drivers", "Passengers")
supp6 <-  ggplot(meansSNPs.m, aes(x=Desc, y=avg, group=developed_AML)) + theme_pubr()+scale_colour_manual(values = c("Blue", "Red"))+
          geom_errorbar(aes(ymin=avg-sd, ymax=avg+sd), width=.3, position=position_dodge(0.5), colour = "black")+
          geom_point(data = meansSNPs.m, aes(colour = developed_AML), size = 3, position = position_dodge((0.5)))+
          xlab("Evolutionary Class")+ylab("Mean Mutation Count")+facet_wrap(~variable)+theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16, family = "Helvetica"))+ theme(axis.text.x = element_text(angle = 30, hjust = 1))+
          theme(legend.title=element_blank())+ggtitle("Mutational Burden across Evolutionary Classes")+theme(plot.title = element_text(face = "bold"))
supp6 

#comparisons for p values that are shown in the paper 
epicCohort.neut = epicCohort[epicCohort$Desc=="Neutral",]
wilcox.test(CADD ~ Driver, data = epicCohort.neut, exact = FALSE)
epicCohort.pos = epicCohort[epicCohort$Desc=="Positive",]
wilcox.test(CADD ~ Driver, data = epicCohort.pos, exact = FALSE)
epicCohort.comb = epicCohort[epicCohort$Desc=="Combination",]
wilcox.test(CADD ~ Driver, data = epicCohort.comb, exact = FALSE)
epicCohort.negPass = epicCohort[epicCohort$Desc=="Negative" | epicCohort$Desc=="Neutral",]
epicCohort.negPass = epicCohort.negPass[epicCohort.negPass$Driver=="Passenger",]
wilcox.test(CADD ~ Desc, data = epicCohort.negPass, exact = FALSE)

#Figure 3A: Predicted functionality of mutations in each evolutionary class. 
means.CADD.dp <- aggregate(CADD ~ Desc+Driver, data = epicCohort, FUN = function(x) c(mean = mean(x), se = serr(x)))
means.CADD.dp <- cbind(means.CADD.dp, means.CADD.dp$CADD)
colnames(means.CADD.dp) <- c("Desc", "Type", "CADD", "mean", "se")
means.CADD.dp$Type = ifelse(means.CADD.dp$Type=="Driver", "AML Driver", "Non-Driver")
means.CADD.dp$Desc <- factor(means.CADD.dp$Desc, levels=c("Neutral", "Negative", "Combination", "Positive"))
fig3a = ggplot(means.CADD.dp, aes(x = Desc, y = mean, colour = Type))+theme_pubr()+scale_colour_manual(values = c("darkgreen", "Red", "Blue"))+
        geom_errorbar(aes(ymin=mean-se, ymax=mean+se,fill=Type), width=.2, position = position_dodge(.4), colour = "black")+
        geom_point(size = 3, position = position_dodge(.4))+theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 16, family = "Helvetica"))+
        ggtitle("")+theme(plot.title = element_text(face = "bold"))+xlab("")+ylab("CADD Score")+ylim(12,28)+ theme(axis.text.x = element_text(angle = 30, hjust = 1))+
        theme(plot.title = element_text(face = "bold"))
fig3a

#Figure 3b: Distribution of function-altering mutations in genes across evolutionary classes. 
epicCohort.highCadd = epicCohort[epicCohort$CADD>10,]
epicCohort.highCadd = unique(epicCohort.highCadd[,c(3,5)])
epicCohort.counts = plyr::count(epicCohort.highCadd, c("GeneName", "Desc"))
epicCohort.counts = epicCohort.counts[!is.na(epicCohort.counts$GeneName),]
input = reshape(epicCohort.counts, idvar = "GeneName", timevar = "Desc",direction = "wide")
colnames(input) = c("Gene", "Neutral", "Combination", "Negative", "Positive")
input[is.na(input)] <- 0
fig3b <-  upset(input, matrix.color = "black",main.bar.color = "black", point.size = 3.5, line.size = 1, empty.intersections = "on", order.by = "freq",
          mainbar.y.label = "Number of Genes", sets.bar.color = c("orange", "lightblue", "darkred", "darkgreen"), sets.x.label = "Genes per Class", 
          text.scale = c(2, 2, 1.5, 2, 1.5, 2))
fig3b

#Figure 2e: Relative passenger to driver mutation proportion across evolutionary classes. 
dpMutationComparison = class.m.se[class.m.se$Desc!="Neutral" & class.m.se$Desc!="Negative",c(1,6,5,23,25)]
fig2e <-  ggplot(data = dpMutationComparison, aes(x=(nbBen), y = (nbPassenger), colour = developed_AML))+geom_point()+
          geom_smooth(method="lm")+theme_pubr()+scale_color_manual(name = "Group", values = c("blue", "red"))+
          theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12, family = "Helvetica"))+ggtitle("")+
          theme(plot.title = element_text(face = "bold"))+xlab("Mutations in AML Drivers")+ylab("Mutations in non driver genes")+facet_wrap(~Desc,1)
fig2e

#Regression Equations 
regression=function(df){
  reg_fun<-lm(formula=dpMutationComparison$nbPassenger~dpMutationComparison$nbBen) 
  slope<-round(coef(reg_fun)[2],3)  
  intercept<-round(coef(reg_fun)[1],3) 
  R2<-round(as.numeric(summary(reg_fun)[8]),3)
  R2.Adj<-round(as.numeric(summary(reg_fun)[9]),3)
  c(slope,intercept,R2,R2.Adj)
}

#Compare slopes (Combination)
dpMutationComparison.comb = dpMutationComparison[dpMutationComparison$Desc!="Positive",]
int <- lm(nbPassenger ~ nbBen*developed_AML, data = dpMutationComparison.comb )
anova(int)
int$coefficients
slopes <- lstrends(int, "developed_AML", var="nbBen")
pairs(slopes)

#Compare slopes (Positive)
dpMutationComparison.pos = dpMutationComparison[dpMutationComparison$Desc!="Combination",]
int <- lm(nbPassenger ~ nbBen*developed_AML, data = dpMutationComparison.pos)
anova(int)
int$coefficients
slopes <- lstrends(int, "developed_AML", var="nbBen")
slopes
pairs(slopes)

#Figure 3C: Inferred pathogenicity of dominant clone is correlated with the variant allele frequency across different evolutionary classes. 
dominantVAF = aggregate(Collapsed.Freq~Sample+Desc, epicCohort, max)
dominantVAF = plyr::join(dominantVAF, epicCohort)
dominantVAF$cadd_bins <- cut(dominantVAF$CADD, breaks=c(0,10,20,30,40,50, 60), labels=c("0-10","10-20","20-30","30-40", "40-50", "50-60"))
dominantVAF$Desc = factor(dominantVAF$Desc, levels = c("Neutral", "Negative", "Combination", "Positive"))
dominantVAF.e <- expand.grid(unique(dominantVAF$Desc), unique(dominantVAF$cadd_bins))
colnames(dominantVAF.e) = c("Desc", "cadd_bins")
dominantVAF = merge(dominantVAF, dominantVAF.e, all = T)
dominantVAF$Collapsed.Freq[is.na(dominantVAF$Collapsed.Freq)]=0
fig3c = ggplot(na.omit(dominantVAF), aes(cadd_bins, Collapsed.Freq, fill=interaction(Desc), dodge=cadd_bins)) +stat_boxplot(geom ='errorbar')+
        geom_boxplot()+theme_pubr()+ scale_fill_manual(name = "", values=c("lightblue","red",  "orange",  "darkgreen"))+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"), text = element_text(size = 13, family = "Helvetica"))+
        theme(strip.text = element_text(colour = 'black', face = "bold"))+xlab("Inferred Pathogenicity of Dominant Clone")+ 
        ylab("Variant Allele Frequency")+ggtitle("")+facet_wrap(~Desc,2)+theme(axis.text.x = element_text(angle = 30, hjust = 1))
fig3c

#Figure 3D: Impact of Negative Selection on Clonal Expansions. 
epicCohort$vaf.bins <- cut(epicCohort$Collapsed.Freq, breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c( ">0.1", "0.2", "0.3", "0.4", ">0.5"))
to_compare = epicCohort[epicCohort$Desc=="Combination" | epicCohort$Desc=="Positive",]
to_compare$Grp = paste(to_compare$Desc, sep = "_", to_compare$developed_AML)
to_compare$Collapsed.Freq <- log10(to_compare$Collapsed.Freq)
to_compare = to_compare[complete.cases(to_compare),]
my_comparisons <- list(c("Combination_Control", "Positive_Control"),c("Combination_Control", "Combination_PreLeukemia"),
                       c("Combination_PreLeukemia", "Positive_PreLeukemia"), c("Positive_Control", "Positive_PreLeukemia"))
compare_means(Collapsed.Freq~Grp, to_compare, method = "wilcox.test")
fig3d =   ggboxplot(to_compare, x="Grp", y="Collapsed.Freq", fill = "Grp")+ theme_pubr()+scale_fill_manual(name = "Desc", values = c("darkorange", "#ffc966", "darkgreen", "#90ee90"))+
          stat_compare_means(comparisons=my_comparisons, method = "wilcox.test", aes(label=..p.signif.., font.label = "bold"), size = 4)+
          theme(plot.title = element_text(hjust = 0.5, face = "bold"), text = element_text(size = 12, family = "Helvetica"))+
          theme(strip.text = element_text(colour = 'black', face = "bold"))+xlab("")+ ylab("Variant Allele Frequency (log10)")+ggtitle("")+theme(axis.text.x = element_text(angle = 30, hjust = 1))+
          scale_x_discrete(labels= c("Combination (C)", "Combination (PL)", "Positive (C)", "Positive (PL)"))+theme(legend.position = "none") +ggtitle("")
fig3d

#p values for comparisons in the figure 
comb.compare = to_compare[to_compare$Desc =="Combination",]
wilcox.test(Collapsed.Freq~developed_AML, comb.compare)
pos.compare = to_compare[to_compare$Desc =="Positive",]
wilcox.test(Collapsed.Freq~developed_AML, pos.compare)
cont.compare = to_compare[to_compare$Desc =="Positive" | to_compare$Desc =="Combination",]
cont.compare = cont.compare[cont.compare$developed_AML =="Control" ,]
wilcox.test(Collapsed.Freq~Desc, cont.compare)

#Supplementary Figure 10. Distribution of mutations in known driver genes across evolutionary classes. 
genes <- plyr::count(epicCohort, c("Desc", "GeneName", "developed_AML", "Driver"))
genes.total <- plyr::count(epicCohort, c("GeneName", "developed_AML", "Driver"))
colnames(genes.total) <- c("GeneName", "developed_AML", "Driver", "Total")
genes.prop <- join(genes, genes.total)
genes.prop$prop <- genes.prop$freq/genes.prop$Total
genes.driver = genes.prop[genes.prop$Driver == "Driver",]
supp10 =  ggplot(data = genes.driver, aes(reorder(GeneName, -prop), prop, fill = Desc))+geom_bar(position = "stack", stat = "identity")+theme_pubr()+
          scale_fill_manual(name = "Class", values = c("orange", "lightblue", "darkgreen"))+theme(plot.title = element_text(hjust = 0.5, face = "bold"), text = element_text(size = 14, family = "Helvetica"))+
          theme(strip.text = element_text(colour = 'black', face = "bold"))+xlab("Gene")+ ylab("Proportion")+ggtitle("")+
          theme(axis.text.x = element_text(angle = 90))+facet_wrap(~developed_AML,2)
supp10

#Figure 4: Survival analyses 
km.epic = unique(epicCohort[,c(6,7,10,11,5)])
km.epic$fustat = ifelse(km.epic$developed_AML=="Control", 0, 1)
km.epic$Desc = relevel(km.epic$Desc, ref = "Neutral")
surv_object <- Surv(time = km.epic$followup_length_in_days, event = km.epic$fustat)
fit1 <- survfit(surv_object ~ Desc, data = km.epic)
fig4a =   ggsurvplot(fit1, data = km.epic, pval = F, palette = c("lightblue", "orange", "darkred", "darkgreen"), 
          legend.title="", font.legend = list(size = 12, color = "black"))+xlab("Time to Event (days)")+
          ylab("AML-free fraction")+guides(colour = guide_legend(nrow = 2))
fig4a
fit2 =    coxph(surv_object ~ Desc, data = km.epic)
summary(fit2)
fig4b =   forest_model(fit2)
figure4 = ggarrange(fig4a$plot,fig4b, nrow = 1, widths  = c(0.4,0.6))
figure4




