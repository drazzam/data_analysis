library(netmeta)
library(pcnetmeta)
library(rjags)
library("xlsx")
library("WriteXLS")
library("tableone")
library("BiocManager")
library("hasseDiagram")

install.packages("hrbrthemes")
BiocManager::install()
BiocManager::install("Rgraphviz")
install.packages("hasseDiagram")

install.packages("Rgraphviz", dependencies=TRUE,repos='http://cran.rstudio.com/')

#Making league as a figure directly
league0 <- netleague(net1, digits = 2, bracket = "(", separator = " to ", seq = netrank(net1, small.values = "bad"))
WriteXLS(league0$random, ExcelFileName = "league0-random.xls", SheetNames = "leaguetable (random)", col.names = FALSE)
# League table for two outcomes with
# - network estimates of first outcome in lower triangle
# - network estimates of second outcome in upper triangle
netleague(net1, net2, digits = 2, ci = T)

league0 <- netleague(net1, net2, seq = netrank(net1, small = "good"), ci = T, digits = 2, bracket = "(", separator = " to ")
WriteXLS(league0$random, ExcelFileName = "league0-random.xls", SheetNames = "leaguetable (random)", col.names = FALSE)

netleague(net1, net2, seq = netrank(net2, small = "bad"), ci = T)
print(netrank(net1, small = "bad"))
print(netrank(net2, small = "bad"))

#DF
data1 <- read.table(file.choose(), header=T, sep=";")
p1 <- pairwise(treatment, event = e, n = n, studlab = study, data = data1, sm = "OR")
net1 <- netmeta(p1, comb.random = TRUE)
forest(net1, ref = "O", drop = F, leftlabs = "Blood Group", smlab= "Compared to O Group 
       (Random-effects)", xlim = c(0.1,5), rightlabs="P-Score", rightcols=c("effect", "ci", "Pscore"), sortvar=Pscore)
netgraph(net1, plastic = FALSE, number.of.studies=T, thickness = "number.of.studies")
netheat(net1, random=TRUE)

#Table of net split
print(netsplit(net1), digits = 2, ci = F, test = T)

netleague(net1, comb.random = TRUE, digits = 2, seq = netrank(net1, small.values = "good"))
print(netrank(net1), comb.random = TRUE)
forest(netsplit(net1), digits = 2)

# 'Comparison-adjusted' funnel plot
ord <- c("A", "B", "AB", "O")
f1 <- funnel(net1, order = ord,
       pch = rep(c(15:18, 3, 4), 1), col = 1:6, pooled="random", sep.trts=" vs ", legend = F)


f2 <- funnel(net1, order = ord,
             pch = rep(c(15:18, 3, 4), 1), col = 1:6,
             linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 3)

#??? ??? ????? ????? ??????? ???? ?????? ?? ??? ???????
#studlab= T       ???? ????? ????????
##############################################

#DHF
data2 <- read.table(file.choose(), header=T, sep=";")
p2 <- pairwise(treatment, event = e, n = n, studlab = study, data = data2, sm = "OR")
net2 <- netmeta(p2, comb.random = TRUE)
forest(net2, ref = "O", drop = F, leftlabs = "Blood Group", smlab= "Compared to O Group 
       (Random-effects)", xlim = c(0.1,5), rightlabs="P-Score", rightcols=c("effect", "ci", "Pscore"), sortvar=Pscore)
netgraph(net2, plastic = FALSE, number.of.studies=T, thickness = "number.of.studies")
netheat(net2, random=TRUE)

#Table of net split
print(netsplit(net2), digits = 2, ci = F, test = T)

netleague(net2, comb.random = TRUE, digits = 2, seq = netrank(net1, small.values = "good"))
print(netrank(net2), comb.random = TRUE)
forest(netsplit(net2), digits = 2)

# 'Comparison-adjusted' funnel plot
ord <- c("A", "B", "AB", "O")
f3 <- funnel(net2, order = ord,
             pch = rep(c(15:18, 3, 4), 1), col = 1:6, pooled="random", sep.trts=" vs ", legend = F)


f4 <- funnel(net2, order = ord,
             pch = rep(c(15:18, 3, 4), 1), col = 1:6,
             linreg = TRUE, rank = TRUE, mm = TRUE, digits.pval = 3)

#??? ??? ????? ????? ??????? ???? ?????? ?? ??? ???????
#studlab= T       ???? ????? ????????
##############################################
#Net Poset

# Outcome labels
outcomes <- c("DF", "DHF")

# Partial order of treatment rankings 
po <- netposet(netrank(net1, small.values = "bad"), netrank(net2, small.values = "bad"), outcomes = outcomes)

# Scatter plot
plot(po)

# New Figure 3
# Libraries
library(ggplot2)
library(dplyr)

# Plot

B <- ggplot(data=data1, aes(x=Dengue.Fever, y=Dengue.Haemorrhagic.Fever, ymin = 0, ymax = 1, xmin=-0.1, xmax=1)) + ylab("Dengue Haemorrhagic Fever") + xlab ("Dengue Fever") + geom_point(data = data1, aes(x=Dengue.Fever, y=Dengue.Haemorrhagic.Fever, color = Group),size=8) + geom_line(color="black", size= 2) + geom_text_repel(data=data1, label = data1$Group, size= 9, vjust = 1, hjust= 1)
B1 <- B+theme(axis.text=element_text(size=12, face="bold", colour = "black"),
       axis.title=element_text(size=16,face="bold")) + theme(legend.position = "none")
B1
# Hasse diagram
hasse(po)

#problem in margins
par(mar = rep(2, 4))
par(mfrow=c(4,2))

#########################################

#Binary analysis
library(meta)

data3 <- read.table(file.choose(), header=T, sep=";")

m <- metabin(Ee, Ne, Ec, Nc, sm="OR", method="I", studlab=study, data=data3, byvar = C, print.byvar = F)

forest(m, text.subgroup.nohet=F, pooled.events=T, colgap.forest.left="1cm", resid.hetstat=F, colgap.forest.right="1cm", lab.e="Rh Positive", colgap="0.5cm", lab.c="Rh Negative", test.overall.random=TRUE, comb.fixed=F, digits.zval=3, digits.pval=3, digits.Q=4, digits.pval.Q=3, digits.tau2=3, label.left ="Higher risk for Rh-ve", label.right="Higher risk for Rh+ve",ff.lr = "bold", just = "center")

#Egger's Test
metabias(m)

#Funnel plot + Trim n' Fill
funnel(m)
tf1 <- trimfill(m)
summary(tf1)
funnel(tf1)
funnel(tf1, pch=ifelse(tf1$trimfill, 1, 16), level=0.9, comb.random=FALSE)

#leave-out 1 study
forest(metainf(m, pooled="random"), layout = "Revman5", col.square.lines="blue", col.inside="black", col.square = "blue", xlim = c(0,100), pscale = 100)

#Bujat Plot
baujat(m, studlab=data1$study)  



#Make Persongraph
library(personograph)

# 2- meta returns random effects estimate on the log scale
point <- exp(m$TE.random)
# Calculated Random Effects RR, using the meta package
point 

# Approximate the Control Event Rates using a weighted median
cer <- w.approx.cer(data3[["Ec"]], data3[["Nc"]])

# Calculate the Intervention Event Rates (IER) from the CER and point estimate
ier <- calc.ier(cer, point, "OR")
# Calcaulte the "uplift" statistics
# Note that this depends on the direction of the outcome effect (higher_is_better)
u <- uplift(ier, cer, higher_is_better=F)
plot(u, n.icons = 1000, icon.style=4, dimensions=c(10,20))   
