# Upload the list in which many duplicities are present: 

Dupp <- read.csv2("Duplicities_maybe.csv",T)

# 1) Make sure the columns are plain character
Dupp$Title_Tor    <- as.character(Dupp$Title_Tor)
Dupp$Title_Chrome <- as.character(Dupp$Title_Chrome)

# 2) Normalise/fix encoding to UTF-8 and drop invalid bytes
Dupp$Title_Tor    <- iconv(Dupp$Title_Tor,    from = "", to = "UTF-8", sub = "")
Dupp$Title_Chrome <- iconv(Dupp$Title_Chrome, from = "", to = "UTF-8", sub = "")

# 3) Remove stray NUL characters if present (can trigger that error)
Dupp$Title_Tor    <- gsub("\\x00", "", Dupp$Title_Tor,    useBytes = TRUE)
Dupp$Title_Chrome <- gsub("\\x00", "", Dupp$Title_Chrome, useBytes = TRUE)

# --- Choose ONE of the following cleaning variants ---

# A) Keep ASCII letters/digits + basic punctuation (strip diacritics by transliteration)
Tor_ascii    <- iconv(Dupp$Title_Tor,    from = "UTF-8", to = "ASCII//TRANSLIT")
Chrome_ascii <- iconv(Dupp$Title_Chrome, from = "UTF-8", to = "ASCII//TRANSLIT")

allowed_pattern <- "[^A-Za-z0-9#?!.,:;_\\-\\(\\)\\[\\] ]"
Tor_clean    <- gsub(allowed_pattern, "", Tor_ascii,    perl = TRUE)
Chrome_clean <- gsub(allowed_pattern, "", Chrome_ascii, perl = TRUE)


# Check if there is any overlap between the two columns
any_overlap <- any(Dupp$Title_Tor %in% Dupp$Title_Chrome)

# If you also want to see which values overlap:
overlapping_values <- intersect(Dupp$Title_Tor, Dupp$Title_Chrome)

any_overlap
overlapping_values

Dupp$Duppl_Somewhere <- ifelse(Dupp$Title_Chrome %in% overlapping_values,"YES","NO") 

Dupp$Search <- c(rep("ArchDenmark",40),rep("Alcatraz",40),rep("BarkBeetle",40),rep("BurgerKing",40),
                 rep("KingValley",40),rep("Aspirin",40),rep("ScottishFlag",40),
                 rep("Pizzagate",40),rep("TajMahal",40),rep("ThreeGorges",40)
)

tapply(as.factor(Dupp$Duppl_Somewhere),Dupp$Search,summary)

sum(17,22,10,8,12,12,11,7,14,11)

table(Dupp$Duppl_Somewhere)

rm(list=ls())



# Upload script from the "Relevant" vs "Irrelevant" tittle questionnaire and process into a think: 
RelIr <- read.csv("Relevant_Irrelevant_5.csv",T)

# Add NAs to every empty space: 
RelIr[RelIr == ""] <- NA

Procedural <- RelIr[RelIr$Task_Name=="The_Task_Procedural",c(1,3,6,41:60)]
  
Conceptual <- RelIr[RelIr$Task_Name=="The_Task_Conceptual",c(1,3,6,21:40)]

# Split into !TRIAL IDs! 
# 1,3,5,7,9,11,13,15,17,19 AND 2,4,6,8,10,12,14,16,18,20... plus reorder since the order is a mess
# Then rerank the columns back to 1,2,3,4...
#   1+2 and 3+4... correspond to the same searc keyword, but different Titles 
# Then calculate, how many times each title was considered relevant or irrelevant 
Procedural <- Procedural[,c(1,2,3,4,15,17:23,5:14,16)]
Conceptual <- Conceptual[,c(1,2,3,4,15,17:23,5:14,16)]

Procedural <- Procedural[order(Procedural$Trial_Id),]
Conceptual <- Conceptual[order(Conceptual$Trial_Id),]

table(as.numeric(as.factor(Procedural$Row_Proced_1)),Procedural$Row_Proced_1)
# False is one, true is two, better let's turn columns 4 to 23 into a series of 0 (F-s) and 1 (T-s)
Procedural[, 4:23] <- lapply(Procedural[, 4:23], function(x)
  ifelse(tolower(as.character(x)) == "true", 1, 0)
)

Conceptual[, 4:23] <- lapply(Conceptual[, 4:23], function(x)
  ifelse(tolower(as.character(x)) == "true", 1, 0)
)

Proced_TF <- as.data.frame(aggregate(Procedural[, 4:23], by = list(TrialID = Procedural$Trial_Id), FUN = mean, na.rm = TRUE))
Concept_TF <- as.data.frame(aggregate(Conceptual[, 4:23], by = list(TrialID = Conceptual$Trial_Id), FUN = mean, na.rm = TRUE))


# Upload the list of the titles N = 400, align with their position (row correspondence)
Titles <- read.csv2("Upload_to_R.csv",T)

# turn into 20×20 and transform: 
Titles_proc <- as.vector(Titles[401:800,2])
Titles_conc <- as.vector(Titles[1:400,2])

Titles_proc <- matrix(Titles_proc, nrow = 20, ncol = 20, byrow=T)
Titles_proc[4,]

Titles_conc <- matrix(Titles_conc, nrow = 20, ncol = 20, byrow=T)
Titles_conc[4,]

# Some trials (as it is probabilistics) were dropped (not run), so we must drop them here, too: 
unique(Conceptual$Trial_Id); unique(Procedural$Trial_Id)

Titles_conc<-Titles_conc[unique(Conceptual$Trial_Id),]
Titles_proc<-Titles_proc[unique(Procedural$Trial_Id),]


# and voilá
c(as.matrix(Proced_TF[,2:21]),
as.matrix(Titles_proc))

Proced_TF2 <- matrix(ncol=20,nrow=40)

Proced_TF2[seq(1, 40, by = 2), ] <- as.matrix(Proced_TF[,2:21]) # Change 36 into appropriate number, depending on how many of the 20×2 of the 1:20 sequence is actually consumed
Proced_TF2[seq(2, 40, by = 2), ] <- as.matrix(Titles_proc)


Concept_TF2 <- matrix(ncol=20,nrow=40)

Concept_TF2[seq(1, 40, by = 2), ] <- as.matrix(Concept_TF[,2:21])
Concept_TF2[seq(2, 40, by = 2), ] <- as.matrix(Titles_conc)


# Turn it into a data frame: a vector of labels and a vector of "relevant/irrelevant ratio"
Concept_TF2_df <- data.frame(
  Label   = as.vector(t(Concept_TF2[seq(2, nrow(Concept_TF2), 2), ])),
  RelIrel = as.vector(t(Concept_TF2[seq(1, nrow(Concept_TF2), 2), ]))
)

Proced_TF2_df <- data.frame(
  Label   = as.vector(t(Proced_TF2[seq(2, nrow(Proced_TF2), 2), ])),
  RelIrel = as.vector(t(Proced_TF2[seq(1, nrow(Proced_TF2), 2), ]))
)


# All good, seems to be in the same (correct) order; so, put together in the 
# Conceptual -> Procedural order
# Then upload the table "Coded_Content" and merge and play...

TF2_df <- rbind.data.frame(Concept_TF2_df,Proced_TF2_df)

CodCont <- read.csv2("Coded_Content.csv")

d <- cbind.data.frame(TF2_df,CodCont)

d$RelIrel <- as.numeric(d$RelIrel)
d$Low.Quality <- d$Low.Quality*(-1)

cor(d[1:800,c(2,6:14)])
cor(d[1:400,c(2,6:14)])
cor(d[401:800,c(2,6:14)])

cor(as.numeric(d$RelIrel),as.numeric(d$AI.Gen))

save(d,file="RelIrel_ratio.Rdata")

# Specify the objects you want to keep
objects_to_keep <- c("d")  

# Remove all other objects
rm(list = setdiff(ls(), objects_to_keep))



####
# Alternative way - long table
####

# Upload script from the "Relevant" vs "Irrelevant" tittle questionnaire and process into a think: 
RelIr <- read.csv("Relevant_Irrelevant_5.csv",T)

# Add NAs to every empty space: 
RelIr[RelIr == ""] <- NA

Procedural <- RelIr[RelIr$Task_Name=="The_Task_Procedural",c(1,3,6,41:60)]

Conceptual <- RelIr[RelIr$Task_Name=="The_Task_Conceptual",c(1,3,6,21:40)]


# Split into !TRIAL IDs! 
# 1,3,5,7,9,11,13,15,17,19 AND 2,4,6,8,10,12,14,16,18,20... plus reorder since the order is a mess
# Then rerank the columns back to 1,2,3,4...
#   1+2 and 3+4... correspond to the same searc keyword, but different Titles 
# Then calculate, how many times each title was considered relevant or irrelevant 
Procedural <- Procedural[,c(1,2,3,4,15,17:23,5:14,16)]
Conceptual <- Conceptual[,c(1,2,3,4,15,17:23,5:14,16)]

Procedural <- Procedural[order(Procedural$Trial_Id),]
Conceptual <- Conceptual[order(Conceptual$Trial_Id),]

table(as.numeric(as.factor(Procedural$Row_Proced_1)),Procedural$Row_Proced_1)
# False is one, true is two, better let's turn columns 4 to 23 into a series of 0 (F-s) and 1 (T-s)
Procedural[, 4:23] <- lapply(Procedural[, 4:23], function(x)
  ifelse(tolower(as.character(x)) == "true", 1, 0)
)

Conceptual[, 4:23] <- lapply(Conceptual[, 4:23], function(x)
  ifelse(tolower(as.character(x)) == "true", 1, 0)
)

trial_Id <- rep(Conceptual$Trial_Id, each=20)
Rec_Session_Id <- rep(Conceptual$Rec_Session_Id, each=20)
Row_Concept <- rep(paste0("Row_Concept_",seq(1:20)),nrow(Conceptual))
relevant_not <- as.vector(t(Conceptual[, 4:23]))

long_conceptual <- data.frame(Trial_Id = trial_Id,
                              Rec_Session_Id = Rec_Session_Id,
                              Row_PrcCon = Row_Concept,
                              relevant = relevant_not)

table(long_conceptual$Rec_Session_Id) # Same number for each ID 

# This is no more relevant... but keep it here in case we needed that later
# Drop the participant with ID 1448743, otherwise the repetition-based thing won't work: 
# long_conceptual <- long_conceptual[long_conceptual$Rec_Session_Id!="1448743",]

trial_Id <- rep(Procedural$Trial_Id, each=20)
Rec_Session_Id <- rep(Procedural$Rec_Session_Id, each=20)
Row_Proced <- rep(paste0("Row_Proced_",seq(1:20)),nrow(Procedural))
relevant_not <- as.vector(t(Procedural[, 4:23]))

long_procedural <- data.frame(Trial_Id = trial_Id,
                              Rec_Session_Id = Rec_Session_Id,
                              Row_PrcCon = Row_Proced,
                              relevant = relevant_not)

table(long_procedural$Rec_Session_Id)


TF2_df_long <- rbind.data.frame(long_conceptual,long_procedural)

# Proced_concept needs to be distinguished, too: 
TF2_df_long$PrCon <- substr(TF2_df_long$Row_PrcCon,1,11)
TF2_df_long$PrCon2 <- paste0(TF2_df_long$PrCon,"_",TF2_df_long$Trial_Id) 

table(TF2_df_long$PrCon2)

CodCont <- read.csv2("Coded_Content.csv") 

# The order is correct, just every series of rows corresponding to the same item must be 
# repeated the correct number of times...
# For example: Trial_Id "1" goes for 20 rows for procedural and 20 rows for conceptual 
# Row_conceptual_1 to Row_conceptual_20 -> Danish Architecture, statements from trial 1 (in the order
# from labvanced).
# Trial_Id "2" - dtto -> Row_conceptual_1 to 20, Danish Architectur, statements 1:20 
# Repeat the corresponding number of times

# Add categories - what's the "odd and even trial": 
CodCont$Keyword <- paste0(CodCont$Keyword,c(rep("_a",20),rep("_b",20)))

reppp <- as.vector(table(TF2_df_long$PrCon2))

kejwort <- unique((CodCont$Keyword))

# assuming CodCont is already ordered: first 20 rows = kejwort[1], next 20 = kejwort[2], etc.
block_size <- 20
n_blocks <- nrow(CodCont) / block_size

# sanity check
stopifnot(n_blocks == length(reppp))

# how many times to repeat each 20-row block
reppp2 <- reppp / block_size

# indices of the 20-row blocks
block_indices <- rep(seq_len(n_blocks), times = reppp2)

# expand to full row indices
row_indices <- unlist(lapply(block_indices, function(i) ((i - 1) * block_size + 1):(i * block_size)))

# expanded data frame
CodCont_exp <- CodCont[row_indices, ]

table(CodCont_exp$Keyword)

d1 <- cbind.data.frame(TF2_df_long,CodCont_exp)


# Alternative way shown below to borrow trust: Take codcont, add a series of keywords that correspond to 
# TF2_df_long. This solution is also much simpler, even though it messess up the original order... Sorry. 
TF2_df_long$Keyword <- rep(kejwort,times=(reppp))
TF2_df_long$Keyword <- paste0(TF2_df_long$Keyword, seq(1:20)) 
TF2_df_long$horder <- seq(1:nrow(TF2_df_long))

CodCont$Keyword <- paste0(CodCont$Keyword,seq(1:20))

d2 <- merge(TF2_df_long, CodCont, by = "Keyword", all.x = TRUE)

d2 <- d2[order(d2$horder), ]

cor(as.numeric(d1$Rec_Session_Id),as.numeric(d2$Rec_Session_Id)) # heh...

####


# Specify the objects you want to keep
objects_to_keep <- c("d","d1","d2")  

# Remove all other objects
rm(list = setdiff(ls(), objects_to_keep))

# 

table(d2$PrCon)
table(d2$Trial_Id)

d2$Category <- paste0(d2$Trial_Id,"_",d2$PrCon)
table(d2$Category)

# A function to turn 0 and 1 into -0.5 and 0.5:
z01 <- function(x) as.numeric(x)              # ensure 0/1
zpm <- function(x) (z01(x) - 0.5)             # -> -0.5/+0.5


# for each word ID, which topic ID does it belong to?
# this vector must have length = number of unique words
# word_index  <- as.numeric(as.factor(d2$Keyword))
# topic_index <- as.numeric(as.factor(d2$Category))

# for each unique word, get its unique topic
# w_topic <- sapply(split(topic_index, word_index), function(x) unique(x)[1])
# w_topic <- as.numeric(w_topic)


dat <- list(
  # sorting variables
  part = as.numeric(as.factor(d2$Rec_Session_Id)), # ID of the respondent
  word = as.numeric(as.factor(d2$Keyword)), # ID of the word (i.e., to which statement the row refer)
  con_proc = as.integer(ifelse(d2$PrCon=="Row_Concept",1,2)), # Procedural vs Concepual
  Topic = as.numeric(as.factor(d2$Category)), # Danish_Architecture_1, _2, 300dpi_1, _2... 
  # w_topic = w_topic,
  # relevant irrelevant 
  RelIrel = as.numeric(d2$relevant),
  # content coding
  Face = zpm(as.numeric(d2$Face)), 
  Gest = zpm(as.numeric(d2$Gesturing)), 
  AI_gen = zpm(as.numeric(d2$AI.Gen)),
  Text_in = zpm(as.numeric(d2$Text.In)),
  Composite = zpm(as.numeric(d2$Composite))
)

summary.data.frame(dat)

library(rethinking)

mod_ri_1 <- ulam(
  alist(
    RelIrel ~ dbinom(1,p),
    
    logit(p) <- a_bar + a_part[part] + a_topic[Topic] + a_cp[con_proc] + # A_part and A_word are varying intercepts per word and participant
      B_face*Face + B_gest*Gest + B_ai*AI_gen + B_text*Text_in + B_compost*Composite,
    
    # participant intercepts
    a_bar ~ normal(0,1),
    a_part[part] ~ normal(0, sigma_part),
    a_cp[con_proc] ~ normal(0, sigma_cp),
    
    # Topic is more important than word (phrase), actually, the varying intercept on the level of the word makes little sense 
    a_topic[Topic] ~ normal(0, sigma_topic),

    # slopes
    c(B_face, B_gest, B_ai, B_text, B_compost) ~ normal(0,1),
    
    # Sigmas where necessary
    sigma_part ~ exponential(1),
    sigma_topic ~ exponential(1),
    sigma_cp ~ exponential(1)
    
  ),data = dat, iter=2.5e3, chains = 4, cores = 4, log_lik = T
)

MP1 <- precis(mod_ri_1, depth=3, prob = 0.95)

post1 <- extract.samples(mod_ri_1)

save(mod_ri_1, file="Model_YouTube_relevant_irrelevant_1.Rdata")
saveRDS(post1, file="Posterior_YouTube_relevant_irrelevant_1.RDS")

write.csv(MP1, file="MP1.csv",T)


# ODDs ratio: 
OR_face <- exp(post1$B_face)
quantile(OR_face, c(.025,.5,.975))  

# Probability scale:
p_face1 <- inv_logit(post1$a_bar + 0.5*post1$B_face)   # Face = +0.5
p_face0 <- inv_logit(post1$a_bar - 0.5*post1$B_face)   # Face = -0.5
dP      <- p_face1 - p_face0

c(face1 = mean(p_face1), face0 = mean(p_face0), dP = mean(dP))
quantile(dP, c(.025,.5,.975))



# Estimate the effects separately for conceptual and procedural: 
mod_ri_2 <- ulam(
  alist(
    RelIrel ~ dbinom(1,p),
    
    logit(p) <- a_bar + a_part[part] + a_topic[Topic] + a_cp[con_proc] + # A_part and A_word are varying intercepts per word and participant
      B_face[con_proc]*Face + B_gest[con_proc]*Gest + B_ai[con_proc]*AI_gen + 
      B_text[con_proc]*Text_in + B_compost[con_proc]*Composite,
    
    # participant intercepts
    a_bar ~ normal(0,1),
    a_part[part] ~ normal(0, sigma_part),
    a_cp[con_proc] ~ normal(0, sigma_cp),
    
    # Topic is more important than word (phrase), actually, the varying intercept on the level of the word makes little sense 
    a_topic[Topic] ~ normal(0, sigma_topic),
    
    # slopes
    vector[2]:B_face ~ normal(0,1), 
    vector[2]:B_gest ~ normal(0,1), 
    vector[2]:B_ai ~ normal(0,1), 
    vector[2]:B_text ~ normal(0,1), 
    vector[2]:B_compost ~ normal(0,1),
    
    # Sigmas where necessary
    sigma_part ~ exponential(1),
    sigma_topic ~ exponential(1),
    sigma_cp ~ exponential(1)
    
  ),data = dat, iter=2.5e3, chains = 4, cores = 4, log_lik = T
)

compare(mod_ri_1,mod_ri_2)

MP2 <- precis(mod_ri_2, depth=3, prob = 0.95)

post2 <- extract.samples(mod_ri_2)

save(mod_ri_2, file="Model_YouTube_relevant_irrelevant_2.Rdata")
saveRDS(post2, file="Posterior_YouTube_relevant_irrelevant_2.RDS")

write.csv(MP2, file="MP2.csv",T)


# ODDs ratio: 
# Conceptual 
OR_face2C <- exp(post2$B_face[,1])
quantile(OR_face2C, c(.025,.5,.975))  

# Procedural
OR_face2P <- exp(post2$B_face[,2])
quantile(OR_face2P, c(.025,.5,.975))  

# Probability scale - conceptual:
p_face1 <- inv_logit(post2$a_bar + post2$a_cp[,1] + 0.5*post2$B_face[,1])   # Face = +0.5
p_face0 <- inv_logit(post2$a_bar + post2$a_cp[,1] - 0.5*post2$B_face[,1])   # Face = -0.5
dP      <- p_face1 - p_face0

c(face1 = mean(p_face1), face0 = mean(p_face0), dP = mean(dP))
quantile(dP, c(.025,.5,.975))

#        2.5%         50%       97.5% 
# -0.07426553 -0.02550107  0.01969657

# Probability scale - procedural
p_face1 <- inv_logit(post2$a_bar + post2$a_cp[,2] + 0.5*post2$B_face[,2])   # Face = +0.5
p_face0 <- inv_logit(post2$a_bar + post2$a_cp[,2] - 0.5*post2$B_face[,2])   # Face = -0.5
dP      <- p_face1 - p_face0

c(face1 = mean(p_face1), face0 = mean(p_face0), dP = mean(dP))
quantile(dP, c(.025,.5,.975))

#        2.5%         50%       97.5% 
# -0.10221909 -0.03978086  0.01294997 


# Estimate the effects separately for conceptual and procedural + fit it as correlated & pooled & decentred: 
mod_ri_3 <- ulam(
  alist(
    RelIrel ~ dbinom(1,p),
    
    logit(p) <- a_bar + a_part[part] + a_topic[Topic] + 
      BA_cp[con_proc,1] + 
      BA_cp[con_proc,2]*Face + 
      BA_cp[con_proc,3]*Gest + 
      BA_cp[con_proc,4]*AI_gen + 
      BA_cp[con_proc,5]*Text_in + 
      BA_cp[con_proc,6]*Composite,
    
    # participant intercepts
    a_bar ~ normal(0,1),
    a_part[part] ~ normal(0, sigma_part),

    # Topic is more important than word (phrase), actually, the varying intercept on the level of the word makes little sense 
    a_topic[Topic] ~ normal(0, sigma_topic),
    
    # slopes
    transpars> matrix[con_proc, 6]:BA_cp <- compose_noncentered(sigma_cp, L_Rho_cp, z_cp),
    matrix[6, con_proc]:z_cp ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[6]:sigma_cp ~ dexp(1),
    cholesky_factor_corr[6]:L_Rho_cp ~ lkj_corr_cholesky(2),
    
    # Sigmas where necessary
    sigma_part ~ exponential(1),
    sigma_topic ~ exponential(1),

    gq> matrix[6, 6]:Rho_cp <<- Chol_to_Corr(L_Rho_cp)

  ),data = dat, iter=2.5e3, chains = 4, cores = 4, log_lik = T
)

compare(mod_ri_1,mod_ri_2,mod_ri_3)

#            WAIC    SE dWAIC  dSE pWAIC weight
# mod_ri_1 5975.8 64.74   0.0   NA  47.3   0.54
# mod_ri_3 5976.2 64.60   0.4 2.85  48.5   0.43
# mod_ri_2 5981.5 64.92   5.8 3.45  52.2   0.03

MP3 <- precis(mod_ri_3, depth=3, prob = 0.95)

post3 <- extract.samples(mod_ri_3)

save(mod_ri_3, file="Model_YouTube_relevant_irrelevant_3.Rdata")
saveRDS(post3, file="Posterior_YouTube_relevant_irrelevant_3.RDS")

write.csv(MP3, file="MP3.csv",T)

# ODDs ratio: 
# Conceptual 
str(post3$BA_cp)

OR_face3C <- exp(post3$BA_cp[,1,2]) # Group 1 (conceptual), predictor 2 (face)
quantile(OR_face3C, c(.025,.5,.975))  

# Procedural
OR_face3P <- exp(post3$BA_cp[,1,2])
quantile(OR_face3P, c(.025,.5,.975))  

# Probability scale - conceptual:
p_face1 <- inv_logit(post3$a_bar + post3$BA_cp[,1,1] + 0.5*post3$BA_cp[,1,2])   # Face = +0.5
p_face0 <- inv_logit(post3$a_bar + post3$BA_cp[,1,1] - 0.5*post3$BA_cp[,1,2])   # Face = -0.5
dP      <- p_face1 - p_face0

c(face1 = mean(p_face1), face0 = mean(p_face0), dP = mean(dP))
quantile(dP, c(.025,.5,.975))


# Probability scale - procedural
p_face1 <- inv_logit(post3$a_bar + post3$BA_cp[,2,1] + 0.5*post3$BA_cp[,2,2])   # Face = +0.5
p_face0 <- inv_logit(post3$a_bar + post3$BA_cp[,2,1] - 0.5*post3$BA_cp[,2,2])   # Face = -0.5
dP      <- p_face1 - p_face0

c(face1 = mean(p_face1), face0 = mean(p_face0), dP = mean(dP))
quantile(dP, c(.025,.5,.975))
