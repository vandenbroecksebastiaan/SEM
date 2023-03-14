library(lavaan)
library(semPlot)

data_A <- read.csv("ICPSR_02760/DS0001/02760-0001-Data.tsv", sep="\t")
data_B <- read.csv("ICPSR_04652/DS0001/04652-0001-Data.tsv", sep="\t")
data_C <- read.csv("ICPSR_36346/DS0001/36346-0001-Data.tsv", sep="\t")

depression_indicators_A <- c("A1PA63", "A1PA64", "A1PA65", "A1PA66",
                             "A1PA67", "A1PA69")
depression_indicators_B <- c("B1PA63", "B1PA64", "B1PA65", "B1PA66",
                             "B1PA67", "B1PA68", "B1PA69")
depression_indicators_C <- c("C1PA63", "C1PA64", "C1PA65", "C1PA66",
                             "C1PA67", "C1PA68", "C1PA69")
anxiety_indicators_A <- c("A1PA85A", "A1PA85B", "A1PA85C", "A1PA85D",
                          "A1PA85E", "A1PA85F", "A1PA85G", "A1PA85H",
                          "A1PA85I", "A1PA85J", "A1PA86")
anxiety_indicators_B <- c("B1PA88A", "B1PA88B", "B1PA88C", "B1PA88D",
                          "B1PA88E", "B1PA88F", "B1PA88G", "B1PA88H",
                          "B1PA88I", "B1PA88J", "B1PA89")
anxiety_indicators_C <- c("C1PA88A", "C1PA88B", "C1PA88C", "C1PA88D",
                          "C1PA88E", "C1PA88F", "C1PA88G", "C1PA88H",
                          "C1PA88I", "C1PA88J", "C1PA89")
# stress_reactivity_indicators_A <- c("A1SE7K", "A1SE7W", "A1SE7X")
stress_reactivity_indicators_B <- c("B1SE7K", "B1SE7W", "B1SE7X")
stress_reactivity_indicators_C <- c("C1SE7K", "C1SE7W", "C1SE7X")

data_B <- data_B[, c(depression_indicators_B, anxiety_indicators_B,
		     stress_reactivity_indicators_B)]
data_C <- data_C[, c(depression_indicators_C, anxiety_indicators_C,
		     stress_reactivity_indicators_C)]

colnames(data_B) <- colnames(data_C)
data <- rbind(data_B, data_C)

# ----------
# Depression
# ----------
# C1PA63: During those two weeks, did you lose interest in most things
# C1PA64: Thinking about those same two weeks, did you feel more tired out or
#         low on energy
# C1PA65: During those same two weeks, did you lose appetite
# C1PA66: Did you have more trouble falling asleep than you usually do during
#         those two weeks
# C1PA67: During that same two week period, did you have a lot more trouble
#         concentrating than usual
# C1PA68: People sometimes feel down on themselves, no good, or worthless.
#         During that two-week period, did you feel this way?
# C1PA69: Did you think a lot about death - either your own, someone else's,
#         or death in general - during those two weeks?

# C1PA63, C1PA66, C1PA68,C1PA69
# 1 | YES
# 2 | NO
# 9 | INAPP

# Recode
# One hot with NO as baseline
data$C1PA63[data$C1PA63==2] = 0
data$C1PA66[data$C1PA66==2] = 0
data$C1PA68[data$C1PA68==2] = 0
data$C1PA69[data$C1PA69==2] = 0

# Fill missing
data$C1PA63[data$C1PA63==9] = NA
data$C1PA66[data$C1PA66==9] = NA
data$C1PA68[data$C1PA68==9] = NA
data$C1PA69[data$C1PA69==9] = NA

# table(data[, "C1PA63"])
# |   0   1 
# |  62 231 
# table(data[, "C1PA66"])
# |   0   1 
# |  83 210 
# table(data[, "C1PA68"])
# |   0   1 
# | 106 187 
# table(data[, "C1PA69"])
# |   0   1 
# | 107 186

# C1PA64, C1PA65, C1PA67
# 1 | YES
# 2 | NO
# 7 | DON'T KNOW
# 9 | INAPP

# Recode
# One hot with NO as baseline
data$C1PA64[data$C1PA64==2] = 0
data$C1PA65[data$C1PA65==2] = 0
data$C1PA67[data$C1PA67==2] = 0

# Fill missing
data$C1PA64[data$C1PA64==7] = NA
data$C1PA64[data$C1PA64==9] = NA
data$C1PA65[data$C1PA65==7] = NA
data$C1PA65[data$C1PA65==9] = NA
data$C1PA67[data$C1PA67==7] = NA
data$C1PA67[data$C1PA67==9] = NA

# table(data[, "C1PA64"])
# |   0   1 
# |  16 276 
# table(data[, "C1PA65"])
# |   0   1 
# | 120 169 
# table(data[, "C1PA67"])
# |   0   1 
# |  42 250

# -------
# Anxiety
# -------

# C1PA88A: How often over the last 12 months were you restless because of
#          your worry
# C1PA88B: How often over the last 12 months were you keyed up, on edge,
#          or had a lot of nervous energy
# C1PA88C: How often over the last 12 months were you irritable because
#          of your worry
# C1PA88D: How often over the last 12 months did you have trouble falling
#          asleep
# C1PA88E: How often over the last 12 months did you have trouble staying
#          asleep because of your worry
# C1PA88F: How often over the last 12 months did you have trouble keeping
#          your mind on what you were doing
# C1PA88G: How often over the last 12 months did you have trouble remembering
#          things because of your worry
# C1PA88H: How often over the last 12 months were you low on energy
# C1PA88I: How often over the last 12 months did you tire easily because of
#          your worry
# C1PA88J: How often over the last 12 months did you have sore or aching
#          muscles because of tension
# C1PA89:  How much does worry interfere with your life or activities

# C1PA88
# 1 | MOST DAYS
# 2 | ABOUT HALF THE DAYS
# 3 | LESS THAN HALF THE DAYS
# 4 | NEVER
# 7 | DON'T KNOW
# 8 | REFUSED
# 9 | INAPP

# Recode
# Higher now means agreeing more with the statement
data$C1PA88A_temp = data$C1PA88A
data$C1PA88A[data$C1PA88A_temp==1] = 4
data$C1PA88A[data$C1PA88A_temp==2] = 3
data$C1PA88A[data$C1PA88A_temp==3] = 2
data$C1PA88A[data$C1PA88A_temp==4] = 1
data$C1PA88A_temp <- NA
data$C1PA88B_temp = data$C1PA88B
data$C1PA88B[data$C1PA88B_temp==1] = 4
data$C1PA88B[data$C1PA88B_temp==2] = 3
data$C1PA88B[data$C1PA88B_temp==3] = 2
data$C1PA88B[data$C1PA88B_temp==4] = 1
data$C1PA88B_temp <- NA
data$C1PA88C_temp = data$C1PA88C
data$C1PA88C[data$C1PA88C_temp==1] = 4
data$C1PA88C[data$C1PA88C_temp==2] = 3
data$C1PA88C[data$C1PA88C_temp==3] = 2
data$C1PA88C[data$C1PA88C_temp==4] = 1
data$C1PA88C_temp <- NA
data$C1PA88D_temp = data$C1PA88D
data$C1PA88D[data$C1PA88D_temp==1] = 4
data$C1PA88D[data$C1PA88D_temp==2] = 3
data$C1PA88D[data$C1PA88D_temp==3] = 2
data$C1PA88D[data$C1PA88D_temp==4] = 1
data$C1PA88D_temp <- NA
data$C1PA88E_temp = data$C1PA88E
data$C1PA88E[data$C1PA88E_temp==1] = 4
data$C1PA88E[data$C1PA88E_temp==2] = 3
data$C1PA88E[data$C1PA88E_temp==3] = 2
data$C1PA88E[data$C1PA88E_temp==4] = 1
data$C1PA88E_temp <- NA
data$C1PA88F_temp = data$C1PA88F
data$C1PA88F[data$C1PA88F_temp==1] = 4
data$C1PA88F[data$C1PA88F_temp==2] = 3
data$C1PA88F[data$C1PA88F_temp==3] = 2
data$C1PA88F[data$C1PA88F_temp==4] = 1
data$C1PA88F_temp <- NA
data$C1PA88G_temp = data$C1PA88G
data$C1PA88G[data$C1PA88G_temp==1] = 4
data$C1PA88G[data$C1PA88G_temp==2] = 3
data$C1PA88G[data$C1PA88G_temp==3] = 2
data$C1PA88G[data$C1PA88G_temp==4] = 1
data$C1PA88G_temp <- NA
data$C1PA88H_temp = data$C1PA88H
data$C1PA88H[data$C1PA88H_temp==1] = 4
data$C1PA88H[data$C1PA88H_temp==2] = 3
data$C1PA88H[data$C1PA88H_temp==3] = 2
data$C1PA88H[data$C1PA88H_temp==4] = 1
data$C1PA88H_temp <- NA
data$C1PA88I_temp = data$C1PA88I
data$C1PA88I[data$C1PA88I_temp==1] = 4
data$C1PA88I[data$C1PA88I_temp==2] = 3
data$C1PA88I[data$C1PA88I_temp==3] = 2
data$C1PA88I[data$C1PA88I_temp==4] = 1
data$C1PA88I_temp <- NA
data$C1PA88J_temp = data$C1PA88J
data$C1PA88J[data$C1PA88J_temp==1] = 4
data$C1PA88J[data$C1PA88J_temp==2] = 3
data$C1PA88J[data$C1PA88J_temp==3] = 2
data$C1PA88J[data$C1PA88J_temp==4] = 1
data$C1PA88J_temp <- NA

# Fill missing
data$C1PA88A[data$C1PA88A==7] = NA
data$C1PA88A[data$C1PA88A==8] = NA
data$C1PA88A[data$C1PA88A==9] = NA
data$C1PA88B[data$C1PA88B==7] = NA
data$C1PA88B[data$C1PA88B==8] = NA
data$C1PA88B[data$C1PA88B==9] = NA
data$C1PA88C[data$C1PA88C==7] = NA
data$C1PA88C[data$C1PA88C==8] = NA
data$C1PA88C[data$C1PA88C==9] = NA
data$C1PA88D[data$C1PA88D==7] = NA
data$C1PA88D[data$C1PA88D==8] = NA
data$C1PA88D[data$C1PA88D==9] = NA
data$C1PA88E[data$C1PA88E==7] = NA
data$C1PA88E[data$C1PA88E==8] = NA
data$C1PA88E[data$C1PA88E==9] = NA
data$C1PA88F[data$C1PA88F==7] = NA
data$C1PA88F[data$C1PA88F==8] = NA
data$C1PA88F[data$C1PA88F==9] = NA
data$C1PA88G[data$C1PA88G==7] = NA
data$C1PA88G[data$C1PA88G==8] = NA
data$C1PA88G[data$C1PA88G==9] = NA
data$C1PA88H[data$C1PA88H==7] = NA
data$C1PA88H[data$C1PA88H==8] = NA
data$C1PA88H[data$C1PA88H==9] = NA
data$C1PA88I[data$C1PA88I==7] = NA
data$C1PA88I[data$C1PA88I==8] = NA
data$C1PA88I[data$C1PA88I==9] = NA
data$C1PA88J[data$C1PA88J==7] = NA
data$C1PA88J[data$C1PA88J==8] = NA
data$C1PA88J[data$C1PA88J==9] = NA

# table(data[, "C1PA88A"])
# |   1   2   3   4 
# | 108 157 421 145 
# table(data[, "C1PA88B"])
# |   1   2   3   4 
# |  93 170 415 153 
# table(data[, "C1PA88C"])
# |   1   2   3   4 
# | 108 169 419 135 
# table(data[, "C1PA88D"])
# |   1   2   3   4 
# | 181 147 329 173 
# table(data[, "C1PA88E"])
# |   1   2   3   4 
# | 149 131 335 217 
# table(data[, "C1PA88F"])
# |   1   2   3   4 
# | 115 126 410 180 
# table(data[, "C1PA88G"])
# |   1   2   3   4 
# |  89 110 317 314 
# table(data[, "C1PA88H"])
# |   1   2   3   4 
# | 180 154 312 186 
# table(data[, "C1PA88I"])
# |   1   2   3   4 
# | 139 130 304 259 
# table(data[, "C1PA88J"])
# |   1   2   3   4 
# | 127  98 251 356 

# C1PA89: How much does the worry interfere with your life or activities
# 1 | A LOT
# 2 | SOME
# 3 | A LITTLE
# 4 | NOT AT ALL
# 8 | REFUSED
# 9 | INAPP

# Recode
# Higher now means agreeing more with the statement
data$C1PA89_temp = data$C1PA89
data$C1PA89[data$C1PA89_temp==1] = 4
data$C1PA89[data$C1PA89_temp==2] = 3
data$C1PA89[data$C1PA89_temp==3] = 2
data$C1PA89[data$C1PA89_temp==4] = 1
data$C1PA89_temp <- NA

# Fill missing
data$C1PA89[data$C1PA89==8] = NA
data$C1PA89[data$C1PA89==9] = NA

# table(data[, "C1PA89"])
# |  1   2   3   4 
# |108 229 333 163

# -----------------
# Stress reactivity
# -----------------

# C1SE7K: My mood often goes up and down
# C1SE7W: I sometimes get very upset and tense as I think about the day's
#         events
# C1SE7X: Minor setbacks irritate me too much

# -1 | NO SAQ DATA
# 1  | TRUE
# 2  | SOMEWHAT TRUE
# 3  | SOMEWHAT FALSE
# 4  | FALSE
# 8  | REFUSED

# Recode
# Higher now means agreeing more with the statement
data$C1SE7K_temp = data$C1SE7K
data$C1SE7K[data$C1SE7K_temp==1] = 4
data$C1SE7K[data$C1SE7K_temp==2] = 3
data$C1SE7K[data$C1SE7K_temp==3] = 2
data$C1SE7K[data$C1SE7K_temp==4] = 1
data$C1SE7K_temp <- NA

data$C1SE7W_temp = data$C1SE7W
data$C1SE7W[data$C1SE7W_temp==1] = 4
data$C1SE7W[data$C1SE7W_temp==2] = 3
data$C1SE7W[data$C1SE7W_temp==3] = 2
data$C1SE7W[data$C1SE7W_temp==4] = 1
data$C1SE7W_temp <- NA

data$C1SE7X_temp = data$C1SE7X
data$C1SE7X[data$C1SE7X_temp==1] = 4
data$C1SE7X[data$C1SE7X_temp==2] = 3
data$C1SE7X[data$C1SE7X_temp==3] = 2
data$C1SE7X[data$C1SE7X_temp==4] = 1
data$C1SE7X_temp <- NA

# Fill missing
data$C1SE7K[data$C1SE7K==-1] = NA
data$C1SE7K[data$C1SE7K==8] = NA
data$C1SE7K[data$C1SE7K==9] = NA
data$C1SE7W[data$C1SE7W==-1] = NA
data$C1SE7W[data$C1SE7W==8] = NA
data$C1SE7W[data$C1SE7W==9] = NA
data$C1SE7X[data$C1SE7X==-1] = NA
data$C1SE7X[data$C1SE7X==8] = NA
data$C1SE7X[data$C1SE7X==9] = NA

# table(data[, "C1SE7K"])
#    1    2    3    4 
# 1172  928  626  167 
# table(data[, "C1SE7W"])
#    1    2    3    4 
# 1173  920  667  132 
# table(data[, "C1SE7X"])
#   1   2   3   4 
# 897 903 912 173 

data <- data[, c(depression_indicators_C, anxiety_indicators_C,
                 stress_reactivity_indicators_C)]
data <- na.omit(data)

# head(data)
# dim(data)
# cor(data)
# sum(is.na(data))

model <- "
    # measurement
    depression =~ C1PA63 + C1PA64 + C1PA65 + C1PA66 + C1PA67 + C1PA68 + C1PA69
    # anxiety =~ C1PA88A + C1PA88B + C1PA88C + C1PA88D + C1PA88E + C1PA88F
    #            + C1PA88G + C1PA88H + C1PA88I + C1PA88J + C1PA89
    # stress_reactivity =~ C1SE7K + C1SE7W + C1SE7X
    # regression
    # depression ~ a*anxiety + b*stress_reactivity
    # stress_reactivity ~ c*anxiety

    # indirect := a*c
    # total := b + (a*c)
"

fit <- cfa(model, data=data, ordered=TRUE)
summary(fit, standardized=TRUE, fit.measures=TRUE)

jpeg(file="temp.png", width=15, height=15, units="cm", res=700)
semPaths(fit, what="est", whatLabels="stand", layout="tree2")
dev.off()
