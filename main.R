library(lavaan)
library(semPlot)

data_B <- read.csv("ICPSR_04652/DS0001/04652-0001-Data.tsv", sep="\t")
data_C <- read.csv("ICPSR_36346/DS0001/36346-0001-Data.tsv", sep="\t")

depression_indicators_B <- c("B1PA63", "B1PA64", "B1PA65", "B1PA66",
                             "B1PA67", "B1PA68", "B1PA69")
depression_indicators_C <- c("C1PA63", "C1PA64", "C1PA65", "C1PA66",
                             "C1PA67", "C1PA68", "C1PA69")

anxiety_indicators_B <- c("B1PA88A", "B1PA88B", "B1PA88C", "B1PA88D",
                          "B1PA88E", "B1PA88F", "B1PA88G", "B1PA88H",
                          "B1PA88I", "B1PA88J", "B1PA89")
anxiety_indicators_C <- c("C1PA88A", "C1PA88B", "C1PA88C", "C1PA88D",
                          "C1PA88E", "C1PA88F", "C1PA88G", "C1PA88H",
                          "C1PA88I", "C1PA88J", "C1PA89")

stress_reactivity_indicators_B <- c("B1SE7K", "B1SE7W", "B1SE7X")
stress_reactivity_indicators_C <- c("C1SE7K", "C1SE7W", "C1SE7X")

harm_avoidance_indicators_B <- c("B1SE7D", "B1SE7V", "B1SE8", "B1SE9")
harm_avoidance_indicators_C <- c("C1SE7D", "C1SE7V", "C1SE8", "C1SE9")

self_directedness_columns_B <- c("B1SE12O", "B1SE12P", "B1SE12R")
self_directedness_columns_C <- c("C1SE14O", "C1SE14P", "C1SE14R")

social_functioning_columns_B <- c("B1SE1BB", "B1SE1D", "B1SE1HH", "B1SE1J",
                                  "B1SE1P", "B1SE1V")
social_functioning_columns_C <- c("C1SE1BB", "C1SE1D", "C1SE1HH", "C1SE1J",
                                  "C1SE1P", "C1SE1V")

social_welness_columns_B <- c("B1SH16A", "B1SH16H", "B1SH16B" ,"B1SH16F",
                              "B1SH16K", "B1SH16C", "B1SH16J", "B1SH16N",
                              "B1SH16D", "B1SH16G", "B1SH16O", "B1SH16E",
                              "B1SH16I", "B1SH16M")
social_welness_columns_C <- c("C1SH16A", "C1SH16H", "C1SH16B" ,"C1SH16F",
                              "C1SH16K", "C1SH16C", "C1SH16J", "C1SH16N",
                              "C1SH16D", "C1SH16G", "C1SH16O", "C1SH16E",
                              "C1SH16I", "C1SH16M")

data_B <- data_B[, c(depression_indicators_B,
                     harm_avoidance_indicators_B,
                     self_directedness_columns_B,
                     social_functioning_columns_B,
                     social_welness_columns_B,
                     "B1PRSEX", "B1PAGE_M2")]
data_B$source <- 1
data_C <- data_C[, c(depression_indicators_C,
                     harm_avoidance_indicators_C,
                     self_directedness_columns_C,
                     social_functioning_columns_C,
                     social_welness_columns_C,
                     "C1PRSEX", "C1PRAGE")]
data_C$source <- 2

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

# Recode
# One hot with NO as baseline
data$C1PA63[data$C1PA63==2] = 0
data$C1PA64[data$C1PA64==2] = 0
data$C1PA65[data$C1PA65==2] = 0
data$C1PA66[data$C1PA66==2] = 0
data$C1PA67[data$C1PA67==2] = 0
data$C1PA68[data$C1PA68==2] = 0
data$C1PA69[data$C1PA69==2] = 0

# Fill missing
data$C1PA63[data$C1PA63==9] = NA
data$C1PA63[data$C1PA63==7] = NA
data$C1PA64[data$C1PA64==7] = NA
data$C1PA64[data$C1PA64==9] = NA
data$C1PA65[data$C1PA65==7] = NA
data$C1PA65[data$C1PA65==9] = NA
data$C1PA66[data$C1PA66==9] = NA
data$C1PA66[data$C1PA66==7] = NA
data$C1PA67[data$C1PA67==7] = NA
data$C1PA67[data$C1PA67==9] = NA
data$C1PA68[data$C1PA68==9] = NA
data$C1PA68[data$C1PA68==7] = NA
data$C1PA69[data$C1PA69==9] = NA
data$C1PA69[data$C1PA69==7] = NA

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
# data$C1PA88A_temp = data$C1PA88A
# data$C1PA88A[data$C1PA88A_temp==1] = 4
# data$C1PA88A[data$C1PA88A_temp==2] = 3
# data$C1PA88A[data$C1PA88A_temp==3] = 2
# data$C1PA88A[data$C1PA88A_temp==4] = 1
# data$C1PA88A_temp <- NA
# data$C1PA88B_temp = data$C1PA88B
# data$C1PA88B[data$C1PA88B_temp==1] = 4
# data$C1PA88B[data$C1PA88B_temp==2] = 3
# data$C1PA88B[data$C1PA88B_temp==3] = 2
# data$C1PA88B[data$C1PA88B_temp==4] = 1
# data$C1PA88B_temp <- NA
# data$C1PA88C_temp = data$C1PA88C
# data$C1PA88C[data$C1PA88C_temp==1] = 4
# data$C1PA88C[data$C1PA88C_temp==2] = 3
# data$C1PA88C[data$C1PA88C_temp==3] = 2
# data$C1PA88C[data$C1PA88C_temp==4] = 1
# data$C1PA88C_temp <- NA
# data$C1PA88D_temp = data$C1PA88D
# data$C1PA88D[data$C1PA88D_temp==1] = 4
# data$C1PA88D[data$C1PA88D_temp==2] = 3
# data$C1PA88D[data$C1PA88D_temp==3] = 2
# data$C1PA88D[data$C1PA88D_temp==4] = 1
# data$C1PA88D_temp <- NA
# data$C1PA88E_temp = data$C1PA88E
# data$C1PA88E[data$C1PA88E_temp==1] = 4
# data$C1PA88E[data$C1PA88E_temp==2] = 3
# data$C1PA88E[data$C1PA88E_temp==3] = 2
# data$C1PA88E[data$C1PA88E_temp==4] = 1
# data$C1PA88E_temp <- NA
# data$C1PA88F_temp = data$C1PA88F
# data$C1PA88F[data$C1PA88F_temp==1] = 4
# data$C1PA88F[data$C1PA88F_temp==2] = 3
# data$C1PA88F[data$C1PA88F_temp==3] = 2
# data$C1PA88F[data$C1PA88F_temp==4] = 1
# data$C1PA88F_temp <- NA
# data$C1PA88G_temp = data$C1PA88G
# data$C1PA88G[data$C1PA88G_temp==1] = 4
# data$C1PA88G[data$C1PA88G_temp==2] = 3
# data$C1PA88G[data$C1PA88G_temp==3] = 2
# data$C1PA88G[data$C1PA88G_temp==4] = 1
# data$C1PA88G_temp <- NA
# data$C1PA88H_temp = data$C1PA88H
# data$C1PA88H[data$C1PA88H_temp==1] = 4
# data$C1PA88H[data$C1PA88H_temp==2] = 3
# data$C1PA88H[data$C1PA88H_temp==3] = 2
# data$C1PA88H[data$C1PA88H_temp==4] = 1
# data$C1PA88H_temp <- NA
# data$C1PA88I_temp = data$C1PA88I
# data$C1PA88I[data$C1PA88I_temp==1] = 4
# data$C1PA88I[data$C1PA88I_temp==2] = 3
# data$C1PA88I[data$C1PA88I_temp==3] = 2
# data$C1PA88I[data$C1PA88I_temp==4] = 1
# data$C1PA88I_temp <- NA
# data$C1PA88J_temp = data$C1PA88J
# data$C1PA88J[data$C1PA88J_temp==1] = 4
# data$C1PA88J[data$C1PA88J_temp==2] = 3
# data$C1PA88J[data$C1PA88J_temp==3] = 2
# data$C1PA88J[data$C1PA88J_temp==4] = 1
# data$C1PA88J_temp <- NA

# Fill missing
# data$C1PA88A[data$C1PA88A==7] = NA
# data$C1PA88A[data$C1PA88A==8] = NA
# data$C1PA88A[data$C1PA88A==9] = NA
# data$C1PA88B[data$C1PA88B==7] = NA
# data$C1PA88B[data$C1PA88B==8] = NA
# data$C1PA88B[data$C1PA88B==9] = NA
# data$C1PA88C[data$C1PA88C==7] = NA
# data$C1PA88C[data$C1PA88C==8] = NA
# data$C1PA88C[data$C1PA88C==9] = NA
# data$C1PA88D[data$C1PA88D==7] = NA
# data$C1PA88D[data$C1PA88D==8] = NA
# data$C1PA88D[data$C1PA88D==9] = NA
# data$C1PA88E[data$C1PA88E==7] = NA
# data$C1PA88E[data$C1PA88E==8] = NA
# data$C1PA88E[data$C1PA88E==9] = NA
# data$C1PA88F[data$C1PA88F==7] = NA
# data$C1PA88F[data$C1PA88F==8] = NA
# data$C1PA88F[data$C1PA88F==9] = NA
# data$C1PA88G[data$C1PA88G==7] = NA
# data$C1PA88G[data$C1PA88G==8] = NA
# data$C1PA88G[data$C1PA88G==9] = NA
# data$C1PA88H[data$C1PA88H==7] = NA
# data$C1PA88H[data$C1PA88H==8] = NA
# data$C1PA88H[data$C1PA88H==9] = NA
# data$C1PA88I[data$C1PA88I==7] = NA
# data$C1PA88I[data$C1PA88I==8] = NA
# data$C1PA88I[data$C1PA88I==9] = NA
# data$C1PA88J[data$C1PA88J==7] = NA
# data$C1PA88J[data$C1PA88J==8] = NA
# data$C1PA88J[data$C1PA88J==9] = NA

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
# data$C1PA89_temp = data$C1PA89
# data$C1PA89[data$C1PA89_temp==1] = 4
# data$C1PA89[data$C1PA89_temp==2] = 3
# data$C1PA89[data$C1PA89_temp==3] = 2
# data$C1PA89[data$C1PA89_temp==4] = 1
# data$C1PA89_temp <- NA

# Fill missing
# data$C1PA89[data$C1PA89==8] = NA
# data$C1PA89[data$C1PA89==9] = NA

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

# recode
# higher now means agreeing more with the statement
# data$C1SE7K_temp = data$C1SE7K
# data$C1SE7K[data$C1SE7K_temp==1] = 4
# data$C1SE7K[data$C1SE7K_temp==2] = 3
# data$C1SE7K[data$C1SE7K_temp==3] = 2
# data$C1SE7K[data$C1SE7K_temp==4] = 1
# data$C1SE7K_temp <- NA

# data$C1SE7W_temp = data$C1SE7W
# data$C1SE7W[data$C1SE7W_temp==1] = 4
# data$C1SE7W[data$C1SE7W_temp==2] = 3
# data$C1SE7W[data$C1SE7W_temp==3] = 2
# data$C1SE7W[data$C1SE7W_temp==4] = 1
# data$C1SE7W_temp <- NA

# data$C1SE7X_temp = data$C1SE7X
# data$C1SE7X[data$C1SE7X_temp==1] = 4
# data$C1SE7X[data$C1SE7X_temp==2] = 3
# data$C1SE7X[data$C1SE7X_temp==3] = 2
# data$C1SE7X[data$C1SE7X_temp==4] = 1
# data$C1SE7X_temp <- NA

# Fill missing
# data$C1SE7K[data$C1SE7K==-1] = NA
# data$C1SE7K[data$C1SE7K==8] = NA
# data$C1SE7K[data$C1SE7K==9] = NA
# data$C1SE7W[data$C1SE7W==-1] = NA
# data$C1SE7W[data$C1SE7W==8] = NA
# data$C1SE7W[data$C1SE7W==9] = NA
# data$C1SE7X[data$C1SE7X==-1] = NA
# data$C1SE7X[data$C1SE7X==8] = NA
# data$C1SE7X[data$C1SE7X==9] = NA

# table(data[, "C1SE7K"])
#    1    2    3    4
# 1172  928  626  167
# table(data[, "C1SE7W"])
#    1    2    3    4
# 1173  920  667  132
# table(data[, "C1SE7X"])
#   1   2   3   4
# 897 903 912 173

# --------------
# Harm avoidance
# --------------
# C1SE7D: It might be fun and exciting to experience an earthquake
# C1SE7V: It might be fun learning to walk a tightrope
# C1SE8:  prefer: riding a long stretch of rapids in a canoe or waiting for
#         someone
# C1SE9:  prefer: be at the circus when two lions get loose down in the ring
#         or bring your family to the circus and not get in

# --- C1SE7D, C1SE7V
# -1 | RESPONDENT DOES NOT HAVE SAQ DATA
# 1  | TRUE OF YOU
# 2  | SOMEWHAT TRUE
# 3  | SOMEWHAT FALSE
# 4  | FALSE
# 8  | REFUSED

# Recode
# Higher now means agreeing more with the statement
# data$C1SE7D_temp = data$C1SE7D
# data$C1SE7D[data$C1SE7D_temp==1] = 4
# data$C1SE7D[data$C1SE7D_temp==2] = 3
# data$C1SE7D[data$C1SE7D_temp==3] = 2
# data$C1SE7D[data$C1SE7D_temp==4] = 1
# data$C1SE7D_temp <- NA
# 
# data$C1SE7V_temp = data$C1SE7V
# data$C1SE7V[data$C1SE7V_temp==1] = 4
# data$C1SE7V[data$C1SE7V_temp==2] = 3
# data$C1SE7V[data$C1SE7V_temp==3] = 2
# data$C1SE7V[data$C1SE7V_temp==4] = 1
# data$C1SE7V_temp <- NA

# Fill missing
data$C1SE7D[data$C1SE7D==-1] = NA
data$C1SE7D[data$C1SE7D==8] = NA
data$C1SE7V[data$C1SE7V==-1] = NA
data$C1SE7V[data$C1SE7V==8] = NA

# table(data$C1SE7D)
# |    1    2    3    4 
# | 4889  838  875  274 
# table(data$C1SE7V)
# |    1    2    3    4 
# | 4138 1163 1222  367 

# --- C1SE8, C1SE9
# -1 | RESPONDENT DOES NOT HAVE SAQ DATA
# 8  | REFUSED
# 1  | SITUATION 1: NO HARM AVOIDANCE
# 2  | SITUATION 2: HARM AVOIDANCE

# Recode
# Higher now means agreeing more with the statement
data$C1SE8_temp = data$C1SE8
data$C1SE8[data$C1SE8_temp==1] = 1
data$C1SE8[data$C1SE8_temp==2] = 0
data$C1SE8_temp <- NA

data$C1SE9_temp = data$C1SE9
data$C1SE9[data$C1SE9_temp==1] = 1
data$C1SE9[data$C1SE9_temp==2] = 0
data$C1SE9_temp <- NA

# Fill missing
data$C1SE8[data$C1SE8==-1] = NA
data$C1SE8[data$C1SE8==8] = NA
data$C1SE9[data$C1SE9==-1] = NA
data$C1SE9[data$C1SE9==8] = NA

# table(data$C1SE8)
# |    0    1 
# | 3089 3803 
# table(data$C1SE9)
# |    0    1 
# | 3898 2994 

# -----------------
# Self-directedness
# -----------------
# C1SE14O: I like to make plans for the future
# C1SE14P: I know what I want out of life
# C1SE14R: I find it helpful to set goals for the near future

# --- C1SE14O, C1SE14P, C1SE14R
# -1 | RESPONDENT DOES NOT HAVE SAQ DATA
# 1  | A LOT
# 2  | SOME
# 3  | A LITTLE
# 4  | NOT AT ALL
# 8  | REFUSED

# Recode
# Higher now means agreeing more with the statement
data$C1SE14O_temp = data$C1SE14O
data$C1SE14O[data$C1SE14O_temp==1] = 4
data$C1SE14O[data$C1SE14O_temp==2] = 3
data$C1SE14O[data$C1SE14O_temp==3] = 2
data$C1SE14O[data$C1SE14O_temp==4] = 1
data$C1SE14O_temp <- NA

data$C1SE14P_temp = data$C1SE14P
data$C1SE14P[data$C1SE14P_temp==1] = 4
data$C1SE14P[data$C1SE14P_temp==2] = 3
data$C1SE14P[data$C1SE14P_temp==3] = 2
data$C1SE14P[data$C1SE14P_temp==4] = 1
data$C1SE14P_temp <- NA

data$C1SE14R_temp = data$C1SE14R
data$C1SE14R[data$C1SE14R_temp==1] = 4
data$C1SE14R[data$C1SE14R_temp==2] = 3
data$C1SE14R[data$C1SE14R_temp==3] = 2
data$C1SE14R[data$C1SE14R_temp==4] = 1
data$C1SE14R_temp <- NA

# Fill missing
data$C1SE14O[data$C1SE14O==-1] = NA
data$C1SE14O[data$C1SE14O==8] = NA
data$C1SE14P[data$C1SE14P==-1] = NA
data$C1SE14P[data$C1SE14P==8] = NA
data$C1SE14R[data$C1SE14R==-1] = NA
data$C1SE14R[data$C1SE14R==8] = NA


# table(data$C1SE14O)
#    1    2    3    4
#  247 1303 2754 2590
# table(data$C1SE14P)
#    1    2    3    4
#  251 1089 2929 2604
# table(data$C1SE14R)
#    1    2    3    4
#  318 1320 3071 2184

# ------------------
# Social functioning
# ------------------
# C1SE1BB: People would describe me as a giving person, willing to share my
#          time with others
# C1SE1D:  Most people see me as loving and affectionate
# C1SE1HH: I have not experienced many warm and trusting relationships with
#          others
# C1SE1J:  Maintaining close relationships has been difficult and frustrating
#          for me
# C1SE1P:  I often feel lonely because I have few close friends with whom to
#          share my concerns
# C1SE1V:  I enjoy personal and mutual conversations with family members and
#          friends

# -1 | RESPONDENT DOES NOT HAVE SAQ DATA
# 1  | AGREE STRONGLY
# 2  | AGREE SOMEWHAT
# 3  | AGREE A LITTLE
# 4  | NEITHER AGREE NOR DISAGREE
# 5  | DISAGREE A LITTLE
# 6  | DISAGREE SOMEWHAT
# 7  | DISAGREE STRONGLY
# 8  | REFUSED

# Recode
# Higher now means agreeing more with the statement
data$C1SE1HH_temp = data$C1SE1HH
data$C1SE1HH[data$C1SE1HH_temp==1] = 7
data$C1SE1HH[data$C1SE1HH_temp==2] = 6
data$C1SE1HH[data$C1SE1HH_temp==3] = 5
data$C1SE1HH[data$C1SE1HH_temp==4] = 4
data$C1SE1HH[data$C1SE1HH_temp==5] = 3
data$C1SE1HH[data$C1SE1HH_temp==6] = 2
data$C1SE1HH[data$C1SE1HH_temp==7] = 1
data$C1SE1HH_temp <- NA

data$C1SE1J_temp = data$C1SE1J
data$C1SE1J[data$C1SE1J_temp==1] = 7
data$C1SE1J[data$C1SE1J_temp==2] = 6
data$C1SE1J[data$C1SE1J_temp==3] = 5
data$C1SE1J[data$C1SE1J_temp==4] = 4
data$C1SE1J[data$C1SE1J_temp==5] = 3
data$C1SE1J[data$C1SE1J_temp==6] = 2
data$C1SE1J[data$C1SE1J_temp==7] = 1
data$C1SE1J_temp <- NA

data$C1SE1P_temp = data$C1SE1P
data$C1SE1P[data$C1SE1P_temp==1] = 7
data$C1SE1P[data$C1SE1P_temp==2] = 6
data$C1SE1P[data$C1SE1P_temp==3] = 5
data$C1SE1P[data$C1SE1P_temp==4] = 4
data$C1SE1P[data$C1SE1P_temp==5] = 3
data$C1SE1P[data$C1SE1P_temp==6] = 2
data$C1SE1P[data$C1SE1P_temp==7] = 1
data$C1SE1P_temp <- NA

# Fill missing
data$C1SE1BB[data$C1SE1BB==-1] = NA
data$C1SE1BB[data$C1SE1BB==8] = NA
data$C1SE1D[data$C1SE1D==-1] = NA
data$C1SE1D[data$C1SE1D==8] = NA
data$C1SE1HH[data$C1SE1HH==-1] = NA
data$C1SE1HH[data$C1SE1HH==8] = NA
data$C1SE1J[data$C1SE1J==-1] = NA
data$C1SE1J[data$C1SE1J==8] = NA
data$C1SE1P[data$C1SE1P==-1] = NA
data$C1SE1P[data$C1SE1P==8] = NA
data$C1SE1V[data$C1SE1V==-1] = NA
data$C1SE1V[data$C1SE1V==8] = NA

# table(data$C1SE1BB)
#    1    2    3    4    5    6    7 
#   28   76  127  472  799 2382 3055 
# table(data$C1SE1D)
#    1    2    3    4    5    6    7 
#   43  126  219  807  815 2598 2306 
# table(data$C1SE1HH)
#    1    2    3    4    5    6    7 
#  291  501  575  441  497 1382 3242 
# table(data$C1SE1J)
#    1    2    3    4    5    6    7 
#  236  575  818  711  512 1524 2550 
# table(data$C1SE1I)
#    1    2    3    4    5    6    7 
#  113  136  142  709 1154 2190 2483 
# table(data$C1SE1P)
#    1    2    3    4    5    6    7 
#  242  447  744  655  479 1321 3036 
# table(data$C1SE1V)
#    1    2    3    4    5    6    7 
#   63   64  105  211  589 1879 4024 

# --------------
# Social welness
# --------------

# Recode
data$C1SH16A_temp = data$C1SH16A
data$C1SH16A[data$C1SH16A_temp==1] = 7
data$C1SH16A[data$C1SH16A_temp==2] = 6
data$C1SH16A[data$C1SH16A_temp==3] = 5
data$C1SH16A[data$C1SH16A_temp==4] = 4
data$C1SH16A[data$C1SH16A_temp==5] = 3
data$C1SH16A[data$C1SH16A_temp==6] = 2
data$C1SH16A[data$C1SH16A_temp==7] = 1
data$C1SH16A_temp <- NA

data$C1SH16H_temp = data$C1SH16H
data$C1SH16H[data$C1SH16H_temp==1] = 7
data$C1SH16H[data$C1SH16H_temp==2] = 6
data$C1SH16H[data$C1SH16H_temp==3] = 5
data$C1SH16H[data$C1SH16H_temp==4] = 4
data$C1SH16H[data$C1SH16H_temp==5] = 3
data$C1SH16H[data$C1SH16H_temp==6] = 2
data$C1SH16H[data$C1SH16H_temp==7] = 1
data$C1SH16H_temp <- NA

data$C1SH16B_temp = data$C1SH16B
data$C1SH16B[data$C1SH16B_temp==1] = 7
data$C1SH16B[data$C1SH16B_temp==2] = 6
data$C1SH16B[data$C1SH16B_temp==3] = 5
data$C1SH16B[data$C1SH16B_temp==4] = 4
data$C1SH16B[data$C1SH16B_temp==5] = 3
data$C1SH16B[data$C1SH16B_temp==6] = 2
data$C1SH16B[data$C1SH16B_temp==7] = 1
data$C1SH16B_temp <- NA

data$C1SH16J_temp = data$C1SH16J
data$C1SH16J[data$C1SH16J_temp==1] = 7
data$C1SH16J[data$C1SH16J_temp==2] = 6
data$C1SH16J[data$C1SH16J_temp==3] = 5
data$C1SH16J[data$C1SH16J_temp==4] = 4
data$C1SH16J[data$C1SH16J_temp==5] = 3
data$C1SH16J[data$C1SH16J_temp==6] = 2
data$C1SH16J[data$C1SH16J_temp==7] = 1
data$C1SH16J_temp <- NA

data$C1SH16G_temp = data$C1SH16G
data$C1SH16G[data$C1SH16G_temp==1] = 7
data$C1SH16G[data$C1SH16G_temp==2] = 6
data$C1SH16G[data$C1SH16G_temp==3] = 5
data$C1SH16G[data$C1SH16G_temp==4] = 4
data$C1SH16G[data$C1SH16G_temp==5] = 3
data$C1SH16G[data$C1SH16G_temp==6] = 2
data$C1SH16G[data$C1SH16G_temp==7] = 1
data$C1SH16G_temp <- NA

data$C1SH16O_temp = data$C1SH16O
data$C1SH16O[data$C1SH16O_temp==1] = 7
data$C1SH16O[data$C1SH16O_temp==2] = 6
data$C1SH16O[data$C1SH16O_temp==3] = 5
data$C1SH16O[data$C1SH16O_temp==4] = 4
data$C1SH16O[data$C1SH16O_temp==5] = 3
data$C1SH16O[data$C1SH16O_temp==6] = 2
data$C1SH16O[data$C1SH16O_temp==7] = 1
data$C1SH16O_temp <- NA

data$C1SH16I_temp = data$C1SH16I
data$C1SH16I[data$C1SH16I_temp==1] = 7
data$C1SH16I[data$C1SH16I_temp==2] = 6
data$C1SH16I[data$C1SH16I_temp==3] = 5
data$C1SH16I[data$C1SH16I_temp==4] = 4
data$C1SH16I[data$C1SH16I_temp==5] = 3
data$C1SH16I[data$C1SH16I_temp==6] = 2
data$C1SH16I[data$C1SH16I_temp==7] = 1
data$C1SH16I_temp <- NA

data$C1SH16M_temp = data$C1SH16M
data$C1SH16M[data$C1SH16M_temp==1] = 7
data$C1SH16M[data$C1SH16M_temp==2] = 6
data$C1SH16M[data$C1SH16M_temp==3] = 5
data$C1SH16M[data$C1SH16M_temp==4] = 4
data$C1SH16M[data$C1SH16M_temp==5] = 3
data$C1SH16M[data$C1SH16M_temp==6] = 2
data$C1SH16M[data$C1SH16M_temp==7] = 1
data$C1SH16M_temp <- NA


# Fill mising
data$C1SH16A[data$C1SH16A==-1] = NA
data$C1SH16A[data$C1SH16A==8] = NA
data$C1SH16H[data$C1SH16H==-1] = NA
data$C1SH16H[data$C1SH16H==8] = NA
data$C1SH16B[data$C1SH16B==-1] = NA
data$C1SH16B[data$C1SH16B==8] = NA
data$C1SH16F[data$C1SH16F==-1] = NA
data$C1SH16F[data$C1SH16F==8] = NA
data$C1SH16K[data$C1SH16K==-1] = NA
data$C1SH16K[data$C1SH16K==8] = NA
data$C1SH16C[data$C1SH16C==-1] = NA
data$C1SH16C[data$C1SH16C==8] = NA
data$C1SH16J[data$C1SH16J==-1] = NA
data$C1SH16J[data$C1SH16J==8] = NA
data$C1SH16N[data$C1SH16N==-1] = NA
data$C1SH16N[data$C1SH16N==8] = NA
data$C1SH16D[data$C1SH16D==-1] = NA
data$C1SH16D[data$C1SH16D==8] = NA
data$C1SH16G[data$C1SH16G==-1] = NA
data$C1SH16G[data$C1SH16G==8] = NA
data$C1SH16O[data$C1SH16O==-1] = NA
data$C1SH16O[data$C1SH16O==8] = NA
data$C1SH16E[data$C1SH16E==-1] = NA
data$C1SH16E[data$C1SH16E==8] = NA
data$C1SH16I[data$C1SH16I==-1] = NA
data$C1SH16I[data$C1SH16I==8] = NA
data$C1SH16M[data$C1SH16M==-1] = NA
data$C1SH16M[data$C1SH16M==8] = NA

# ---
# Age
# ---

# Categorize based on Chan et al. (2020)
data$C1PRAGE_temp <- data$C1PRAGE
data$C1PRAGE[data$C1PRAGE_temp<=40] <- 1                        # young adulthood
data$C1PRAGE[data$C1PRAGE_temp>40 & data$C1PRAGE_temp<=60] <- 2 # middle adulthood
data$C1PRAGE[data$C1PRAGE_temp>60] <- 3                         # late adulthood
data$C1PRAGE_temp <- NA

# -----------------------------------------------------------------------------

data <- data[, c(depression_indicators_C,
                 harm_avoidance_indicators_C,
                 self_directedness_columns_C,
                 social_functioning_columns_C,
                 "C1PRSEX", "C1PRAGE", "source")]
data <- na.omit(data)
print("DIM DATA"); dim(data);

# library(corrplot)
# jpeg(file="visualizations/corr.png", width=30, height=30, units="cm", res=300)
# corrplot(cor(data), method="color", type="lower")
# dev.off()
# 
# library("misty")
# jpeg(file="visualizations/corr_poly.png", width=30, height=30, units="cm", res=300)
# corrplot(cor.poly(data)$result$cor, method="color", type="lower")
# dev.off()

# --- Depression
# table(data$C1PA63)
#   0   1 
# 126 479 
# table(data$C1PA64)
#   0   1 
#  51 554 
# table(data$C1PA65)
#   0   1 
# 263 342 
# table(data$C1PA66)
#   0   1 
# 172 433 
# table(data$C1PA67)
#   0   1 
#  88 517 
# table(data$C1PA68)
#   0   1 
# 222 383 
# table(data$C1PA69)
#   0   1 
# 229 376 

# --- Harm avoidance
# table(data$C1SE7V)
#   1   2   3   4 
#  33  92  91 389 
# table(data$C1SE7D)
#   1   2   3   4 
#  25  79  69 432 
# table(data$C1SE8)
#   0   1 
# 335 270 
# table(data$C1SE9)
#   0   1 
# 276 329 

# --- Self-directedness
# table(data$C1SE14O)
#   1   2   3   4 
#  48 140 241 176 
# table(data$C1SE14P)
#   1   2   3   4 
#  67 144 235 159 
# table(data$C1SE14R)
#   1   2   3   4 
#  47 149 228 181

# --- Social functioning
# table(data$C1SE1BB)
#   1   2   3   4   5   6   7 
#   4   7  16  36  70 207 265 
# table(data$C1SE1D)
#   1   2   3   4   5   6   7 
#   5  14  23  68  74 225 196 
# table(data$C1SE1HH)
#   1   2   3   4   5   6   7 
#  75  71  69  34  51 113 192 
# table(data$C1SE1J)
#   1   2   3   4   5   6   7 
#  67  88 106  57  52  94 141 
# table(data$C1SE1I)
#   1   2   3   4   5   6   7 
#   7  13  14  57 124 175 215 
# table(data$C1SE1P)
#   1   2   3   4   5   6   7 
#  71  82  80  51  50 103 168 
# table(data$C1SE1V)
#   1   2   3   4   5   6   7 
#  14  14  17  26  82 159 293 

# --- Invariance of gender
# cat("GENDER\n")
# table(data$C1PRSEX)
#   1   2 
# 168 440 

# cfa.model <- "
#     depression =~ C1PA63 + C1PA64 + C1PA65 + C1PA66 + C1PA67 + C1PA68 + C1PA69
#     harm_avoidance =~ C1SE7V + C1SE7D + C1SE8 + C1SE9
#     self_directedness =~ C1SE14O + C1SE14P + C1SE14R
#     social_functioning =~ C1SE1BB + C1SE1D + C1SE1HH + C1SE1J + C1SE1P + C1SE1V
# "
# cfa.fit.conf <- cfa(cfa.model, data=data, ordered=TRUE, estimator="DWLS",
#                     group="C1PRSEX", meanstructure=TRUE)
# cfa.fit.tau <- cfa(cfa.model, data=data, ordered=TRUE, estimator="DWLS",
#                    group="C1PRSEX", group.equal=c("loadings"), meanstructure=TRUE)
# cfa.fit.parallel <- cfa(cfa.model, data=data, ordered=TRUE, estimator="DWLS",
#                         group="C1PRSEX", group.equal=c("loadings", "intercepts"),
#                         meanstructure=TRUE)

# lavTestLRT(cfa.fit.conf, cfa.fit.tau)
# Chi-Squared Difference Test
#               Df AIC BIC  Chisq Chisq diff    RMSEA Df diff Pr(>Chisq)    
# cfa.fit.conf 328         651.13                                           
# cfa.fit.tau  344         701.56     50.438 0.084144      16  1.952e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# lavTestLRT(cfa.fit.tau, cfa.fit.parallel)
# Chi-Squared Difference Test
#                   Df AIC BIC  Chisq Chisq diff RMSEA Df diff Pr(>Chisq)
# cfa.fit.tau      344         701.56                                    
# cfa.fit.parallel 380         726.79     25.229     0      36     0.9104
# summary(cfa.fit.conf, fit.measures=TRUE, standardized=TRUE)$fit
#                  npar                  fmin                 chisq 
#               172.000                 0.535               651.126 
#                    df                pvalue        baseline.chisq 
#               328.000                 0.000              7769.046 
#           baseline.df       baseline.pvalue                   cfi 
#               380.000                 0.000                 0.956 
#                   tli                 rmsea        rmsea.ci.lower 
#                 0.949                 0.057                 0.051 
#        rmsea.ci.upper        rmsea.ci.level          rmsea.pvalue 
#                 0.063                 0.900                 0.037 
#        rmsea.close.h0 rmsea.notclose.pvalue     rmsea.notclose.h0 
#                 0.050                 0.000                 0.080 
#                  srmr 
#                 0.090 
# summary(cfa.fit.tau, fit.measures=TRUE, standardized=TRUE)$fit
#                  npar                  fmin                 chisq 
#               156.000                 0.577               701.564 
#                    df                pvalue        baseline.chisq 
#               344.000                 0.000              7769.046 
#           baseline.df       baseline.pvalue                   cfi 
#               380.000                 0.000                 0.952 
#                   tli                 rmsea        rmsea.ci.lower 
#                 0.947                 0.059                 0.052 
#        rmsea.ci.upper        rmsea.ci.level          rmsea.pvalue 
#                 0.065                 0.900                 0.012 
#        rmsea.close.h0 rmsea.notclose.pvalue     rmsea.notclose.h0 
#                 0.050                 0.000                 0.080 
#                  srmr 
#                 0.092 
# summary(cfa.fit.parallel, fit.measures=TRUE, standardized=TRUE)$fit
#                  npar                  fmin                 chisq 
#               120.000                 0.598               726.793 
#                    df                pvalue        baseline.chisq 
#               380.000                 0.000              7769.046 
#           baseline.df       baseline.pvalue                   cfi 
#               380.000                 0.000                 0.953 
#                   tli                 rmsea        rmsea.ci.lower 
#                 0.953                 0.055                 0.049 
#        rmsea.ci.upper        rmsea.ci.level          rmsea.pvalue 
#                 0.061                 0.900                 0.091 
#        rmsea.close.h0 rmsea.notclose.pvalue     rmsea.notclose.h0 
#                 0.050                 0.000                 0.080 
#                  srmr 
#                 0.092 

# --- Longitudinal invariance
# cat("TIME\n")
# table(data$source)
#   1   2 
# 378 230 

# cfa.fit.conf <- cfa(cfa.model, data=data, ordered=TRUE, estimator="DWLS",
#                     group="source")
# cfa.fit.tau <- cfa(cfa.model, data=data, ordered=TRUE, estimator="DWLS",
#                    group="source", group.equal=c("loadings"))
# cfa.fit.parallel <- cfa(cfa.model, data=data, ordered=TRUE, estimator="DWLS",
#                         group="source", group.equal=c("loadings", "intercepts"))

# lavTestLRT(cfa.fit.conf, cfa.fit.tau)
# Chi-Squared Difference Test
#               Df AIC BIC  Chisq Chisq diff    RMSEA Df diff Pr(>Chisq)   
# cfa.fit.conf 328         653.66                                          
# cfa.fit.tau  344         688.60     34.939 0.062399      16   0.004052 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# lavTestLRT(cfa.fit.tau, cfa.fit.parallel)
# Chi-Squared Difference Test
#                   Df AIC BIC  Chisq Chisq diff RMSEA Df diff Pr(>Chisq)
# cfa.fit.tau      344         688.60                                    
# cfa.fit.parallel 380         702.82     14.218     0      36     0.9996

# summary(cfa.fit.conf, fit.measures=TRUE, standardized=TRUE)$fit
#                  npar                  fmin                 chisq 
#               172.000                 0.538               653.664 
#                    df                pvalue        baseline.chisq 
#               328.000                 0.000              7849.813 
#           baseline.df       baseline.pvalue                   cfi 
#               380.000                 0.000                 0.956 
#                   tli                 rmsea        rmsea.ci.lower 
#                 0.949                 0.057                 0.051 
#        rmsea.ci.upper        rmsea.ci.level          rmsea.pvalue 
#                 0.064                 0.900                 0.032 
#        rmsea.close.h0 rmsea.notclose.pvalue     rmsea.notclose.h0 
#                 0.050                 0.000                 0.080 
#                  srmr 
#                 0.092 
# summary(cfa.fit.tau, fit.measures=TRUE, standardized=TRUE)$fit
#                  npar                  fmin                 chisq 
#               156.000                 0.566               688.602 
#                    df                pvalue        baseline.chisq 
#               344.000                 0.000              7849.813 
#           baseline.df       baseline.pvalue                   cfi 
#               380.000                 0.000                 0.954 
#                   tli                 rmsea        rmsea.ci.lower 
#                 0.949                 0.057                 0.051 
#        rmsea.ci.upper        rmsea.ci.level          rmsea.pvalue 
#                 0.064                 0.900                 0.025 
#        rmsea.close.h0 rmsea.notclose.pvalue     rmsea.notclose.h0 
#                 0.050                 0.000                 0.080 
#                  srmr 
#                 0.094 
# summary(cfa.fit.parallel, fit.measures=TRUE, standardized=TRUE)$fit
#                  npar                  fmin                 chisq 
#               120.000                 0.578               702.820 
#                    df                pvalue        baseline.chisq 
#               380.000                 0.000              7849.813 
#           baseline.df       baseline.pvalue                   cfi 
#               380.000                 0.000                 0.957 
#                   tli                 rmsea        rmsea.ci.lower 
#                 0.957                 0.053                 0.047 
#        rmsea.ci.upper        rmsea.ci.level          rmsea.pvalue 
#                 0.059                 0.900                 0.210 
#        rmsea.close.h0 rmsea.notclose.pvalue     rmsea.notclose.h0 
#                 0.050                 0.000                 0.080 
#                  srmr 
#                 0.093 

sem.model <- "
    # measurement
    depression =~ C1PA63 + C1PA64 + C1PA65 + C1PA66 + C1PA67 + C1PA68 + C1PA69
    harm_avoidance =~ C1SE7V + C1SE7D + C1SE8 + C1SE9
    self_directedness =~ C1SE14O + C1SE14P + C1SE14R
    social_functioning =~ C1SE1BB + C1SE1D + C1SE1HH + C1SE1J + C1SE1P + C1SE1V

    # structural
    social_functioning~harm_avoidance + a*self_directedness
    depression~b*social_functioning + c*self_directedness
    
    IE := a*b
    TE := c + (a*b) 
"

sem.fit <- sem(sem.model, data=data, ordered=TRUE, meanstructure=FALSE,
               estimator="DWLS")

lavInspect(sem.fit, what="std.all")
stop()

lambda = lavInspect(sem.fit, what="std.all")$lambda   # loadings                (n variables, n factors)
theta = lavInspect(sem.fit, what="std.all")$theta     # factor covariances ?    (n factors, n factors)
psi = lavInspect(sem.fit, what="std.all")$psi         # error covariances       (n variables, n variables)
beta = lavInspect(sem.fit, what="std.all")$beta       # regression coefficients (n variables, n factors)
tau = lavInspect(sem.fit, what="std.all")$tau         # thresholds

lambda
#         dprssn hrm_vd slf_dr scl_fn
# C1PA63   0.850  0.000  0.000  0.000
# C1PA64   0.536  0.000  0.000  0.000
# C1PA65   0.168  0.000  0.000  0.000
# C1PA66   0.061  0.000  0.000  0.000
# C1PA67   0.548  0.000  0.000  0.000
# C1PA68   0.757  0.000  0.000  0.000
# C1PA69   0.306  0.000  0.000  0.000
# C1SE7V   0.000  0.602  0.000  0.000
# C1SE7D   0.000  0.724  0.000  0.000
# C1SE8    0.000  0.719  0.000  0.000
# C1SE9    0.000  0.446  0.000  0.000
# C1SE14O  0.000  0.000  0.821  0.000
# C1SE14P  0.000  0.000  0.807  0.000
# C1SE14R  0.000  0.000  0.699  0.000
# C1SE1BB  0.000  0.000  0.000  0.564
# C1SE1D   0.000  0.000  0.000  0.539
# C1SE1HH  0.000  0.000  0.000  0.788
# C1SE1J   0.000  0.000  0.000  0.759
# C1SE1P   0.000  0.000  0.000  0.746
# C1SE1V   0.000  0.000  0.000  0.628

theta
#         C1PA63 C1PA64 C1PA65 C1PA66 C1PA67 C1PA68 C1PA69 C1SE7V C1SE7D C1SE8 C1SE9 C1SE14O C1SE14P C1SE14R C1SE1B C1SE1D C1SE1H C1SE1J C1SE1P C1SE1V
# C1PA63   0.278                                                                                                                                      
# C1PA64   0.000  0.713                                                                                                                               
# C1PA65   0.000  0.000  0.972                                                                                                                        
# C1PA66   0.000  0.000  0.000  0.996                                                                                                                 
# C1PA67   0.000  0.000  0.000  0.000  0.699                                                                                                          
# C1PA68   0.000  0.000  0.000  0.000  0.000  0.428                                                                                                   
# C1PA69   0.000  0.000  0.000  0.000  0.000  0.000  0.906                                                                                            
# C1SE7V   0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.637                                                                                     
# C1SE7D   0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.475                                                                              
# C1SE8    0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000 0.482                                                                        
# C1SE9    0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000 0.000 0.801                                                                  
# C1SE14O  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000 0.000 0.000   0.325                                                          
# C1SE14P  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000 0.000 0.000   0.000   0.349                                                  
# C1SE14R  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000 0.000 0.000   0.000   0.000   0.512                                          
# C1SE1BB  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000 0.000 0.000   0.000   0.000   0.000  0.682                                   
# C1SE1D   0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000 0.000 0.000   0.000   0.000   0.000  0.000  0.709                            
# C1SE1HH  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000 0.000 0.000   0.000   0.000   0.000  0.000  0.000  0.379                     
# C1SE1J   0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000 0.000 0.000   0.000   0.000   0.000  0.000  0.000  0.000  0.425              
# C1SE1P   0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000 0.000 0.000   0.000   0.000   0.000  0.000  0.000  0.000  0.000  0.444       
# C1SE1V   0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000  0.000 0.000 0.000   0.000   0.000   0.000  0.000  0.000  0.000  0.000  0.000  0.606

psi
#                    dprssn hrm_vd slf_dr scl_fn
# depression          0.737                     
# harm_avoidance      0.000  1.000              
# self_directedness   0.000 -0.107  1.000       
# social_functioning  0.000  0.000  0.000  0.690

beta
#                    dprssn hrm_vd slf_dr scl_fn
# depression              0  0.000 -0.427  0.136
# harm_avoidance          0  0.000  0.000  0.000
# self_directedness       0  0.000  0.000  0.000
# social_functioning      0 -0.172 -0.548  0.000

tau
#            thrshl
# C1PA63|t1  -0.816
# C1PA64|t1  -1.369
# C1PA65|t1  -0.170
# C1PA66|t1  -0.569
# C1PA67|t1  -1.052
# C1PA68|t1  -0.340
# C1PA69|t1  -0.306
# C1SE7V|t1  -1.590
# C1SE7V|t2  -0.810
# C1SE7V|t3  -0.362
# C1SE7D|t1  -1.738
# C1SE7D|t2  -0.944
# C1SE7D|t3  -0.560
# C1SE8|t1    0.136
# C1SE9|t1   -0.107
# C1SE14O|t1 -1.412
# C1SE14O|t2 -0.498
# C1SE14O|t3  0.545
# C1SE14P|t1 -1.225
# C1SE14P|t2 -0.389
# C1SE14P|t3  0.634
# C1SE14R|t1 -1.423
# C1SE14R|t2 -0.461
# C1SE14R|t3  0.517
# C1SE1BB|t1 -0.153
# C1SE1BB|t2  0.771
# C1SE1BB|t3  1.252
# C1SE1BB|t4  1.702
# C1SE1BB|t5  2.095
# C1SE1BB|t6  2.479
# C1SE1D|t1  -0.457
# C1SE1D|t2   0.507
# C1SE1D|t3   0.899
# C1SE1D|t4   1.470
# C1SE1D|t5   1.840
# C1SE1D|t6   2.399
# C1SE1HH|t1 -0.480
# C1SE1HH|t2  0.008
# C1SE1HH|t3  0.220
# C1SE1HH|t4  0.367
# C1SE1HH|t5  0.695
# C1SE1HH|t6  1.150
# C1SE1J|t1  -0.733
# C1SE1J|t2  -0.288
# C1SE1J|t3  -0.070
# C1SE1J|t4   0.166
# C1SE1J|t5   0.644
# C1SE1J|t6   1.208
# C1SE1P|t1  -0.589
# C1SE1P|t2  -0.132
# C1SE1P|t3   0.078
# C1SE1P|t4   0.293
# C1SE1P|t5   0.664
# C1SE1P|t6   1.183
# C1SE1V|t1  -0.037
# C1SE1V|t2   0.664
# C1SE1V|t3   1.183
# C1SE1V|t4   1.447
# C1SE1V|t5   1.684
# C1SE1V|t6   1.995

summary(sem.fit, standardized=TRUE, fit.measures=TRUE)
# lavaan 0.6.14 ended normally after 40 iterations
# 
#   Estimator                                       DWLS
#   Optimization method                           NLMINB
#   Number of model parameters                        85
# 
#   Number of observations                           608
# 
# Model Test User Model:
#                                                       
#   Test statistic                               505.233
#   Degrees of freedom                               165
#   P-value (Chi-square)                           0.000
# 
# Model Test Baseline Model:
# 
#   Test statistic                              7312.549
#   Degrees of freedom                               190
#   P-value                                        0.000
# 
# User Model versus Baseline Model:
# 
#   Comparative Fit Index (CFI)                    0.952
#   Tucker-Lewis Index (TLI)                       0.945
# 
# Root Mean Square Error of Approximation:
# 
#   RMSEA                                          0.058
#   90 Percent confidence interval - lower         0.053
#   90 Percent confidence interval - upper         0.064
#   P-value H_0: RMSEA <= 0.050                    0.009
#   P-value H_0: RMSEA >= 0.080                    0.000
# 
# Standardized Root Mean Square Residual:
# 
#   SRMR                                           0.081
# 
# Parameter Estimates:
# 
#   Standard errors                             Standard
#   Information                                 Expected
#   Information saturated (h1) model        Unstructured
# 
# Latent Variables:
#                         Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#   depression =~                                                              
#     C1PA63                 1.000                               0.850    0.850
#     C1PA64                 0.631    0.073    8.662    0.000    0.536    0.536
#     C1PA65                 0.198    0.047    4.236    0.000    0.168    0.168
#     C1PA66                 0.071    0.048    1.492    0.136    0.061    0.061
#     C1PA67                 0.646    0.066    9.757    0.000    0.548    0.548
#     C1PA68                 0.890    0.078   11.460    0.000    0.757    0.757
#     C1PA69                 0.360    0.051    7.005    0.000    0.306    0.306
#   harm_avoidance =~                                                          
#     C1SE7V                 1.000                               0.602    0.602
#     C1SE7D                 1.203    0.160    7.504    0.000    0.724    0.724
#     C1SE8                  1.194    0.155    7.718    0.000    0.719    0.719
#     C1SE9                  0.740    0.106    7.004    0.000    0.446    0.446
#   self_directedness =~                                                       
#     C1SE14O                1.000                               0.821    0.821
#     C1SE14P                0.982    0.049   19.988    0.000    0.807    0.807
#     C1SE14R                0.851    0.043   19.704    0.000    0.699    0.699
#   social_functioning =~                                                      
#     C1SE1BB                1.000                               0.564    0.564
#     C1SE1D                 0.956    0.056   17.145    0.000    0.539    0.539
#     C1SE1HH                1.398    0.066   21.183    0.000    0.788    0.788
#     C1SE1J                 1.345    0.064   21.088    0.000    0.759    0.759
#     C1SE1P                 1.323    0.064   20.568    0.000    0.746    0.746
#     C1SE1V                 1.113    0.060   18.606    0.000    0.628    0.628
# 
# Regressions:
#                        Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#   social_functioning ~                                                      
#     harm_avdnc           -0.161    0.033   -4.824    0.000   -0.172   -0.172
#     slf_drctdn (a)       -0.376    0.024  -15.948    0.000   -0.548   -0.548
#   depression ~                                                              
#     scl_fnctnn (b)        0.206    0.064    3.214    0.001    0.136    0.136
#     slf_drctdn (c)       -0.442    0.056   -7.956    0.000   -0.427   -0.427
# 
# Covariances:
#                     Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#   harm_avoidance ~~                                                      
#     self_dirctdnss    -0.053    0.016   -3.400    0.001   -0.107   -0.107
# 
# Intercepts:
#                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#    .C1PA63            0.000                               0.000    0.000
#    .C1PA64            0.000                               0.000    0.000
#    .C1PA65            0.000                               0.000    0.000
#    .C1PA66            0.000                               0.000    0.000
#    .C1PA67            0.000                               0.000    0.000
#    .C1PA68            0.000                               0.000    0.000
#    .C1PA69            0.000                               0.000    0.000
#    .C1SE7V            0.000                               0.000    0.000
#    .C1SE7D            0.000                               0.000    0.000
#    .C1SE8             0.000                               0.000    0.000
#    .C1SE9             0.000                               0.000    0.000
#    .C1SE14O           0.000                               0.000    0.000
#    .C1SE14P           0.000                               0.000    0.000
#    .C1SE14R           0.000                               0.000    0.000
#    .C1SE1BB           0.000                               0.000    0.000
#    .C1SE1D            0.000                               0.000    0.000
#    .C1SE1HH           0.000                               0.000    0.000
#    .C1SE1J            0.000                               0.000    0.000
#    .C1SE1P            0.000                               0.000    0.000
#    .C1SE1V            0.000                               0.000    0.000
#    .depression        0.000                               0.000    0.000
#     harm_avoidance    0.000                               0.000    0.000
#     self_dirctdnss    0.000                               0.000    0.000
#    .social_fnctnng    0.000                               0.000    0.000
# 
# Thresholds:
#                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#     C1PA63|t1        -0.816    0.058  -14.184    0.000   -0.816   -0.816
#     C1PA64|t1        -1.369    0.073  -18.852    0.000   -1.369   -1.369
#     C1PA65|t1        -0.170    0.051   -3.321    0.001   -0.170   -0.170
#     C1PA66|t1        -0.569    0.054  -10.548    0.000   -0.569   -0.569
#     C1PA67|t1        -1.052    0.063  -16.820    0.000   -1.052   -1.052
#     C1PA68|t1        -0.340    0.052   -6.552    0.000   -0.340   -0.340
#     C1PA69|t1        -0.306    0.052   -5.907    0.000   -0.306   -0.306
#     C1SE7V|t1        -1.590    0.083  -19.216    0.000   -1.590   -1.590
#     C1SE7V|t2        -0.810    0.057  -14.109    0.000   -0.810   -0.810
#     C1SE7V|t3        -0.362    0.052   -6.954    0.000   -0.362   -0.362
#     C1SE7D|t1        -1.738    0.091  -19.002    0.000   -1.738   -1.738
#     C1SE7D|t2        -0.944    0.060  -15.721    0.000   -0.944   -0.944
#     C1SE7D|t3        -0.560    0.054  -10.390    0.000   -0.560   -0.560
#     C1SE8|t1          0.136    0.051    2.674    0.008    0.136    0.136
#     C1SE9|t1         -0.107    0.051   -2.107    0.035   -0.107   -0.107
#     C1SE14O|t1       -1.412    0.074  -18.990    0.000   -1.412   -1.412
#     C1SE14O|t2       -0.498    0.053   -9.357    0.000   -0.498   -0.498
#     C1SE14O|t3        0.545    0.054   10.152    0.000    0.545    0.545
#     C1SE14P|t1       -1.225    0.068  -18.154    0.000   -1.225   -1.225
#     C1SE14P|t2       -0.389    0.052   -7.436    0.000   -0.389   -0.389
#     C1SE14P|t3        0.634    0.055   11.571    0.000    0.634    0.634
#     C1SE14R|t1       -1.423    0.075  -19.021    0.000   -1.423   -1.423
#     C1SE14R|t2       -0.461    0.053   -8.719    0.000   -0.461   -0.461
#     C1SE14R|t3        0.517    0.053    9.675    0.000    0.517    0.517
#     C1SE1BB|t1       -0.153    0.051   -2.998    0.003   -0.153   -0.153
#     C1SE1BB|t2        0.771    0.057   13.580    0.000    0.771    0.771
#     C1SE1BB|t3        1.252    0.068   18.311    0.000    1.252    1.252
#     C1SE1BB|t4        1.702    0.089   19.086    0.000    1.702    1.702
#     C1SE1BB|t5        2.095    0.122   17.217    0.000    2.095    2.095
#     C1SE1BB|t6        2.479    0.178   13.940    0.000    2.479    2.479
#     C1SE1D|t1        -0.457    0.053   -8.639    0.000   -0.457   -0.457
#     C1SE1D|t2         0.507    0.053    9.516    0.000    0.507    0.507
#     C1SE1D|t3         0.899    0.059   15.218    0.000    0.899    0.899
#     C1SE1D|t4         1.470    0.077   19.124    0.000    1.470    1.470
#     C1SE1D|t5         1.840    0.099   18.661    0.000    1.840    1.840
#     C1SE1D|t6         2.399    0.163   14.696    0.000    2.399    2.399
#     C1SE1HH|t1       -0.480    0.053   -9.038    0.000   -0.480   -0.480
#     C1SE1HH|t2        0.008    0.051    0.162    0.871    0.008    0.008
#     C1SE1HH|t3        0.220    0.051    4.292    0.000    0.220    0.220
#     C1SE1HH|t4        0.367    0.052    7.035    0.000    0.367    0.367
#     C1SE1HH|t5        0.695    0.056   12.506    0.000    0.695    0.695
#     C1SE1HH|t6        1.150    0.065   17.641    0.000    1.150    1.150
#     C1SE1J|t1        -0.733    0.056  -13.045    0.000   -0.733   -0.733
#     C1SE1J|t2        -0.288    0.052   -5.584    0.000   -0.288   -0.288
#     C1SE1J|t3        -0.070    0.051   -1.378    0.168   -0.070   -0.070
#     C1SE1J|t4         0.166    0.051    3.240    0.001    0.166    0.166
#     C1SE1J|t5         0.644    0.055   11.727    0.000    0.644    0.644
#     C1SE1J|t6         1.208    0.067   18.045    0.000    1.208    1.208
#     C1SE1P|t1        -0.589    0.054  -10.864    0.000   -0.589   -0.589
#     C1SE1P|t2        -0.132    0.051   -2.593    0.010   -0.132   -0.132
#     C1SE1P|t3         0.078    0.051    1.540    0.124    0.078    0.078
#     C1SE1P|t4         0.293    0.052    5.665    0.000    0.293    0.293
#     C1SE1P|t5         0.664    0.055   12.040    0.000    0.664    0.664
#     C1SE1P|t6         1.183    0.066   17.876    0.000    1.183    1.183
#     C1SE1V|t1        -0.037    0.051   -0.729    0.466   -0.037   -0.037
#     C1SE1V|t2         0.664    0.055   12.040    0.000    0.664    0.664
#     C1SE1V|t3         1.183    0.066   17.876    0.000    1.183    1.183
#     C1SE1V|t4         1.447    0.076   19.077    0.000    1.447    1.447
#     C1SE1V|t5         1.684    0.088   19.119    0.000    1.684    1.684
#     C1SE1V|t6         1.995    0.112   17.873    0.000    1.995    1.995
# 
# Variances:
#                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#    .C1PA63            0.278                               0.278    0.278
#    .C1PA64            0.713                               0.713    0.713
#    .C1PA65            0.972                               0.972    0.972
#    .C1PA66            0.996                               0.996    0.996
#    .C1PA67            0.699                               0.699    0.699
#    .C1PA68            0.428                               0.428    0.428
#    .C1PA69            0.906                               0.906    0.906
#    .C1SE7V            0.637                               0.637    0.637
#    .C1SE7D            0.475                               0.475    0.475
#    .C1SE8             0.482                               0.482    0.482
#    .C1SE9             0.801                               0.801    0.801
#    .C1SE14O           0.325                               0.325    0.325
#    .C1SE14P           0.349                               0.349    0.349
#    .C1SE14R           0.512                               0.512    0.512
#    .C1SE1BB           0.682                               0.682    0.682
#    .C1SE1D            0.709                               0.709    0.709
#    .C1SE1HH           0.379                               0.379    0.379
#    .C1SE1J            0.425                               0.425    0.425
#    .C1SE1P            0.444                               0.444    0.444
#    .C1SE1V            0.606                               0.606    0.606
#    .depression        0.532    0.067    7.966    0.000    0.737    0.737
#     harm_avoidance    0.363    0.061    5.985    0.000    1.000    1.000
#     self_dirctdnss    0.675    0.040   16.747    0.000    1.000    1.000
#    .social_fnctnng    0.219    0.019   11.373    0.000    0.690    0.690
# 
# Scales y*:
#                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#     C1PA63            1.000                               1.000    1.000
#     C1PA64            1.000                               1.000    1.000
#     C1PA65            1.000                               1.000    1.000
#     C1PA66            1.000                               1.000    1.000
#     C1PA67            1.000                               1.000    1.000
#     C1PA68            1.000                               1.000    1.000
#     C1PA69            1.000                               1.000    1.000
#     C1SE7V            1.000                               1.000    1.000
#     C1SE7D            1.000                               1.000    1.000
#     C1SE8             1.000                               1.000    1.000
#     C1SE9             1.000                               1.000    1.000
#     C1SE14O           1.000                               1.000    1.000
#     C1SE14P           1.000                               1.000    1.000
#     C1SE14R           1.000                               1.000    1.000
#     C1SE1BB           1.000                               1.000    1.000
#     C1SE1D            1.000                               1.000    1.000
#     C1SE1HH           1.000                               1.000    1.000
#     C1SE1J            1.000                               1.000    1.000
#     C1SE1P            1.000                               1.000    1.000
#     C1SE1V            1.000                               1.000    1.000
# 
# Defined Parameters:
#                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
#     IE               -0.077    0.023   -3.303    0.001   -0.075   -0.075
#     TE               -0.519    0.044  -11.697    0.000   -0.502   -0.502
modindices(sem.fit, sort=TRUE, maximum.number=20)
#                    lhs op     rhs     mi    epc sepc.lv sepc.all sepc.nox
# 391            C1SE1BB ~~  C1SE1D 90.619  0.329   0.329    0.473    0.473
# 371            C1SE14O ~~ C1SE14R 44.130  0.317   0.317    0.778    0.778
# 214 social_functioning =~ C1SE14P 36.950 -0.551  -0.311   -0.311   -0.311
# 392            C1SE1BB ~~ C1SE1HH 34.474 -0.290  -0.290   -0.570   -0.570
# 204 social_functioning =~  C1PA65 29.688 -0.415  -0.234   -0.234   -0.234
# 205 social_functioning =~  C1PA66 26.101 -0.403  -0.227   -0.227   -0.227
# 213 social_functioning =~ C1SE14O 22.440  0.463   0.261    0.261    0.261
# 164         depression =~  C1SE1D 20.018 -0.253  -0.215   -0.215   -0.215
# 209 social_functioning =~  C1SE7V 19.761  0.255   0.144    0.144    0.144
# 400            C1SE1HH ~~  C1SE1J 19.647  0.170   0.170    0.424    0.424
# 393            C1SE1BB ~~  C1SE1J 18.669 -0.207  -0.207   -0.385   -0.385
# 378            C1SE14P ~~ C1SE14R 17.649 -0.213  -0.213   -0.505   -0.505
# 172     harm_avoidance =~  C1PA66 16.804  0.347   0.209    0.209    0.209
# 207 social_functioning =~  C1PA68 16.614  0.408   0.230    0.230    0.230
# 401            C1SE1HH ~~  C1SE1P 15.907  0.154   0.154    0.377    0.377
# 190  self_directedness =~  C1PA68 14.881 -0.399  -0.328   -0.328   -0.328
# 187  self_directedness =~  C1PA65 14.420  0.278   0.229    0.229    0.229
# 206 social_functioning =~  C1PA67 14.077 -0.351  -0.198   -0.198   -0.198
# 177     harm_avoidance =~ C1SE14P 13.430  0.307   0.185    0.185    0.185
# 272             C1PA66 ~~  C1PA69 13.113  0.241   0.241    0.253    0.253

jpeg(file="visualizations/model.png", width=30, height=30, units="cm", res=400)
semPaths(sem.fit, what="diagram", whatLabels="stand", layout="tree2", rotation=2,
         sizeMan=5, sizeMan2=3, sizeLat=8, sizeLat2=4, intercepts=FALSE,
         edge.color="black", thresholds=FALSE, label.scale=TRUE, asize=1.5,
         edge.label.cex=0.5, label.cex=1)
dev.off()
