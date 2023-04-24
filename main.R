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
table(data$C1PRSEX)
cfa.model <- "
    depression =~ NA*C1PA63 + C1PA64 + C1PA65 + C1PA66 + C1PA67 + C1PA68 + C1PA69
    harm_avoidance =~ NA*C1SE7V + C1SE7D + C1SE8 + C1SE9
    self_directedness =~ NA*C1SE14O + C1SE14P + C1SE14R
    social_functioning =~ NA*C1SE1BB + C1SE1D + C1SE1HH + C1SE1J + C1SE1P + C1SE1V
    depression ~~ 1*depression
    harm_avoidance ~~ 1*harm_avoidance
    self_directedness ~~ 1*self_directedness
    social_functioning ~~ 1*social_functioning
"
cfa.fit.conf <- cfa(cfa.model, data=data, ordered=TRUE, estimator="DWLS",
                    group="C1PRSEX", meanstructure=TRUE)
cfa.fit.tau <- cfa(cfa.model, data=data, ordered=TRUE, estimator="DWLS",
                   group="C1PRSEX", group.equal=c("loadings"), meanstructure=TRUE)
cfa.fit.parallel <- cfa(cfa.model, data=data, ordered=TRUE, estimator="DWLS",
                        group="C1PRSEX", group.equal=c("loadings", "intercepts"), meanstructure=TRUE)

lavTestLRT(cfa.fit.conf, cfa.fit.tau)
# Chi-Squared Difference Test
#               Df AIC BIC  Chisq Chisq diff    RMSEA Df diff Pr(>Chisq)    
# cfa.fit.conf 328         651.13                                           
# cfa.fit.tau  348         703.46     52.336 0.072928      20  0.0001017 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
lavTestLRT(cfa.fit.tau, cfa.fit.parallel)
#            Chi-Squared Difference Test
#                   Df AIC BIC  Chisq Chisq diff RMSEA Df diff Pr(>Chisq)
# cfa.fit.tau      348         703.46                                    
# cfa.fit.parallel 384         727.41     23.946     0      36     0.9381

summary(cfa.fit.conf, fit.measures=TRUE, standardized=TRUE)$fit
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
summary(cfa.fit.tau, fit.measures=TRUE, standardized=TRUE)$fit
#                  npar                  fmin                 chisq 
#               152.000                 0.579               703.462 
#                    df                pvalue        baseline.chisq 
#               348.000                 0.000              7769.046 
#           baseline.df       baseline.pvalue                   cfi 
#               380.000                 0.000                 0.952 
#                   tli                 rmsea        rmsea.ci.lower 
#                 0.947                 0.058                 0.052 
#        rmsea.ci.upper        rmsea.ci.level          rmsea.pvalue 
#                 0.064                 0.900                 0.017 
#        rmsea.close.h0 rmsea.notclose.pvalue     rmsea.notclose.h0 
#                 0.050                 0.000                 0.080 
#                  srmr 
#                 0.092 
summary(cfa.fit.parallel, fit.measures=TRUE, standardized=TRUE)$fit
#                  npar                  fmin                 chisq 
#               116.000                 0.598               727.408 
#                    df                pvalue        baseline.chisq 
#               384.000                 0.000              7769.046 
#           baseline.df       baseline.pvalue                   cfi 
#               380.000                 0.000                 0.954 
#                   tli                 rmsea        rmsea.ci.lower 
#                 0.954                 0.054                 0.048 
#        rmsea.ci.upper        rmsea.ci.level          rmsea.pvalue 
#                 0.060                 0.900                 0.117 
#        rmsea.close.h0 rmsea.notclose.pvalue     rmsea.notclose.h0 
#                 0.050                 0.000                 0.080 
#                  srmr 
#                 0.092 

# --- Invariance of age
cat("AGE\n")
table(data$C1PRAGE)
cfa.fit.conf <- cfa(cfa.model, data=data, ordered=TRUE, meanstructure=FALSE,
               estimator="DWLS", group="C1PRAGE")
cfa.fit.tau <- cfa(cfa.model, data=data, ordered=TRUE, meanstructure=FALSE,
                   estimator="DWLS", group="C1PRAGE", group.equal=c("loadings"))
cfa.fit.parallel <- cfa(cfa.model, data=data, ordered=TRUE, meanstructure=FALSE,
                        estimator="DWLS", group="C1PRAGE",
                        group.equal=c("loadings", "intercepts"))
lavTestLRT(cfa.fit.conf, cfa.fit.tau)
lavTestLRT(cfa.fit.tau, cfa.fit.parallel)

summary(cfa.fit.conf, fit.measures=TRUE, standardized=TRUE)$fit
summary(cfa.fit.tau, fit.measures=TRUE, standardized=TRUE)$fit
summary(cfa.fit.parallel, fit.measures=TRUE, standardized=TRUE)$fit

# --- Longitudinal invariance
cat("TIME\n")
table(data$source)
cfa.fit.conf <- cfa(cfa.model, data=data, ordered=TRUE, estimator="DWLS",
                    group="source")
cfa.fit.tau <- cfa(cfa.model, data=data, ordered=TRUE, estimator="DWLS",
                   group="source", group.equal=c("loadings"))
cfa.fit.parallel <- cfa(cfa.model, data=data, ordered=TRUE, estimator="DWLS",
                        group="source", group.equal=c("loadings", "intercepts"))

lavTestLRT(cfa.fit.conf, cfa.fit.tau)
# Chi-Squared Difference Test
#               Df AIC BIC  Chisq Chisq diff    RMSEA Df diff Pr(>Chisq)   
# cfa.fit.conf 328         653.66                                          
# cfa.fit.tau  348         696.43     42.762 0.061186      20   0.002198 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
lavTestLRT(cfa.fit.tau, cfa.fit.parallel)
# Chi-Squared Difference Test
# 
#                   Df AIC BIC  Chisq Chisq diff RMSEA Df diff Pr(>Chisq)
# cfa.fit.tau      348         696.43                                    
# cfa.fit.parallel 384         717.76      21.33     0      36     0.9751

summary(cfa.fit.conf, fit.measures=TRUE, standardized=TRUE)$fit
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
summary(cfa.fit.tau, fit.measures=TRUE, standardized=TRUE)$fit
#                  npar                  fmin                 chisq 
#               152.000                 0.573               696.426 
#                    df                pvalue        baseline.chisq 
#               348.000                 0.000              7849.813 
#           baseline.df       baseline.pvalue                   cfi 
#               380.000                 0.000                 0.953 
#                   tli                 rmsea        rmsea.ci.lower 
#                 0.949                 0.057                 0.051 
#        rmsea.ci.upper        rmsea.ci.level          rmsea.pvalue 
#                 0.064                 0.900                 0.025 
#        rmsea.close.h0 rmsea.notclose.pvalue     rmsea.notclose.h0 
#                 0.050                 0.000                 0.080 
#                  srmr 
#                 0.094 
summary(cfa.fit.parallel, fit.measures=TRUE, standardized=TRUE)$fit
#                  npar                  fmin                 chisq 
#               116.000                 0.590               717.755 
#                    df                pvalue        baseline.chisq 
#               384.000                 0.000              7849.813 
#           baseline.df       baseline.pvalue                   cfi 
#               380.000                 0.000                 0.955 
#                   tli                 rmsea        rmsea.ci.lower 
#                 0.956                 0.054                 0.047 
#        rmsea.ci.upper        rmsea.ci.level          rmsea.pvalue 
#                 0.060                 0.900                 0.164 
#        rmsea.close.h0 rmsea.notclose.pvalue     rmsea.notclose.h0 
#                 0.050                 0.000                 0.080 
#                  srmr 
#                 0.093 

stop()

# ------------------------------------------------------------------------------

base.model <- "
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

base.fit <- sem(base.model, data=data, ordered=TRUE, meanstructure=FALSE,
                estimator="DWLS")

# str(base.fit)
# base.fit$ lambda: num [1:20, 1:4] 1 0.6308 0.1975 0.0714 0.6455 ...
# base.fit$ theta : num [1:20, 1:20] 0.278 0 0 0 0 ...
# base.fit$ psi   : num [1:4, 1:4] 0.532 0 0 0 0 ...
# base.fit$ beta  : num [1:4, 1:4] 0 0 0 0 0 ...
# base.fit$ nu    : num [1:20, 1] 0 0 0 0 0 0 0 0 0 0 ...
# base.fit$ alpha : num [1:4, 1] 0 0 0 0
# base.fit$ tau   : num [1:60, 1] -0.816 -1.369 -0.17 -0.569 -1.052 ...
# base.fit$ delta : num [1:20, 1] 1 1 1 1 1 1 1 1 1 1 ...

summary(base.fit, standardized=TRUE, fit.measures=TRUE)
modindices(base.fit, sort=TRUE, maximum.number=20)
jpeg(file="visualizations/base_model.png", width=30, height=30, units="cm", res=400)
semPaths(base.fit, what="diagram", whatLabels="stand", layout="tree2", rotation=2,
         sizeMan=5, sizeMan2=3, sizeLat=8, sizeLat2=4, intercepts=FALSE,
         edge.color="black", thresholds=FALSE, label.scale=TRUE, asize=1.5,
         edge.label.cex=0.5, label.cex=1)
dev.off()

# improved.model <- "
#     # measurement
#     depression =~ C1PA63 + C1PA64 + C1PA65 + C1PA66 + C1PA67 + C1PA68 + C1PA69
#     harm_avoidance =~ C1SE7V + C1SE7D + C1SE8 + C1SE9
#     self_directedness =~ C1SE14O + C1SE14P + C1SE14R
#     social_functioning =~ C1SE1BB + C1SE1D + C1SE1HH + C1SE1J + C1SE1P + C1SE1V
# 
#     # structural
#     social_functioning~harm_avoidance + self_directedness
#     depression~social_functioning
# "
# 
# improved.fit <- sem(improved.model, data=data, ordered=TRUE, meanstructure=FALSE,
#                 estimator="DWLS")
# summary(improved.fit, standardized=TRUE, fit.measures=TRUE)