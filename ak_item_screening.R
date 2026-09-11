## Item screening of the AK (actinic keratosis / AKQoL) data set using the
## "twigg" package, following the six-step strategy of Kreiner & Christensen
## (2011) (see tab:item_screening_guide):
##
##   Step 1  Analysis of consistency
##   Step 2  Analysis of DIF and LD
##   Step 3  (a) Elimination of spurious evidence of LD
##           (b) Elimination of spurious evidence for items with more than
##               one DIF source
##           (c) Elimination of spurious evidence for sources with an
##               apparent DIF effect on more than one item
##   Step 4  Analysis of the association between the score and exogenous
##           covariates
##   Step 5  Definition of a GLLRM and analysis of the global Markov
##           properties of G_Rasch
##   Step 6  Tests of global Markov properties of the GLLRM
##
## Data: ak.csv (99 respondents, AKQoL items Q1-Q10, covariates woman/Alder).
## See ak_data_description.md for details on the data set.

##### ** Load package + dependencies ** #####
library("DescTools")
library("iarm")
library("coin")
library("stats")
# devtools::install_github("SebastianKvist99/twigg")
library("twigg")

##### ** Load data ** #####
data <- read.csv("ak.csv")

i.list <- paste0("Q", 1:10)
exo.list <- c("woman", "Alder")

##### ** Inspect data ** #####
cc.id <- complete.cases(data[, c(i.list, exo.list)])
cat("Complete cases:", sum(cc.id), "of", nrow(data), "\n")
cc.data <- data[cc.id, ]

##### ** Step 1: analysis of consistency ** #####
step1 <- screen_items(cc.data, i.list, exo.list)
print(step1$M1)
print(step1$M2)
print(step1$M3)

##### ** Step 2: analysis of DIF and LD ** #####
step2.dif <- screen_DIF(cc.data, i.list, exo.list)
step2.ld <- screen_LD(cc.data, i.list)

s.d.list <- s.d_list(step2.dif, i.list, exo.list)
s.list <- s.d.list$SOURCE
d.list <- s.d.list$DIF
has.dif <- any(lengths(s.list) > 0)

##### ** Step 3(a): elimination of spurious evidence of LD ** #####
gen.ld <- genuine_LD(step2.ld)

##### ** Step 3(b)/(c): elimination of spurious DIF evidence ** #####
if (has.dif) {
  step3b <- run_step3b(cc.data, s.list, i.list, p_value_method = "asymptotic")
  step3c <- run_step3c(cc.data, d.list, i.list, p_value_method = "asymptotic")
  step3bc <- combine_step3bc(step3b, step3c, s.list, d.list)
  print(step3bc$table)
} else {
  step3bc <- NULL
  cat("No DIF detected in Step 2 - Step 3(b)/(c) skipped.\n")
}

##### ** Step 4: score/covariate association ** #####
step4 <- step4_structure_screen(cc.data, i.list, exo.list)
print(step4)

##### ** Step 5: GLLRM definition and global Markov properties of G_Rasch ** #####
step5 <- build_gllrm_graph(i.list, exo.list,
                            ld = gen.ld,
                            dif = step3bc,
                            step4 = step4)
step5.irt <- build_irt_graph(step5)
print(step5.irt$edges)
step5.moral <- build_moralized_graph(step5, score_node = "S")
print(step5.moral$edges)

##### ** Step 6: tests of global Markov properties of the GLLRM ** #####
step6 <- step6_check_gllrm(cc.data, step5)
print(step6$moralized_graph)
print(step6$moralized_graph$edges)
