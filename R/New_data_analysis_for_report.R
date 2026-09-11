## Data example code for report.
## Date: 24-05-2026
## Last edited: 29-05-2026
## By         : Sebastian Kvist
##### ** Load package + dependencies ** #####
# library("usethis")
# library("devtools")
library("DescTools")
library("iarm")
library("coin")
library("stats")
devtools::install_github("SebastianKvist99/twigg")
# library("twigg")


##### ** Load data ** #####
path <- "/Users/sebastiankvist/Desktop/PREP"
path.data <- file.path(path, "SPADI.csv")
data <- read.csv(path.data)
# head(data)

i.list.d <- paste0("D", 1:8)
i.list.p <- paste0("P", 1:5)
exo.list <- c("gender", "over60")

##### ** Inspect data ** #####
# print(nrow(data))
# [1] 229
# print(sum(stats::complete.cases(data)))
# [1] 213
# print(sum(stats::complete.cases(data[i.list])))
# [1] 213
# item.cc <- stats::complete.cases(data[,i.list, drop = FALSE])
# item.cc.data <- data[item.cc, ]
# for (x in exo.list){
#  print(sum(is.na(item.cc.data[,x])))
# }
# [1] 0
# [1] 0
cc.id <- complete.cases(data)
cc.data <- data[cc.id,]

##### ** split into pain and disability ** #####
data.d <- cc.data[c(exo.list, i.list.d)]
#head(d.data)
data.p <- cc.data[c(exo.list, i.list.p)]
#head(p.data)

##### ** Step 1 ** #####
### ** disability
step1.d <- screen_items(data.d, i.list.d, exo.list)
print(step1.d$M1)
print(step1.d$M2)
print(step1.d$M3)

### ** pain
step1.p <- screen_items(data.p, i.list.p, exo.list)
print(step1.p$M1)
print(step1.p$M2)
print(step1.p$M3)

##### ** Step 2 ** #####
### ** disability
step2.dif.d <- screen_DIF(data.d, i.list.d, exo.list)
# No DIF in disability subscale
step2.ld.d <- screen_LD(data.d, i.list.d)

## Dont run. Wont make sense as the DIF list is empty.
# s.d.list.d <- s.d_list(step2.dif.d, i.list.d, exo.list)
# s.list.d <- s.d.list.d$SOURCE
# d.list.d <- s.d.list.d$DIF

### ** pain
step2.dif.p <- screen_DIF(data.p, i.list.p, exo.list)
step2.ld.p <- screen_LD(data.p, i.list.p)
s.d.list.p <- s.d_list(step2.dif.p, i.list.p, exo.list)
s.list.p <- s.d.list.p$SOURCE
d.list.p <- s.d.list.p$DIF

##### ** Step 3 ** #####
##### **  3a  ** #####
### ** disability
gen.ld.d <- genuine_LD(step2.ld.d)

### ** pain
gen.ld.p <- genuine_LD(step2.ld.p)

##### **  3b  ** #####
### ** disability does not make sense to run as no DIF was detected
### ** pain
pain.3b <- run_step3b(data.p, s.list.p, i.list.p,
                      p_value_method = "asymptotic")

##### **  3c  ** #####
### ** disability does not make sense to run as no DIF was detected
### ** pain
pain.3c <- run_step3c(data.p, d.list.p, i.list.p,
                      p_value_method = "asymptotic")

##### ** Combine step3bc pain  ** #####
pain.3bc <- combine_step3bc(pain.3b, pain.3c, s.list.p, d.list.p)

##### ** Step 4 ** #####
## ** disability
s4.d <- step4_structure_screen(data.d, i.list.d, exo.list)
print(s4.d)

## ** pain
s4.p <- step4_structure_screen(data.p, i.list.p, exo.list)
print(s4.p)

##### ** Step 5 ** #####
## ** disability
s5.d <- build_gllrm_graph(i.list.d, exo.list,
                          ld = gen.ld.d,
                          step4 = s4.d)
s5.d.irt <- build_irt_graph(s5.d)
s5.d.irt$edges
s5.d.m <- build_moralized_graph(s5.d, score_node = "S")
s5.d.m$edges

## ** pain
s5.p <- build_gllrm_graph(i.list.p, exo.list,
                          ld = gen.ld.p,
                          dif = pain.3bc,
                          step4 = s4.p)
s5.p.irt <- build_irt_graph(s5.p)
s5.p.irt$edges
s5.p.m <- build_moralized_graph(s5.p, score_node = "S")
s5.p.m$edges

##### ** Step 6 ** #####
## ** disability
s6.d <- step6_check_gllrm(data.d, s5.d)
s6.d$moralized_graph
s6.d$moralized_graph$edges

## ** pain
s6.p <- step6_check_gllrm(data.p, s5.p)
s6.p$moralized_graph
s6.p$moralized_graph$edges



