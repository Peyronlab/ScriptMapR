################ example script################
library(survival)
setwd("./")

data(lung)

### number of patients or samples
nb_samples = dim(lung)[1]


### rename data to find gender first extract gender// 2 ways to do it
gender = lung$sex  #best
gender = lung[, 5]

# find indices with simple which test
males_indices = which(gender == "1")
female_indices = which(gender == "2")


# replace number by gender using indices
gender[males_indices] = "Male"
gender[female_indices] = "Female"

### finding how many deaths
deaths = sum(lung$status == 2)

inst = lung$inst
inst = lung[, 1]

### extract each column into distinct variables

time = lung$time
status = lung$status
age = lung$age
sex = lung$sex
ph.ecog = lung$ph.ecog
ph.karno = lung$ph.karno
pat.karno = lung$pat.karno
meal.cal = lung$meal.cal
wt.loss = lung$wt.loss

attach(lung)


# binarize age
old_indices = which(age > 50)
old = rep(0, nb_samples)
old[old_indices] = 1


### survival model
formula = survival::Surv(lung$time, lung$status)


fit <- survival::survfit(formula ~ old, data = data.frame(lung))

fit_table <- survival::coxph(formula ~ old)
info = summary(fit_table)


# extract pvalue
pval = info$sctest[3]

## graphical representation
plot(fit, col = c("blue", "red"), lty = c(1, 2))

if (pval < 0.05) {
    reject = FALSE
} else {
    reject = TRUE
    a = "OK"
}


# simple loop example
count = 0
for (j in old[old_indices]) {
    count = count + 1
}

