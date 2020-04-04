# Modeling the effects of Climate and Herbivory on Shrub Growth
# Peter Frank 
# peterfr@stud.ntnu.no
# 2020-03-04

# Our objective is to test the following three hypotheses:

#H1 - Increased temperature and precipitation will have a positive effect on radial growth 
#H2 - Increased herbivore density will have a negative effect on radial growth 
#H3 - Shrub growth positivly effected by the interactive effects of herbivory and temperature

# MODEL SELECTION THROUGH MODEL AVERAGING ####

# Betula Model Averaging ####

anova.lme(lme_global_b)

summary(lme_global_b)

intervals(lme_global_b)

r.squaredLR(lme_global_b)

plot(lme_global_b)

qqnorm(resid(lme_global_b))

qqline(resid(lme_global_b))

dredge_b = dredge(lme_global_b)

model.sel_b = model.sel(dredge_b, fit = TRUE)

write.table(model.sel_b, "/Users/peterfrank/Documents/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/model.sel_b.txt", sep = "\t")

mod.avg_b = model.avg(model.sel_b)

importance(mod.avg_b)

pred_avg_b = predict(mod.avg_b)

summary(mod.avg_b)

summary(pred_avg_b)

plot(model.sel_b)

# Salix Model Averaging ####

summary(lme_global_s)

r.squaredLR(lme_global_s)

plot(lme_global_s)

qqnorm(resid(lme_global_s))

qqline(resid(lme_global_s))

dredge_s = dredge(lme_global_s)

model.sel_s = model.sel(dredge_s)

write.table(model.sel_s, "/Users/peterfrank/Documents/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/model.sel_s.txt", sep = "\t")

mod.avg_s = model.avg(model.sel_s)

importance(mod.avg_s)

summary(mod.avg_s)

model.importance.plot(model.sel_s)
