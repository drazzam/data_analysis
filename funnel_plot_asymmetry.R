result_rma= rma(
     yi = dataset$effect_size,
     sei = dataset$se_effect_size,
     method = "REML"
 )
result_egger= regtest(
     x = result_rma,
     model = "lm",
     predictor = "sei"
 )
