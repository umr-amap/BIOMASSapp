library(BIOMASS)

# Add ~ 40 simulated height per plot (4 plots)

data("NouraguesTrees")
data("NouraguesHD")

hd_model = modelHD(
  D = NouraguesHD$D, H = NouraguesHD$H,
  useWeight = FALSE, drawGraph = T, method = "log2")

H_model <- retrieveH(
  D = NouraguesTrees$D,
  model = hd_model)

set.seed(3)
H_model$H <- H_model$H + rnorm(nrow(NouraguesTrees), 0, H_model$RSE)

H_model$H[H_model$H<8] <- NA

NouraguesTrees$H <- H_model$H

NouraguesTrees <- do.call(rbind, lapply( split(NouraguesTrees , NouraguesTrees$Plot) , function(dat) {
  dat$H[sample(1:nrow(dat), size = (nrow(dat)-40))] <- NA
  dat
}))

modelHD(
  D = NouraguesTrees$D, H = NouraguesTrees$H,
  useWeight = FALSE, drawGraph = T, method = "log2")

write.csv(x = NouraguesTrees, file="inst/shinyApp/exemple_data/forest_inv_exemple.csv", row.names = FALSE)

