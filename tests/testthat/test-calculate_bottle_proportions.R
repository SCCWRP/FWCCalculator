flow <- FashionValleyFlow
sample <- FashionValleySample
joined <- FashionValleyJoined

excel_right_riemann_proportions <- c(0.0493912531825856,0.00341079686993055,0.00341904547179253,0.00373386710952494,0.00402119340771739,0.00441162722918465,0.00472507409994006,0.00492441531160468,0.00497665645673058,0.00494503681625964,0.00482955639019186,0.00467420772179116,0.00452298335432144,0.00434151411335779,0.00415454580448614,0.00397307656352248,0.0038053549923288,0.00365962969276708,0.00351527916018235,0.00340117350109156,0.00374761477929491,0.00579326804106704,0.00664012449889744,0.854982705431429)
excel_left_riemann_proportions <- c(0.0489462267319265,0.00344926307444132,0.00340521821475172,0.00363920653185269,0.00395027335341045,0.0043274074645026,0.00465912031403985,0.00489861423860202,0.00497156353746291,0.00497156353746291,0.00486695699570012,0.00472105839797834,0.00456827779093006,0.00438659274471049,0.00420903690408681,0.00402459905413664,0.00384704321351296,0.00369976821392588,0.0035552460180694,0.00343136985019242,0.00347128550428612,0.00538035489145695,0.0064277967109501,0.856192156711611)
excel_trapezoidal_proportions <- c(0.0491688721862431,0.00343001854287145,0.0034121359517115,0.00368656494681993,0.0039857544527652,0.00436954237073638,0.00469211680358312,0.00491152244127632,0.00497411151033614,0.00495829229507926,0.00484824558024882,0.00469761913932464,0.00454561711446509,0.00436404003499486,0.00418177516355693,0.00399882250015131,0.00382618671626105,0.00367968702714302,0.00353525071392806,0.00341626270351764,0.00360953224643861,0.00558693415354818,0.0065340236930577,0.855587071711942)

right_riemann_calculated_proportions <- calculate_bottle_proportions(flow, joined, time_unit = "s", method = "right_riemann")$Proportions
left_riemann_calculated_proportions <- calculate_bottle_proportions(flow, joined, time_unit = "s", method = "left_riemann")$Proportions
trapezoidal_calculated_proportions <- calculate_bottle_proportions(flow, joined, time_unit = "s", method = "trapezoidal")$Proportions

sample_concentrations <- sample$conc_values[sample$conc == "Concentration - Ent1A (genes per 100 mL)"]

excel_right_riemann_emc <- 110173.4043
excel_left_riemann_emc <- 109906.1578
excel_trapezoidal_emc <- 110039.8605

test_that("sample proportions are calculated correctly", {
  expect_equal(right_riemann_calculated_proportions, excel_right_riemann_proportions)
  expect_equal(left_riemann_calculated_proportions, excel_left_riemann_proportions)
  expect_equal(trapezoidal_calculated_proportions, excel_trapezoidal_proportions)
})

test_that("EMC is calculated correctly", {
  expect_equal(as.numeric(right_riemann_calculated_proportions%*%sample_concentrations), excel_right_riemann_emc)
  expect_equal(as.numeric(left_riemann_calculated_proportions%*%sample_concentrations), excel_left_riemann_emc)
  expect_equal(as.numeric(trapezoidal_calculated_proportions%*%sample_concentrations), excel_trapezoidal_emc)
})
