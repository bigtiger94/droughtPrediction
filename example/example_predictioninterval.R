###### only for checking linear model

temp_smi.ts = merge(sy.sri.df, sy.smi.ts, all=F) %>% cbind(year(.), month(.), .)%>%setNames(., c("YEAR", "MONTH", colnames(sy.sri.df), "SMI"))
calibyear = c(2000:2013)
validyear = setdiff(temp_smi.ts$YEAR, calibyear)
calib_smi.ts = temp_smi.ts$YEAR %in% calibyear %>% temp_smi.ts[.,]
valid_smi.ts = temp_smi.ts$YEAR %in% validyear %>% temp_smi.ts[.,]

bsnname = "soyang"
mm = 1
calib_smi.mm = calib_smi.ts$SMI[month(calib_smi.ts) == (12)]
calib_sri.mm = calib_smi.ts[month(calib_smi.ts) == mm, 3:4]
temp_df = data.frame(calib_smi.mm[-length(calib_smi.mm),], calib_sri.mm[-1,]) %>% setNames(.,c("SMI", "SRI3_lead1", "SRI12_lead1"))
temp_lm = lm(SMI ~ SRI3_lead1, data=temp_df)

valid_smi.mm = valid_smi.ts$MONTH %in% mm %>% valid_smi.ts[.,]
validdata = data.frame(SRI3_lead1 = as.vector(valid_smi.mm$SRI3))


## auto from R function
valid.pred = predict(temp_lm, newdata=validdata, interval="prediction", level=0.95) %>% xts(., (date(valid_smi.mm)-31))

temp_lm$fitted.values
calib_smi.ts$SMI[end(calib_smi.ts)]
valid_smi.ts$MONTH %in% 12 %>% valid_smi.ts$SMI[.]

(sd_total1 = (valid.pred$upr-valid.pred$fit)/1.96)
(sd_total2 = (valid.pred$fit-valid.pred$lwr)/1.96)


## by myself
term3 = (valid_smi.mm$SRI3 - mean(temp_df$SRI3_lead1))^2 / nrow(temp_df) / var(as.vector(temp_df$SRI3_lead1))
sd(temp_lm$residuals) * sqrt((1+1/nrow(temp_df)+term3))
var(calib_sri.mm$SRI3)
nrow(calib_sri.mm) / var(calib_sri.mm$SRI3)

sd(temp_lm$residuals)
