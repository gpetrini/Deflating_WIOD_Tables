source("validate_niots_contract.R")
set.seed(1)
n <- 7; yrs <- as.character(1995:1997)
mk_fd <- function(scale = 1) {
  d <- as.data.frame(matrix(abs(rnorm(n * 5)) * scale, n, 5))
  colnames(d) <- c("HFCE", "GFCF", "GGFC", "EXPO", "INVNT"); d
}
mk_country <- function() list(
  Ft = setNames(lapply(yrs, function(y) mk_fd(10)), yrs),
  Fn = setNames(lapply(yrs, function(y) mk_fd(7)),  yrs),
  Fm = setNames(lapply(yrs, function(y) mk_fd(1)),  yrs),
  lista_Am = setNames(lapply(yrs, function(y) matrix(runif(n*n, 0, .05), n, n)), yrs),
  lista_Z  = setNames(lapply(yrs, function(y) diag(n)), yrs)
)
isos <- c("USA","DEU","NLD","JPN","BRA","MEX","CHN","IND")
good <- setNames(lapply(isos, function(i) mk_country()), isos)

cat("\n--- 1. good object, strict ---\n");        stopifnot(validate_niots(good))
cat("\n--- 2. not a list ---\n");                 stopifnot(!validate_niots(data.frame(a=1)))
cat("\n--- 3. unnamed countries ---\n");          x<-good; names(x)<-NULL; stopifnot(!validate_niots(x))
cat("\n--- 4. non-ISO name ---\n");               x<-good; names(x)[2]<-"Germany"; stopifnot(!validate_niots(x))
cat("\n--- 5. slot missing in country 3 ---\n");  x<-good; x$NLD$Fm<-NULL; stopifnot(!validate_niots(x))
cat("\n--- 6. year keys differ in one slot ---\n");x<-good; names(x$JPN$Fn)<-c("1995","1996","1999"); stopifnot(!validate_niots(x))
cat("\n--- 7. year key not a date ---\n");        x<-good; for(s in names(x$USA)) names(x$USA[[s]])[3]<-"nineteen"; names(x)->nm; y<-x; y<-lapply(y,function(ct){lapply(ct,function(s){names(s)[3]<-"nineteen";s})}); names(y)<-nm; stopifnot(!validate_niots(y))
cat("\n--- 8. Ft is a matrix ---\n");             x<-good; x$USA$Ft[["1996"]]<-as.matrix(x$USA$Ft[["1996"]]); stopifnot(!validate_niots(x))
cat("\n--- 9. demand column renamed ---\n");      x<-good; colnames(x$BRA$Fn[["1995"]])[1]<-"HFCE_TOTAL"; stopifnot(!validate_niots(x))
cat("\n--- 10. non-numeric column ---\n");        x<-good; x$MEX$Ft[["1997"]]$EXPO<-as.character(x$MEX$Ft[["1997"]]$EXPO); stopifnot(!validate_niots(x))
cat("\n--- 11. non-square matrix ---\n");         x<-good; x$CHN$lista_Z[["1996"]]<-matrix(1,n,n-1); stopifnot(!validate_niots(x))
cat("\n--- 12. sector count differs ---\n");      x<-good; x$IND$lista_Am[["1995"]]<-diag(n+1); stopifnot(!validate_niots(x))
cat("\n--- 13. Fn rows != sectors ---\n");        x<-good; x$USA$Fn[["1995"]]<-x$USA$Fn[["1995"]][1:3,]; stopifnot(!validate_niots(x))
cat("\n--- 14. a required country absent ---\n"); x<-good[1:7]; stopifnot(!validate_niots(x))
cat("\n--- 15. Ft component zero ---\n");         x<-good; x$DEU$Ft[["1996"]]$INVNT<-0; stopifnot(!validate_niots(x))
cat("\n--- 16. import share above 1 ---\n");      x<-good; x$NLD$Fm[["1997"]]$EXPO<-x$NLD$Ft[["1997"]]$EXPO*2; stopifnot(!validate_niots(x))
cat("\n--- 17. the same, strict = FALSE ---\n");  stopifnot(validate_niots(x, strict = FALSE))
cat("\nALL 17 CHECKS BEHAVED AS SPECIFIED\n")
