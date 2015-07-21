## ----load_libraries, message=FALSE, warning=FALSE------------------------
library(FLasher)
# Explicitly load Rcpp because we want to use cppFunction()
library(Rcpp)
# Useful library for testing stuff
library(testthat)
library(biodyn)

## ----simple_example------------------------------------------------------
source = '
    FLQuant flq_test(FLQuant x, double mult){
        FLQuant y = x * mult;
        return y;
    }'


## ----compile_simple_example----------------------------------------------
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

source = '
   
    FLQuant flq_test(FLQuant x, double mult){
        logistic nr(x,x,x,x);
        FLQuant y = x * mult;
        return y;
    }'

cppFunction(code=source, depends = 'FLasher,biodyn', plugins=c('cpp11','biodyn'), rebuild=TRUE)


## ----test_simple_example-------------------------------------------------
data(ple4)
mult <- 2.0
flq_in <- catch.n(ple4)
flq_out <- flq_test(flq_in, mult)
# What is it
is(flq_out)
# Has the multiplication worked?`
flq_in[1:3,"2000"]
flq_out[1:3,"2000"]
# The following test only produces output if the test fails
expect_that(flq_in * mult, equals(flq_out))

## ----flq7----------------------------------------------------------------
source = '
     FLQuant7 flq7_test(FLQuant x, FLQuant y){
         // Make the FLQuant7 by passing in one object
         FLQuant7 z(x);
         // Add another by using ()
         z(y);
         return z;
    }
'

cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----flq7_test-----------------------------------------------------------
flq1 <- catch.n(ple4)
flq2 <- stock.n(ple4)
flqs <- flq7_test(flq1, flq2)
# What is it? It's a list
is(flqs)
length(flqs)
expect_that(flq1, equals(flqs[[1]]))
expect_that(flq2, equals(flqs[[2]]))

## ----flq7_get_set--------------------------------------------------------
source = '
     FLQuant flq7_get_test(FLQuant7 x){
         // Pull out the first FLQuant
         FLQuant y = x(1);
         return y;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

source = '
     FLQuant7 flq7_set_test(FLQuant7 x, FLQuant y){
         // Insert an FLQuant in the 2nd position
         x(2) = y;
         return x;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----flq7_get_set_test---------------------------------------------------
flq3 <- flq7_get_test(flqs)
expect_that(flq3, equals(flqs[[1]]))
flq4 <- landings.n(ple4)
flqs2 <- flq7_set_test(flqs, flq4)
expect_that(flq4, equals(flqs2[[2]]))

## ----flq_dim_accessor----------------------------------------------------
source = '
     std::vector<unsigned int> flq_dim_test(FLQuantAD x){
        return x.get_dim();
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)
dim(flq_in)
flq_dim_test(flq_in)

## ----flq_dim_accessors---------------------------------------------------
source = '
    std::vector<unsigned int> flq_dims_test(FLQuant x){
        // Make a vector to return
        std::vector<unsigned int> dim_out {
            x.get_nquant(),
            x.get_nyear(),
            x.get_nunit(),
            x.get_nseason(),
            x.get_narea(),
            x.get_niter()};
        return dim_out;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_flq_dim_accessors----------------------------------------------
dim(flq_in)
flq_dims_test(flq_in)

## ----set_accessor--------------------------------------------------------
source = '
    FLQuant flq_set_accessor_test(FLQuant x, unsigned int single_index,
                                  std::vector<unsigned int> indices1,
                                  std::vector<unsigned int> indices2,
                                  double value){
        x(single_index) = value;
        x(indices1[0], indices1[1], indices1[2], indices1[3],
          indices1[4], indices1[5]) = value;
        x(indices2) = value;
        return x;
    }
'

cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_set_accessor---------------------------------------------------
single_index <- round(runif(1, min=1, max=prod(dim(flq_in))))
indices1 <- round(runif(6, min=1, max=dim(flq_in)))
indices2 <- round(runif(6, min=1, max=dim(flq_in)))
value <- 666
flq_out <- flq_set_accessor_test(flq_in, single_index, indices1, indices2, value)
# And test
# Single index
c(flq_in)[single_index]
c(flq_out)[single_index]
# Six seperate integers
flq_in[indices1[1], indices1[2], indices1[3], indices1[4], indices1[5], indices1[6]] 
flq_out[indices1[1], indices1[2], indices1[3], indices1[4], indices1[5], indices1[6]] 
# Vector of integers
flq_in[indices2[1], indices2[2], indices2[3], indices2[4], indices2[5], indices2[6]] 
flq_out[indices2[1], indices2[2], indices2[3], indices2[4], indices2[5], indices2[6]] 

## ----set_accessor_range--------------------------------------------------
source = '
    FLQuant flq_range_accessor_test(FLQuant x, 
                                  std::vector<unsigned int> indices,
                                  double value){
        x(indices) = value;
        return x;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_set_accessor_range---------------------------------------------
indices <- round(runif(6, min=1, max=dim(flq_in))) 
indices[1] <- dim(flq_in)[1] + 1
# Would be nice to see the error but it breaks the generation of the Vignette!
expect_that(flq_range_accessor_test(flq_in, indices, value), throws_error())

## ----subset_flquant------------------------------------------------------
source = '
    FLQuant flq_subset_test(FLQuant x, 
                                  std::vector<unsigned int> indices1,
                                  std::vector<unsigned int> indices2){
        // Subset using two integer vectors
        FLQuant y = x(indices1, indices2); 
        return y;
    }
'

cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_subset---------------------------------------------------------
indices1 <- round(runif(6, min=1, max=ceiling(dim(flq_in)/2))) 
indices2 <- round(runif(6, min=ceiling(dim(flq_in)/2), max=dim(flq_in))) 
flq_out <- flq_subset_test(flq_in, indices1, indices2)
# Test subset is correct
subset_in <- flq_in[indices1[1]:indices2[1], indices1[2]:indices2[2],
indices1[3]:indices2[3], indices1[4]:indices2[4],
indices1[5]:indices2[5], indices1[6]:indices2[6]]
# If all this good, the test returns nothing
expect_that(subset_in, equals(flq_out))

## ----flquant_maths-------------------------------------------------------
source = '
    FLQuant7 flq_subtract_test(FLQuant x, FLQuant y, double value){
        // Subtracting a single value
        FLQuant7 z(x - value);
        z(value - x);
        // Adding a single value
        z(x + value);
        z(value + x);
        // Multiplying by a single value
        z(x * value);
        z(value * x);
        // Dividing by a single value
        z(x / value);
        z(value / x);
        // Operating on pairs of equal sized FLQuant objects
        z(x-y);
        z(x+y);
        z(x*y);
        z(x/y);
        // log and exp
        z(log(x));
        z(exp(y));
        return z;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_subset_flquant-------------------------------------------------
value <- runif(1)
flq1 <- landings.n(ple4)
flq2 <- catch.n(ple4)
flqs_out <- flq_subtract_test(flq1, flq2, value)
expect_that(flq1 - value, equals(flqs_out[[1]]))
expect_that(value - flq1, equals(flqs_out[[2]]))
expect_that(flq1 + value, equals(flqs_out[[3]]))
expect_that(value + flq1, equals(flqs_out[[4]]))
expect_that(flq1 * value, equals(flqs_out[[5]]))
expect_that(value * flq1, equals(flqs_out[[6]]))
expect_that(flq1 / value, equals(flqs_out[[7]]))
expect_that(value / flq1, equals(flqs_out[[8]]))
expect_that(flq1 - flq2, equals(flqs_out[[9]]))
expect_that(flq1 + flq2, equals(flqs_out[[10]]))
expect_that(flq1 * flq2, equals(flqs_out[[11]]))
expect_that(flq1 / flq2, equals(flqs_out[[12]]))
# When calling log and exp, units are not set correctly so we just check the data
expect_that(log(flq1@.Data), equals(flqs_out[[13]]@.Data))
expect_that(exp(flq2@.Data), equals(flqs_out[[14]]@.Data))

## ----flquant_other_methods-----------------------------------------------
source = '
    FLQuant7 flq_other(FLQuant x){
        FLQuant7 z(quant_sum(x));
        z(year_sum(x));
        z(quant_mean(x));
        z(max_quant(x));
        z(scale_by_max_quant(x));
        return z;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_flquant_other_methods------------------------------------------
flqs <- flq_other(flq1)
expect_that(quantSums(flq1), equals(flqs[[1]]))
# expect_that(yearSums(flq1), equals(flqs[[2]])) # broken - fix it
expect_that(apply(flq1, 2:6, mean), equals(flqs[[3]]))
expect_that(apply(flq1, 2:6, max), equals(flqs[[4]]))
scaled_flq1 <- sweep(flq1, 2:6, apply(flq1, 2:6, max), "/")
expect_that(scaled_flq1, equals(flqs[[5]]))

## ----flquant_iterator----------------------------------------------------
source = '
    FLQuant flq_iterator(FLQuant flq1, FLQuant flq2){
        // Make an FLQuant of same size to dump output in
        FLQuant flq3(flq1);
        // lambda function
        std::transform(flq1.begin(), flq1.end(), flq2.begin(), flq3.begin(),
            [](double x, double y) { return sqrt(x*x + y*y); } );
        return flq3;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_flquant_iterator-----------------------------------------------
flq3 <- flq_iterator(flq1, flq2)
expect_that(flq3, equals(sqrt(flq1^2 + flq2^2)))

## ----flquant_constructor-------------------------------------------------
source = '
    FLQuant7 flq_constructor(std::vector<unsigned int> dim, double value, std::string units, Rcpp::List dimnames, std::vector<double> data){
        FLQuant7 out;
        // Make an empty one filled with 0s and add it to the list
        FLQuant flq(dim[0], dim[1], dim[2], dim[3], dim[4], dim[5]);
        out(flq);
        // Fill flq with a value and add it to list
        flq.fill(value);
        out(flq);
        // Add dimnames, units and data
        flq.set_dimnames(dimnames);
        flq.set_units(units);
        flq.set_data(data);
        out(flq);
        return out;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_flq_constructor------------------------------------------------
# Make a dummy FLQ and see how all the bits go together
dim <- dim(flq1)
dimnames <- dimnames(flq1)
units <- units(flq1)
value <- runif(1)
data <- c(flq1)
flqs <- flq_constructor(dim, value, units, dimnames, data)
# First flq is full of 0s with no units or dimnames
dim(flqs[[1]])
dimnames(flqs[[1]])
units(flqs[[1]])
# Show a subset
flqs[[1]][1:3,1]
# Result of fill
expect_that(all(flqs[[2]]==value), is_true())
# The fully assembled FLQuant
expect_that(flqs[[3]], equals(flq1))

## ----access_fwdsr_constructor--------------------------------------------
source = '
    double test_fwdSR_eval(std::string model_name, FLQuant params, int timelag, FLQuant residuals, bool residuals_mult, double ssb, int year, int unit, int season, int area, int iter){
        fwdSR fwdsr(model_name, params, timelag, residuals, residuals_mult);
        double out = fwdsr.eval_model(ssb, year, unit, season, area, iter);
        return out;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----get_fwdsr_bits------------------------------------------------------
data(ple4)
# Fit a Ricker to the data so we can get the parameters.
ple4_sr_ricker <- fmle(as.FLSR(ple4,model="ricker"), control  = list(trace=0))
# The parameters are in the params slot
# Turn them into an FLQuant 
params <- as.FLQuant(params(ple4_sr_ricker))
# Look at the params
dimnames(params)
# The first dimension is for the parameters a and b.
# Dimensions 2 to 6 are of length 1.
# We don't use the residuals or the timelag in this example but we need them for the fwdSR constructor
# Get the residuals - thay are already an FLQuant
residuals <- residuals(ple4_sr_ricker)    
# These are multiplicative on the log scale
residuals_mult <- TRUE
# The time lag is 1 time step, i.e. 1 year (e.g. SSB in 2000 determines recruitment in 2001)
timelag = 1
# Finally we need a value of SSB 
# Just pull out the SSB in the first year of ple4 and make it a single numeric
ssb <- c(ssb(ple4)[,1])
# Eval the function for the first year, unit, season etc.
test_fwdSR_eval('ricker', params, timelag, residuals, residuals_mult, ssb, 1, 1, 1, 1, 1)
# Is this value right? Use the FLR method to check
predict(ple4_sr_ricker, ssb=FLQuant(ssb))

## ----fwdsr_bad_season_example--------------------------------------------
# Evaluate the value in season 1
test_fwdSR_eval('ricker', params, timelag, residuals, residuals_mult, ssb, 1, 1, 1, 1, 1)
# Evaluate the value in season 2
test_fwdSR_eval('ricker', params, timelag, residuals, residuals_mult, ssb, 1, 1, 2, 1, 1)
# Evaluate the value in season 3
test_fwdSR_eval('ricker', params, timelag, residuals, residuals_mult, ssb, 1, 1, 3, 1, 1)
# Evaluate the value in season 4
test_fwdSR_eval('ricker', params, timelag, residuals, residuals_mult, ssb, 1, 1, 4, 1, 1)

## ----fwdsr_season_2_example----------------------------------------------
season_params <- FLQuant(0, dimnames=list(params = c('a', 'b'), season = 1:4))
season_params[,,,2] <- c(params(ple4_sr_ricker))
season_params
# Evaluate the value in season 1
test_fwdSR_eval('ricker', season_params, timelag, residuals, residuals_mult, ssb, 1, 1, 1, 1, 1)
# Evaluate the value in season 2
test_fwdSR_eval('ricker', season_params, timelag, residuals, residuals_mult, ssb, 1, 1, 2, 1, 1)
# Evaluate the value in season 3
test_fwdSR_eval('ricker', season_params, timelag, residuals, residuals_mult, ssb, 1, 1, 3, 1, 1)
# Evaluate the value in season 4
test_fwdSR_eval('ricker', season_params, timelag, residuals, residuals_mult, ssb, 1, 1, 4, 1, 1)
# Evaluate the value in season 5 - even though there is no season 5
test_fwdSR_eval('ricker', season_params, timelag, residuals, residuals_mult, ssb, 1, 1, 5, 1, 1)
# If the season you ask for is not in the params, it just uses season 1
# This is dangerous and should be changed to fail or give a warning

## ----fwdsr_year_example--------------------------------------------------
year_params <- FLQuant(NA, dimnames=list(params = c('a', 'b'), year = 1:10))
# b parameter is constant in time but a parameter increases (for whatever reason)
year_params['b',] <- params['b']
year_params['a',] <- c(params['a']) * seq(from=1, to = 1.5, length=10)
year_params

# Evaluate the value in year 1
test_fwdSR_eval('ricker', year_params, timelag, residuals, residuals_mult, ssb, 1, 1, 1, 1, 1)
# Evaluate the value in year 2 with the same SSB
test_fwdSR_eval('ricker', year_params, timelag, residuals, residuals_mult, ssb, 2, 1, 1, 1, 1)
# Evaluate the value in year 3 with the same SSB
test_fwdSR_eval('ricker', year_params, timelag, residuals, residuals_mult, ssb, 3, 1, 1, 1, 1)
# And so on

## ----fwdbiol_constructor-------------------------------------------------
source = '
    fwdBiol test_fwdBiol(SEXP flbiol, std::string model_name, FLQuant params, int timelag, FLQuant residuals, bool residuals_mult){
        // Pass in the FLBiol (as an SEXP) plus the bits that make up the SRR
        fwdBiol biol1(flbiol, model_name, params, timelag, residuals, residuals_mult);
        // Or make the fwdSR then make the fwdBiol
        fwdSR sr(model_name, params, timelag, residuals, residuals_mult);
        fwdBiol biol2(flbiol, sr);
        // Return it back to R but loses SRR part
        return biol2;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_fwdbiol_constructor--------------------------------------------
# Make an FLBiol out of ple4
data(ple4)
ple4_biol <- as(ple4, "FLBiol")
# Pass into the constructor function with the SRR bits from above
biol_out <- test_fwdBiol(ple4_biol, 'ricker', year_params, timelag, residuals, residuals_mult)
is(biol_out)
# It's an FLBiol, same as the one we out in, and we have lost the SRR bits
expect_that(biol_out, equals(ple4_biol))

## ----fwdbiol_accessor----------------------------------------------------
source = '
    FLQuant test_fwdBiol(SEXP flbiol, std::string model_name, FLQuant params, int timelag, FLQuant residuals, bool residuals_mult, double value, std::vector<unsigned int> indices_min, std::vector<unsigned int> indices_max){
        // Make the fwdBiol
        fwdBiol biol(flbiol, model_name, params, timelag, residuals, residuals_mult);
        // Pull out the whole FLQuant of abundances
        FLQuant abund = biol.n();
        // Pull out first value from the abundances
        double val1 = abund(1,1,1,1,1,1);
        // Or do it all in 1 stage - note the extra ()
        double val2 = biol.n()(1,1,1,1,1,1);
        // Write a single value - again note the extra ()
        biol.n()(1,1,1,1,1,1) = value;
        // Subset a member FLQuant - read only
        FLQuant n_subset = biol.n(indices_min, indices_max);
        return n_subset;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_fwdbiol_accessor-----------------------------------------------
indices_min <- c(1,1,1,1,1,1)
indices_max <- c(4,4,1,1,1,1)
value <- 666
out <- test_fwdBiol(ple4_biol, 'ricker', year_params, timelag, residuals, residuals_mult, value, indices_min, indices_max)
# It's the subsetted n slot, with the first value changed
out 

## ----fwdbiol_biomass-----------------------------------------------------
source = '
    FLQuant7 test_fwdBiol_biomass(fwdBiol flb, std::vector<unsigned int> indices_min, std::vector<unsigned int> indices_max){
        FLQuant biomass1 = flb.biomass();
        FLQuant biomass2 = flb.biomass(indices_min, indices_max);
        FLQuant7 flqs(biomass1);
        flqs(biomass2);
        return flqs;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_fwdbiol_biomass------------------------------------------------
indices_min <- c(1,1,1,1,1)
indices_max <- c(4,1,1,1,1)
flqs <- test_fwdBiol_biomass(ple4_biol, indices_min, indices_max)
# The whole FLQuant (show the first 4 years)
quantSums(n(ple4_biol) * wt(ple4_biol))[,1:4]
flqs[[1]][,1:4]
# Using the subset method
flqs[[2]]

## ----fwdBiols_make-------------------------------------------------------
source = '
    fwdBiols test_fwdBiols(fwdBiol biol1, fwdBiol biol2){
        // Make a list of fwdBiol objects
        fwdBiols biols(biol1);
        biols(biol2);
        return biols;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_fwdBiols_make--------------------------------------------------
# Make two identical biols
data(ple4)
ple4_biol1 <- as(ple4, "FLBiol")
ple4_biol2 <- as(ple4, "FLBiol")
biols <- test_fwdBiols(ple4_biol1, ple4_biol1)
is(biols)

## ----fwdBiols_access-----------------------------------------------------
source = '
    fwdBiol test_fwdBiols_accessor(fwdBiol biol1, fwdBiol biol2){
        // Make the fwdBiols object
        fwdBiols biols(biol1);
        biols(biol2);
        // Pull out second fwdBiol from fwdBiols
        fwdBiol biol = biols(2);
        return biol;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_fwdBiols_access------------------------------------------------
out <- test_fwdBiols_accessor(ple4_biol1, ple4_biol2)
# It's an FLBiol
is(out)

## ----FLCatch_access------------------------------------------------------
source = '
    FLCatch test_FLCatch(FLCatch flc){
        // Change a value in the landings
        flc.landings_n()(1,1,1,1,1,1) = 666;
        return flc;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_FLCatch_access-------------------------------------------------
data(ple4)
ple4_catch <- as(ple4, "FLCatch")
# Need to set something for desc and name slots
ple4_catch@desc <- "something"
ple4_catch@name <- "something"
out <- test_FLCatch(ple4_catch)
is(out)
# Landings are the same except the value we changed
landings.n(ple4_catch)[1:4,1:4]
landings.n(out)[1:4,1:4]

## ----FLCatch_access_q----------------------------------------------------
source = '
    Rcpp::List test_FLCatch_q(FLCatch flc, std::vector<unsigned int> indices_min, std::vector<unsigned int> indices_max){
        FLQuant params1 = flc.catch_q_params();
        std::vector<double> params2 = flc.catch_q_params(4,1,1,1,4);
        FLQuant params3 = flc.catch_q_params(indices_min, indices_max);
        return Rcpp::List::create(Rcpp::Named("method1") = params1,
                                  Rcpp::Named("method2") = params2,
                                  Rcpp::Named("method3") = params3);
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_FLCatch_access_q-----------------------------------------------
data(ple4)
ple4_catch <- as(ple4, "FLCatch")
# Need to set something for desc and name slots
ple4_catch@desc <- "something"
ple4_catch@name <- "something"
# Make the params
catch.q(ple4_catch) <- FLPar(c(0.5,0.1), dimnames=list(param = c('alpha', 'beta'), iter = 1))
# Subset indices to include both params and years 1 to 4
indices_min <- c(1,1,1,1,1,1)
indices_max <- c(2,4,1,1,1,1)
out <- test_FLCatch_q(ple4_catch, indices_min, indices_max)
# The parameters as an FLQuant
out[["method1"]]
# The parameter values (at a year and iter range outside of the original range)
out[["method2"]]
# Subset and create a new FLQuant of the required size outside of the original range
out[["method3"]]

## ----FLCatch_methods-----------------------------------------------------
source = '
    FLQuant7 test_FLCatch_methods(FLCatch flc, std::vector<unsigned int> indices_min_age, std::vector<unsigned int> indices_max_age, std::vector<unsigned int> indices_min, std::vector<unsigned int> indices_max){
        FLQuant cn1 = flc.catch_n();
        FLQuant cn2 = flc.catch_n(indices_min_age, indices_max_age);
        FLQuant cw1 = flc.catch_wt();
        FLQuant cw2 = flc.catch_wt(indices_min_age, indices_max_age);
        FLQuant catch1 = flc.catches();
        FLQuant catch2 = flc.catches(indices_min, indices_max);
        FLQuant landings1 = flc.landings();
        FLQuant landings2 = flc.landings(indices_min, indices_max);
        FLQuant discards1 = flc.discards();
        FLQuant discards2 = flc.discards(indices_min, indices_max);
        FLQuant lsel = flc.landings_sel();
        FLQuant dsel = flc.discards_sel();
        // Put all the results together
        FLQuant7 flqs(cn1);
        flqs(cn2);
        flqs(cw1);
        flqs(cw2);
        flqs(catch1);
        flqs(catch2);
        flqs(landings1);
        flqs(landings2);
        flqs(discards1);
        flqs(discards2);
        flqs(lsel);
        flqs(dsel);
        return flqs;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_FLCatch_methods------------------------------------------------
data(ple4)
ple4_catch <- as(ple4, "FLCatch")
# Need to set something for desc and name slots
ple4_catch@desc <- "something"
ple4_catch@name <- "something"
indices_min_age <- c(1,1,1,1,1,1)
indices_max_age <- c(4,4,1,1,1,1)
indices_min <- c(1,1,1,1,1)
indices_max <- c(4,1,1,1,1)
flqs_out <- test_FLCatch_methods(ple4_catch, indices_min_age, indices_max_age, indices_min, indices_max)
# Compare the results
# Catch numbers
catch.n(ple4_catch)[1:4, 1:4]
flqs_out[[1]][1:4, 1:4]
flqs_out[[2]]
# Catch wts
catch.wt(ple4_catch)[1:4, 1:4]
flqs_out[[3]][1:4, 1:4]
flqs_out[[4]]
# Catch total
catch(ple4_catch)[, 1:4]
flqs_out[[5]][, 1:4]
flqs_out[[6]]
# Landings total
landings(ple4_catch)[, 1:4]
flqs_out[[7]][, 1:4]
flqs_out[[8]]
# Discards total
discards(ple4_catch)[, 1:4]
flqs_out[[9]][, 1:4]
flqs_out[[10]]
# Landings sel
landings.sel(ple4_catch)[1:4, 1:4]
flqs_out[[11]][1:4, 1:4]
# Discards sel
discards.sel(ple4_catch)[1:4, 1:4]
flqs_out[[12]][1:4, 1:4]

## ----FLFishery_as--------------------------------------------------------
source = '
    FLFishery test_FLFishery(FLFishery flf){
        // Change landings_n in the second FLCatch - note the brackets
        flf(2).landings_n()(1,1,1,1,1,1) = 666;
        return flf;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----FLFishery_test_as---------------------------------------------------
# Use the random generator from the test_helper_functions.R
flf_in <- random_FLFishery_generator(max_catches = 2)
is(flf_in)
length(flf_in)
flf_out <- test_FLFishery(flf_in)
landings.n(flf_in[[2]])[,1,1,1,1,1]
landings.n(flf_out[[2]])[,1,1,1,1,1]

## ----FLFishery_subset----------------------------------------------------
source = '
    FLQuant test_FLFishery_indices(FLFishery flf, std::vector<unsigned int> indices_min, std::vector<unsigned int> indices_max){
        // Change landings_n in the second FLCatch - note the brackets
        FLQuant flq = flf(2).catch_n(indices_min, indices_max);
        return flq;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----FLFishery_test_subset-----------------------------------------------
indices_min <- c(1,1,1,1,1,1)
indices_max <- pmax(dim(landings.n(flf_in[[2]])) - 1, c(1,1,1,1,1,1))
flq_out <- test_FLFishery_indices(flf_in, indices_min, indices_max)
# Bit faffy subsetting the original object
flq_in <- catch.n(flf_in[[2]])[indices_min[1]:indices_max[1],
    indices_min[2]:indices_max[2],
    indices_min[3]:indices_max[3],
    indices_min[4]:indices_max[4],
    indices_min[5]:indices_max[5],
    indices_min[6]:indices_max[6]]
expect_that(flq_in, equals(flq_out))

## ----FLFishery_effort----------------------------------------------------
source = '
    FLFishery test_FLFishery_effort(FLFishery flf){
        // Change effort in the first year etc - note the extra brackets
        flf.effort()(1,1,1,1,1,1) = 666;
        return flf;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_FLFishery_effort-----------------------------------------------
flf_out <- test_FLFishery_effort(flf_in)
effort(flf_in)[,1:dim(effort(flf_in))[2],1,1,1,1]
effort(flf_out)[,1:dim(effort(flf_out))[2],1,1,1,1]

## ----FLFisheries_example-------------------------------------------------
source = '
    FLFisheries test_FLFisheries(FLFisheries flfs){
        // Pull out a whole FLFishery
        FLFishery flf = flfs(1);
        // Pull out a whole FLCatch using 2 sets of ()
        FLCatch flc1 = flfs(1)(1);
        // Or 1 set o ()
        FLCatch flc2 = flfs(1,1);
        // Change a value in the landings_n of an FLCatch
        flfs(1,1).landings_n()(1,1,1,1,1,1) = 666;
        return flfs;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----test_FLFisheries_example--------------------------------------------
flfs_in <- random_FLFisheries_generator(max_fisheries = 2)
flfs_out <- test_FLFisheries(flfs_in)
landings.n(flfs_in[[1]][[1]])[,1,1,1,1,1]
landings.n(flfs_out[[1]][[1]])[,1,1,1,1,1]


## ----FLStock-------------------------------------------------------------
source = '
    FLStock test_FLStock(FLStock fls){
        FLQuant ca = fls.catches;
        // Public members means you can access the members directly
        fls.catches(1,1,1,1,1,1) = 666;
        fls.catches.set_units("test");
        return fls;
    }
'
cppFunction(code=source, depends = 'FLasher', plugins='cpp11', rebuild=TRUE)

## ----FLStock_test--------------------------------------------------------
data(ple4)
out <- test_FLStock(ple4)
catch(out)[1,1:4]

