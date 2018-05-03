# we can "hand code" the Wald, or use the inverse Gaussian distribution in R (we do the latter here). 
# to generate random deviates manually, see https://en.wikipedia.org/wiki/Inverse_Gaussian_distribution#Generating_random_variates_from_an_inverse-Gaussian_distribution (correct at time of writing) and the referenced paper Michael, John R.; Schucany, William R.; Haas, Roy W. (May 1976). "Generating Random Variates Using Transformations with Multiple Roots". The American Statistician. American Statistical Association. 30 (2): 88–90
library("statmod")
library("dfoptim") # we use the nmkb fn from this library to perform bounded optimization


gen_m <- 2
gen_a <- 2.5
gen_Ter <- 0.5
gen_N <- 10000

# we use the RNG for the inverse gaussian. First passage times in Wald are distributed as IG(a/m,a^2)
rt <- rinvgauss(n=N, mean = gen_a/gen_m, shape = gen_a^2) + gen_Ter

# define the Wald density (one could also use dinvgauss from the statmod package,
# but first need to transform from shifted Wald to unshifted ex-Gaussian
rswald <- function(t, m, a, Ter){
  ans <- a/sqrt(2*pi*(t-Ter)^3)*
    exp((-(a-m*(t-Ter))^2)/(2*(t-Ter)))
}

# in the below, Ter needs to be smaller than min RT
fitres <- nmkb(par=c(1,1,min(rt)/2),
               fn = function(theta) sum(-2*log(rswald(rt,theta[1],theta[2],theta[3]))),
               lower=c(0,0,0),
               upper=c(Inf,Inf,min(rt)))

print(fitres)

# a further exercise would be to put this in a loop so as to generate many data sets and fit the model to each data set