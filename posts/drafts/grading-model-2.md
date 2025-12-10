---
title: "Improving our Mathematical Model"
date: "September 2025"
description: "Improvements to our previous model"
---

# Further Developments
Let's continue! We will begin with some basic input validation

![](/public/grading-model-2/1.png)

For our baseline cubic adjustment y=ax^3+bx, we require:

b >= 0. This is strictly necessary since, regardless of the value assigned to a, we observe an 'inversion' where our function violates monotonicity: as x increase, y decreases in some points around the origin.

Heuristically a > 0 for similar reasons. Technically this is not entirely true since if we use a prior scaling parameter to restrict our relevant domain to within the local extremum we have a valid function. This is duly noted (for now) as we currently need less complexity not more!

![](/public/grading-model-2/2.png)

We now move on to alerting users of dubious parameter combinations. Remembering back to DB 4, we can efficiently "raise the alarm" if a substantial proportion of our outputs are rounded to either extremity.

For a concrete example of this, imagine if someone with a 3.5 rating was receiving a group-adjustment from 80 to 100. Acknowledging the large degree of subjectivity which we want to build into our function for versatile use-cases, we can almost certainly assume that our user has made a mistake and is not receiving a desirable adjustment.

We can say this because we observe that a +0.5 out of a possible +2 receives such a high bonus that it needs to be rounded down to 100. We have come to admire in this function the "diminishing returns" behavior where, as we obtain a greater proportion of a  +2 bonus (and vice versa with -2 penalty), we continually receive slightly less reward ensuring that insanely high grades near 100 remain exclusive to the very few who probably deserve it.

How can we incorporate such considerations into our model, including further support for aforementioned subjectivity. I propose "second-degree" parameters. These are more easily interpretable parameters which can be set quickly based on desired behavior and act as a reference for "first-degree" parameters. This helps us greatly as it quantifies subjectivity.

Back to our concrete example, what proportion of values should be rounded? Some might say that only individual ratings of 1 and 5 should be rounded. Others might suggest values less than 1.5 and more than 4.5, while their neighbors might say values less than 2 and greater than 4. Their uncles might be even more liberal (although I wouldn't recommend it).

Our idea is a parameter, say upsilon, which measures this proportion of rounded extreme values. Ideally, we would generalize as a percentage but we will save those 5 minutes for a later day and work on our assumed individual rating domain of [1, 5].

Before we continue it must be said that in our implementation of this function, rather than fixing units for our y-axis and requiring that our curve adjustment matches this scale, we are taking the approach of not requiring this restriction. This allows for greater flexibility in alpha-beta parameter adjustments, although I wonder whether this approach yields a return.

So in our concrete symmetrical example: for 1 and 5 we can say upsilon = 0.2; for less than 1.5 and greater than 4.5 we can say upsilon = 1; for less than 2 and greater than 4 we can say upsilon = 2. In this definition,  an upsilon less than 0 or greater than 4 is invalid. While an upsilon greater than - let's say 3 - is highly questionable.

Our function will compute the adjustment for the extreme values and through a warning() if a value outside of the upsilon range is rounded. (we think second-degree parameters shouldn't have the power to stop() in order to prevent parameter-ception).

Before we implement this, we would like to acknowledge a key limitation of this approach which we will likely have to take into account: one similar to a computer's struggle with floating-point values. Should values of 99.9 be considered rounded? We might need to hard-code a buffer to account for this. Furthermore, this consideration alerts us of a key pitfall of a naïve implementation: curves which diminish to the point where they do not hit this rounding cap, say taper off around 99, must be taken into account. Therefore we will not flag if our calculated value to compare with upsilon is 0, but must implement an additional second-degree parameter which will alert us if too much of the response range is left unattainable. Let's call such parameter eta (why yes, I am having fun naming these!). We will simply calculate the adjusted value of either extremity (1 and 5) and throw a warning if the unutilized response units are greater than eta.

The benefit of this conceived approach is first the simplicity and secondly the similarity of upsilon and eta. One exists for group grade input (eta) while the other exists for individual ratings (upsilon).

In order to implement our work, we must now consider how we go about computing the above calculations. We can't use the same calculation pipeline as we have in place for our output, since our scaled adjustment (for non-genuine scoring) would spoil our results. Thankfully, we modularized our adjustment steps well and can simply drop this adjustment in a separate, prior pipeline in our (growing!) input validation section.

Theta is very transparent: expected/average grade as a percentage. After adjusting our default value from 50 to 70 in order to better reflect common grading practice, we incorporate some bogus-value checks to ensure [0,100] (although I would suggest that values outside of let's say [40,80] are very dubious).
Implementation
calculate.grades <- function(group_grade, individual_ratings_vector, alpha=1.5, beta=10, theta=70, zeta=1, eta=20, upsilon=1) {
  
  number_of_members <- length(individual_ratings_vector)
  total_rating <- sum(individual_ratings_vector)
  centered_ratings_vector <- individual_ratings_vector - 3
  
  ##input validation
  if(group_grade < 0 || group_grade > 100 || !is.numeric(group_grade)) {
    stop("group_grade must be in [0,100]")
  }
  if(!is.vector(individual_ratings_vector) || number_of_members==1) {
    stop("individual_ratings_vector must be a vector of length more than 1") # a group of one member cannot receive a peer rating so a peer adjustment cannot be considered
  }
  
  if(!is.numeric(alpha) || alpha < 0) {
    stop("we require alpha >= 0")
  }
  if(!is.numeric(beta) || beta < 0) {
    stop("we require beta >= 0")
  }
  
  if(!is.numeric(eta) || eta < 0 || eta > 100) {
    stop("we require eta in [0,100]")
  } else if(eta > 20) {
    warning("eta probably too high")
  }
  
  if(!is.numeric(upsilon) || upsilon < 0 || upsilon > 4) {
    stop("we require upsilon in [0,4]")
  } else if(upsilon > 2) {
    warning("upsilon probably too high")
  }
  
  check_vec <- c(1, 1+1/2*upsilon, 5-1/2*upsilon, 5)
  adjustment_check <- (alpha*check_vec)^3+beta*check_vec
  adjustment_check[1:2] <- adjustment_check[1:2]*group_grade/theta
  adjustment_check[3:4] <- adjustment_check[3:4]*(100-group_grade)/theta
  
  if(adjustment_check[2] < 0.1 || adjustment_check[3] > 99.9) {
    warning("Upsilon not satisfied. Current parameters provide too strong of an adjustment.")
  }
  
  check_vec
  if(adjustment_check[1] > 1/2*eta|| adjustment_check[4] < 100-1/2*eta) {
    warning("Eta not satisfied. Current parameters do not utilize full grade range.")
  }
  
  ##output calculation
  raw_adjustment_vector <- (alpha*centered_ratings_vector)^3+beta*centered_ratings_vector
  
  bounded_adjustment_vector <- ifelse(centered_ratings_vector<0, raw_adjustment_vector*group_grade/theta, raw_adjustment_vector*(100-group_grade)/theta)
  
  secure_adjustment_vector <- bounded_adjustment_vector * (number_of_members * 3 / total_rating) * 1/zeta
  #zeta>1 probably needed to reduce adjustment severity
  
  final_grade_vector <- rep(group_grade, number_of_members) + secure_adjustment_vector
  
  final_grade_vector[final_grade_vector<0]<- 0
  final_grade_vector[final_grade_vector>100]<- 100
  
  return(final_grade_vector)
}

calculate.grades(50,  c(1, 2, 3))

Bug found: check_vec not adjusted to [-2,2]

## Taking Stock
This has helped identify our problems, but hasn't really solved them. Upsilon is a tricky bugger.

One idea we can incorporate is a hidden parameter which scales the x-axis to utilize the curve given as best as possible. This is promising as it might automate resolutions which could otherwise be torturous for our user.

Further explanatory analysis
Let's simplify our thinking by considering a quadratic version of function, which doesn't contain a third x term causing us these scaling problems.

y = 1/2*x*|x|	
It's derivative:
y = |x|
Our cubic function
y = 1/3 x^3 + x
 It's derivative:
y = x^2 + 1
 
 


Blue is quadratic. Red is cubic.
 
What would happen if we changed our domain to [-1,1]? Until now we have been considering [-2,2] (which we inherited from our rating vector scale).
 
Let's consider when b=0:
 

Green: y=x. Blue: y=x*|x|. Red: y=x^3. Purple: y=x^3*|x|
 
 
Functions are scaled by alpha = 2
 
 
Functions are scaled by alpha = 0.5

As you can see, the multiplicative identity -- 1 -- unifies these polynomial functions of differing degree. At the point x=1 we also know our y value will be equal to alpha.
 
The first improvement is to simply transform our individual grade from [-2,2] to [-1,1]. It is now closed under multiplication, which might be useful. More importantly, as there was a short range of useful alpha parameters, it now "doubles that" in comparison to the interval between natural numbers -- 1 -- and is therefore slightly more intuitive (0.05 now equals 0.1).
 
 The next improvement we can make, for upsilon
 
reverse engineer where our rounding borders are for input user alpha/beta. return what the actual upsilon is. user can interpret as "this is where effectively 0% / 100%". Then user can adjust alpha/beta and see change in actual upsilon. NB: assume group is average. if higher/lower performing then this will shift.



 
 
Our function's derivative. DB 2

calculate.grades <- function(group_grade, individual_ratings_vector, alpha=1.5, beta=10, theta=70, zeta=1, eta=20, upsilon=1) {
  
  number_of_members <- length(individual_ratings_vector)
  total_rating <- sum(individual_ratings_vector)
  centered_ratings_vector <- (individual_ratings_vector - 3) * 0.5
  
  ##input validation
  if(group_grade < 0 || group_grade > 100 || !is.numeric(group_grade)) {
    stop("group_grade must be in [0,100]")
  }
  if(!is.vector(individual_ratings_vector) || number_of_members==1) {
    stop("individual_ratings_vector must be a vector of length more than 1") # a group of one member cannot receive a peer rating so a peer adjustment cannot be considered
  }
  
  if(!is.numeric(alpha) || alpha < 0) {
    stop("we require alpha >= 0")
  }
  if(!is.numeric(beta) || beta < 0) {
    stop("we require beta >= 0")
  }
  
  if(!is.numeric(eta) || eta < 0 || eta > 100) {
    stop("we require eta in [0,100]")
  } else if(eta > 20) {
    warning("eta probably too high")
  }
  
  if(!is.numeric(upsilon) || upsilon < 0 || upsilon > 4) {
    stop("we require upsilon in [0,4]")
  } else if(upsilon > 2) {
    warning("upsilon probably too high")
  }
  
  check_vec <- 0.5 * c(-2, -2+1/2*upsilon, 2-1/2*upsilon, 2)
  adjustment_check <- (alpha*check_vec)^3+beta*check_vec
  adjustment_check[1:2] <- adjustment_check[1:2]*group_grade/theta
  adjustment_check[3:4] <- adjustment_check[3:4]*(100-group_grade)/theta
  
  if(adjustment_check[2] < 0.1 || adjustment_check[3] > 99.9) {
    warning("Upsilon not satisfied. Current parameters provide too strong of an adjustment.")
  }
  
  check_vec
  if(adjustment_check[1] > 1/2*eta|| adjustment_check[4] < 100-1/2*eta) {
    warning("Eta not satisfied. Current parameters do not utilize full grade range.")
  }
  
  ##output calculation
  raw_adjustment_vector <- (alpha*centered_ratings_vector)^3+beta*centered_ratings_vector
  
  bounded_adjustment_vector <- ifelse(centered_ratings_vector<0, raw_adjustment_vector*group_grade/theta, raw_adjustment_vector*(100-group_grade)/theta)
  
  secure_adjustment_vector <- bounded_adjustment_vector * (number_of_members * 3 / total_rating) * 1/zeta
  #zeta>1 probably needed to reduce adjustment severity
  
  final_grade_vector <- rep(group_grade, number_of_members) + secure_adjustment_vector
  
  final_grade_vector[final_grade_vector<0]<- 0
  final_grade_vector[final_grade_vector>100]<- 100
  
  return(final_grade_vector)
}

calculate.grades(50,  c(1, 2, 3, 4, 5))

actual.upsion <- function(group_grade, precision=0.1, alpha=1.5, beta=10, theta=70, zeta=1, eta=20, upsilon=1) {
  
  individual_ratings_vector <- seq(1,5,by=precision)
  number_of_members <- length(individual_ratings_vector)
  total_rating <- sum(individual_ratings_vector)
  centered_ratings_vector <- (individual_ratings_vector - 3) * 0.5
  
  raw_adjustment_vector <- (alpha*centered_ratings_vector)^3+beta*centered_ratings_vector
  
  bounded_adjustment_vector <- ifelse(centered_ratings_vector<0, raw_adjustment_vector*group_grade/theta, raw_adjustment_vector*(100-group_grade)/theta)
  
  secure_adjustment_vector <- bounded_adjustment_vector * (number_of_members * 3 / total_rating) * 1/zeta
  #zeta>1 probably needed to reduce adjustment severity
  
  final_grade_vector <- rep(group_grade, number_of_members) + secure_adjustment_vector
  
  actual_upsilon_lower <- if (any(final_grade_vector <= 0)) max(which(final_grade_vector <= 0)) else 0

  actual_upsilon_upper <- if (any(final_grade_vector >= 5)) min(which(final_grade_vector >= 5)) else 0

  actual_upsilon <- precision * (actual_upsilon_lower + actual_upsilon_upper)
  return(actual_upsilon)
}
actual.upsion(50, precision = 1)

 
Notes: higher b means adjustments closer to 3 are stronger
 
to be continued