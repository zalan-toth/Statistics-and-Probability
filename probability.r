###########################################################################################################
#
#   Probability assignment 2024
#
#    Zalán Tóth
#
###########################################################################################################
#
# Skeleton code is provided for each of the five R functions q1 - q5 that you are reqiured to develop
# Note that all functions take no arguments and all return two values. This should NOT be changed. 
# Neither should the final version of your functions print any output to the console, or generate and display 
# any charts. You may choose to have print (or cat) statements in your code while you are developing your 
# functions but be sure to remove, or comment them out before you make your submission.
#
# PENALTIES WILL BE IMPOSED IF THE VARIOUS STIPULATIONS RE THE ASSIGNMENT FORMAT ARE NOT ADHERED TO 
#
studentID <- 20102768 # Zalán Tóth


q1 <- function()
{
    #A municipal council is elected periodically to manage the affairs of a city. Upon election the
    #council must form an executive committee comprising 8 roles (chair, secretary, treasurer etc.)
    #and 5 councillors have expressed an interest in taking a particular role. How likely is it that at
    #least two councillors will have targeted the same role?


    #SIMULATION
    number_of_simulations <- 200
    same_role <- 0 #init
    for (i in 1:number_of_simulations) {
       # Assigning roles to 5 councillors from 8 roles
       roles <- sample(1:8, 5, replace = TRUE)
       # Check for duplicate
       if (length(unique(roles)) < 5) {
          same_role <- same_role + 1
       }
    }
    estimateP <- same_role/number_of_simulations; #calculating average

    #EXACT CALCULATION
    exactP <- 1 - ((factorial(8) / factorial(8 - 5)) / 8^5)

    
    # Return two values
    answer1 <- c(exactP, estimateP)


    return (answer1)
}

q2 <- function()
{
    #The city have a number of projects competing for public monies in the next financial year and
    #these can be placed in three categories. 9 are transport initiatives (bus lanes, cycle paths etc.),
    #10 and in the recreational domain (botanical gardens, galleries etc.) and the remaining 12 are
    #projects to promote the city’s tourism industry. If only 8 of these will receive funding and it
    #may be assumed that all projects have the same likelihood of being selected how likely is it
    #that exactly 3 transport projects will be funded?


    #EXACT CALCULATION
    # Calculating combinations
    total <- choose(31, 8)
    favorable <- choose(9, 3) * choose(31 - 9, 8 - 3)
    exactP <- favorable / total



    #SIMULATION
    number_of_simulations <- 200
    successful <- replicate(number_of_simulations, {
       projects <- sample(1:31, 8) # Simulate selecting 8 projects
       sum(projects <= 9) == 3 # Check if exactly 3 are transport projects
    })

    estimateP <- mean(successful)



    
    # Return two values
    answer2 <- c(exactP, estimateP)

    return (answer2)
}

q3 <- function()
{
    #A total of 7 issues are compiled (e.g., crime, unemployment, amenities etc.) and the council
    #have identified 3 that they think are of particular concern to the electorate. A survey of 84
    #members of the public is taken, and each is asked to select the 3 most important issues out of
    #the 7. Assuming the 7 issues are of equal concern to the public, how likely is it that there will
    #be at least 2 of those surveyed who will have the same view as the council?


    #SIMULATION
    number_of_simulations <- 200
    total <- choose(7, 3)
    estimateP <- -1 #I didn't do this part.


    #EXACT CALCULATION
    exactP <- pbinom(1, 84, 1/choose(7,3), lower=F)

    # Return two values
    answer3 <- c(exactP, estimateP)

    return (answer3)
}

q4 <- function()
{
    #Special emergency meetings (SEM) are called from time to time to deal with exceptional
    #events (flooding, worker strikes etc.) and an analysis of the pattern of such meetings suggest it
    #may be modelled with a Poisson distribution with an average of 8 months and 30 days between
    #SEMs. Taking 30 days in a month how likely is it that the number of SEMs in a 19 month
    #period will be at least 3?

    #SIMULATION
    number_of_simulations <- 200

    # Simulate SEMs over the 19-month period
    sem <- rpois(number_of_simulations, (1/9)*19)

    # At least 3 SEMs
    estimateP <- sum(sem >= 3) / number_of_simulations




    #EXACT CALCULATION
    exactP <- 1 - ppois(2, (1/9)*19)


    # Return two values
    answer4 <- c(exactP, estimateP)

    return (answer4)
}

q5 <- function()
{
    #The council is promoting an initiative to encourage the public to reduce the amount of waste
    #going to landfill sites. Currently the amount of such waste per household per annum can be
    #approximately modelled as being normally distributed with mean 4.7 and standard deviation
    #1.5 kg. How likely is it that the amount of such waste for one particular household will be
    #between 2.5 and 6.4 kg inclusive?
    #normally distributed, mean 4.7, sd 1.5 kg, chance on 2.5kg <= x <= 6.4 kg

    #SIMULATION
    number_of_simulations <- 200
    samples <- rnorm(number_of_simulations, mean = 4.7, sd = 1.5)

    # Samples between 2.5 kg and 6.4 kg
    count_within_range <- sum(samples >= 2.5 & samples <= 6.4)

    estimateP <- count_within_range / length(samples)



    #EXACT CALCULATION
    #print("4.7 mean, 1.5 standard deviation, kg >= 2.5 ")
    calc1 <- pnorm(2.5,4.7,1.5)
    #print("4.7 mean, 1.5 standard deviation, kg > 6.4 ")
    calc2 <- pnorm(6.4,4.7,1.5)
    exactP <- calc2 - calc1

    # Return two values
    answer5 <- c(exactP, estimateP)

    return (answer5)
}

q1()
q2()
q3()
q4()
q5()
