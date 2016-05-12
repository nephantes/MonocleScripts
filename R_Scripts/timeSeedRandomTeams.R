participants <- c("Kevin", "Anne", "Isabel", "Jake", "Mike", "Kayleigh", "James", "Jenna", "Max", 
                  "Jason", "Gordie", "Adam", "Meg", "Hakan", "Tim", "Raed", "Kellie Ann" , "Danny", "Batu", "Brad") #Jaqueto

teamPool <- c("America", "AMURICAW!", "France", "Germany", "China", "Britain", "Netherlands", "Turkey")

numParticipants <- length(participants)
if (numParticipants %% 4 != 0){
  print("Not the right number of players")
} else { 
  print("Beginning TEAM FORMATION...")
}

Teams <- matrix(nrow = numParticipants / 4, ncol = 4)
sample(teamPool) -> teamPool
colnames(Teams) <- c("Captain", "Co-Captain", "3rd Wheel", "Random and Still Last")
rownames(Teams) <- teamPool[1:nrow(Teams)]
rm(numParticipants)

particRand <- c()

### RANDOM TEAM GENERATOR ###

seedGenerator <- function(){
randomizer_Seed <- Sys.time()
seconds <- substr(randomizer_Seed, 18, 19)
minutes <- substr(randomizer_Seed, 15, 16)
hours <- substr(randomizer_Seed, 12, 13)
as.numeric(seconds) -> seconds
as.numeric(minutes) -> minutes
as.numeric(hours) -> hours
if (seconds == 0) {
  (minutes * hours) -> seconds
} else {
  seconds -> seconds
}
(seconds * minutes * hours) -> seed
((seed ^ 13) / 2) -> seed
return(seed)
}

while (length(participants > 0)){
  seedGenerator() -> seed
  if (length(participants) >= 10){
    substr(seed, 3,4) -> seed
    as.numeric(seed) -> seed
    if (seed < length(participants)){
      participants[seed] -> selected
      append(particRand, selected) -> particRand
      setdiff(participants, selected) -> participants
    } else {
      while (seed > length(participants)){
        seedGenerator() -> seed
        substr(seed, 3,4) -> seed
        as.numeric(seed) -> seed
        if (seed < length(participants)) {
          participants[seed] -> selected
          append(particRand, selected) -> particRand
          setdiff(participants, selected) -> participants 
        }
      }
    }
  } else {
    if (length(participants) < 10){
      substr(seed, 3,3) -> seed
      as.numeric(seed) -> seed
      if (seed <= length(participants)){
        participants[seed] -> selected
        append(particRand, selected) -> particRand
        setdiff(participants, selected) -> participants
      } else {
        while (seed > length(participants)){
          seedGenerator() -> seed
          substr(seed, 3,3) -> seed
          as.numeric(seed) -> seed
          if (seed < length(participants)) {
            participants[seed] -> selected
            append(particRand, selected) -> particRand
            setdiff(participants, selected) -> participants 
          }
        }
      }
    }
  }
}
Teams[,]<-c(particRand)
