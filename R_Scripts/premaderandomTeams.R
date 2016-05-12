participantsf <- c("Anne", "Isabel", "Kayleigh", "Jenna", "Monika", "Susan")

participantsm <- c("Kevin", "Jake", "James", "Jason", "Heykan", "Tim", "Batu", "Nick", "Raed", "Max")  

#spectators <- c("Priya", "Kristin", "Dave", "Mercedeh", "Calorina", "Janelle", "Brad")

teamPool <- c("America", "Iran", "Germany", "China", "Britain", "Netherlands", "Canada", "Turkey")

sample(participantsf) -> participantsf
sample(participantsm) -> participantsm
append(participantsm, participantsf) -> participants

numParticipants <- length(participants)
if (numParticipants %% 4 != 0){
  print("Not the right number of players FUCKTWAD")
} else { 
  print("Beginning TEAM FORMATION...")
}

Teams <- matrix(participants, nrow = 4, ncol = numParticipants / 4, byrow = TRUE)
rownames(Teams) <- c("Captain", "Co-Captain", "3rd Wheel", "Random and Still Last")
sample(teamPool) -> teamPool
colnames(Teams) <- teamPool[1:(numParticipants / 4)]
View(Teams)
