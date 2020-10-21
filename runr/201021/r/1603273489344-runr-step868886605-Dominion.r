# Dominion random card picker

randomiser <- function() {
  cards <- c("Moat", "Dismantle", "Envoy", "Black Market", "Adventurer", "Bureaucrat",
             "Cellar", "Chancellor", "Chapel", "Council Room", "Feast", "Festival", "Laboratory",
             "Library", "Market", "Militia", "Mine", "Money Lender", "Remodel", "Smithy", 
             "Spy", "Thief", "Throne Room", "Village", "Witch", "Woodcutter", "Workshop", "Gardens")
  playing_cards <- sample(cards, 10)
  playing_cards
}
randomiser()
