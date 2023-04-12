// from boardGame import *
// from enum import Enum

enum CardSuit {
    case Clubs
    case Hearts
    case Spades
    case Diamonds
}

enum CardValue (value: Int) {
    case Two extends CardValue(2)
    case Three extends CardValue(3)
    // etc.
}

class PlayingCard(suit: CardSuit, value: Int) extends Card {
    override def toString: String =
        var repStr = ""
        if value > 10 then
            repStr = s"${List('J','Q','K','A')(value-11)}${suit.toString()(0)}"
        else
            repStr = s"${value}${suit}"
        repStr
}
class CardGame extends Game {
    var cards = ListBuffer[PlayingCard]()
    for suit <- CardSuit.values do
        for value <- 2 to 13 do
            cards += PlayingCard(suit, value)
    var deck = Stack(cards)
    deck.shuffle()
}
class HeartsPlayer(name: String) extends Player {
    var wonCards = Stack(ListBuffer[PlayingCard])
    // var playedCard = Card()
    // def playCard() = ???
    def scoreStack() =
        var score = 0
        for card <- wonCards.cards do
            if card.suit == CardSuit.Hearts then
                score += 1
            else if card.suit == CardSuit.Spades and card.value == 12 then
                score += 13
        return score
    // move to game?
    def showInfo(game: HeartsGame) =
        super.showInfo()
        for player <- game.players do
            print(s"${player.name} has won ${player.wonCards.count} cards")
        print(s"So far, the cards played have been ${game.playedCards}")
}
class HeartsCard extends PlayingCard {

}
class HeartsGame(endScore: Int) extends CardGame {
    var heartsBroken = false
    // var playArea = Location()
    var player1 = HeartsPlayer("Alice")
    var player2 = HeartsPlayer("Bob")
    var player3 = HeartsPlayer("Carrie")
    var player4 = HeartsPlayer("David")
    var player1.nextPlayer = player2
    var player2.nextPlayer = player3
    var player3.nextPlayer = player4
    var player4.nextPlayer = player1
    // necessary?
    val players = List(player1, player2, player3, player4)
    val firstPlayer = players(0)
    var playedCards = ListBuffer[PlayingCard]()
    
    def beats(card: Card, other: Card, lead: CardSuit) =
        if card.suit == lead and other.suit != lead then
            return true
        else if card.suit != lead and other.suit == lead then
            return false
        else if card.value.value > other.value.value then
            return true
        else
            return false

    // override def round(condition: => Boolean)(body: => Unit): Unit = ???
    override def turn(player: HeartsPlayer): Unit = ???

    // ignoring player count deck modifications
    // game loop
    override def play() =
        round(true) {
            eachPlayer(firstPlayer) { player =>
                if player.points >= endScore then
                    endRounds
            }
            deal(13) from deck
            //swapping would happen here
            //find player with 2 of clubs here
            var winningPlayer = players(0)
            // eachPlayer(firstPlayer) {
            for player<-players do
                for card <- player.hand.cards do
                    if card.suit == CardSuit.Clubs and card.value == 2 then
                        winningPlayer = player
            // }
            round(firstPlayer.hand.count > 0) {
                eachPlayer(firstPlayer) {
                    turn
                }
                playedCards = ListBuffer[Card]()
                var currentPlayer = winningPlayer
                currentPlayer.showInfo()
                // var index = prompt "Which card [index] would you like to lead? "
                //             where 
                // REPLACE BELOW WITH PROMPT
                var index = 0
                var invalidChoice = true
                while invalidChoice do
                    print("Which card [index] would you like to lead? ")
                    try {
                        index = readLine.toInt()
                    } catch {
                        print("Please enter a number")
                    }
                    if currentPlayer.hand.cards(index).suit != CardSuit.Hearts then
                        invalidChoice = true
                    else if heartsBroken then
                        invalidChoice = true
                    else
                        if currentPlayer.hand.cards.filter(_.suit != CardSuit.Hearts).isEmpty then
                            invalidChoice = true
                        print("Hearts cannot be led until a Heart or the QS have been won")
                winningCard = currentPlayer.hand.cards(index)
                playedCards += winningCard
                currentPlayer.hand.cards.remove(index)
                eachPlayer(currentPlayer.nextPlayer) { player =>
                    if player == winningPlayer then
                        skipRest
                    player.showInfo(this)
                    // should be forced to follow if possible
                    // some prompt nonsense here instead of this
                    index = 0
                    invalidChoice = true
                    while invalidChoice do
                        print("Which card [index] would you like to play? ")
                        try {
                            index = readLine.toInt()
                        } catch {
                            print("Please enter a number")
                        }
                        if currentPlayer.hand.cards[index].suit == winningCard.suit then
                            invalidChoice = false
                        if currentPlayer.hand.cards.filter(_.suit == winningCard.suit).isEmpty then
                            invalidChoice = false
                        print("You must follow suit if possible")
                    playedCard = currentPlayer.hand.cards(index)
                    if playedCard.suit == CardSuit.HEARTS then
                        heartsBroken = true
                    else if playedCard.suit == CardSuit.Spades and playedCard.value == 12 then
                        heartsBroken = true
                    playedCards += playedCard
                    currentPlayer.hand.cards.remove(index)
                    if beats(playedCard, winningCard, winningCard.suit) then
                        winningCard = playedCard
                        winningPlayer = currentPlayer
                }
                for wonCard <- playedCards do
                    wonCard.discard(winningPlayer.wonCards, false)
            }
            eachPlayer(firstPlayer) { player =>
                var score = player.scoreStack()
                player.wonCards.discardAll(deck, false)
                if score == 26 then
                    player.points -= 26
                    eachPlayer(firstPlayer) { player =>
                        player.points += 26
                    }
                else
                    player.points += score
            }
            eachPlayer(firstPlayer) { player =>
                print(s"${player.name} Score: ${player.points}")
            }
            deck.shuffle()
            heartsBroken = false
        }
        var winner = HeartsPlayer("")
        winner.points = endScore + 26
        eachPlayer(firstPlayer) { player =>
            if player.points < winner.points then
                winner = player
        }
        print(s"Winner: ${winner.name}")
}

@main
def main =
    var newGame = HeartsGame(50)
    newGame.play()
end main