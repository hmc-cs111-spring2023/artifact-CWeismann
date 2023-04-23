// package hearts

// import boardgame.*
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scala.language.dynamics
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

class PlayingCard(val _suit: CardSuit, val _value: Int) extends Card {
    this.suit = _suit
    this.value = _value
    override def toString: String =
        var repStr = ""
        if _value > 10 then
            repStr = s"${List('J','Q','K','A')(_value-11)}${_suit.toString()(0)}"
        else
            repStr = s"${_value}${_suit.toString()(0)}"
        repStr
}
class CardGame[P <: Player] extends Game[P] {
    var cards = ListBuffer[Card]()
    for suit <- CardSuit.values do
        for value <- 2 to 14 do
            cards += PlayingCard(suit, value)
    var deck = Stack[Card](cards)
    deck.shuffle()
}
class HeartsPlayer(_name: String) extends Player(_name: String) {
    // var name = _name
    var wonCards = Stack[PlayingCard]()
    // var nextPlayer: HeartsPlayer = HeartsPlayer("")
    // var playingCardHand = Stack[PlayingCard]()
    var nextPlayer = this
    // def this(player: Player) =
    //     var name = player.name
    //     var wonCards = Stack[PlayingCard]()
    //     var nextPlayer = 
    // var playedCard = Card()
    // def playCard() = ???
    def scoreStack() =
        var score = 0
        for card <- this.wonCards.cards do
            if card.suit == CardSuit.Hearts then
                score += 1
            else if card.suit == CardSuit.Spades && card.value == 12 then
                score += 13
        score
    // move to game?
    def showInfo(game: HeartsGame) =
        // super.showInfo(game)
        println(s"${this.name}'s known information")
        println(s"hand = ${hand.cards}")
        for player <- game.players do
            println(s"${player.name} has won ${player.wonCards.count} cards")
        print(s"So far, the cards played have been ")
        for card <- game.playedCards do
            print(s"${card} ")
        print("\n")
}
// class HeartsCard extends PlayingCard {

// }
class HeartsGame(endScore: Int) extends CardGame[HeartsPlayer] {
    var heartsBroken = false
    // this.playArea = Location()
    var player1 = HeartsPlayer("Alice")
    var player2 = HeartsPlayer("Bob")
    var player3 = HeartsPlayer("Carrie")
    var player4 = HeartsPlayer("David")
    player1.nextPlayer = player2
    player2.nextPlayer = player3
    player3.nextPlayer = player4
    player4.nextPlayer = player1
    // necessary?
    this.players = ListBuffer(player1, player2, player3, player4)
    val firstPlayer = players(0)
    var playedCards = ListBuffer[Card]()
    
    def beats(card: Card, other: Card, lead: Any): Boolean =
        if card.suit == lead && other.suit != lead then
            return true
        else if card.suit != lead && other.suit == lead then
            return false
        else if card.value.asInstanceOf[Int] > other.value.asInstanceOf[Int] then
            return true
        else
            return false

    // override def round(condition: => Boolean)(body: => Unit): Unit = ???
    // override def turn(player: HeartsPlayer): Unit = ???

    // ignoring player count deck modifications
    // game loop
    override def play() =
        var q = true
        round(q) {
            q = false
            eachPlayer(firstPlayer) { player =>
                if player.points >= endScore then
                    endRounds
            }
            deal(13) from deck
            // swapping would happen here
            // find player with 2 of clubs here
            var winningPlayer = players(0)
            for player <- players do
                for card <- player.hand.cards do
                    if card.toString == "2C" then
                        winningPlayer = player
            round(firstPlayer.hand.count > 0) {
                // delete next line
                firstPlayer.hand.count = 0
                playedCards = ListBuffer[Card]()
                // var currentPlayer: HeartsPlayer = winningPlayer
                winningPlayer.showInfo(this)
        //         // var index = prompt "Which card [index] would you like to lead? "
        //         //             where 
        //         // REPLACE BELOW WITH PROMPT
                var index = 0
                var invalidChoice = true
                while invalidChoice do
                    print("\nWhich card [index] would you like to lead? ")
                    try {
                        index = readLine.toInt
                    } catch {
                        case _ => print("\nPlease enter a number")
                    }
                    try {
                        var card = winningPlayer.hand.cards(index)
                    } catch {
                        case _ => print("\nYou do not have that many cards in your hand")
                    }
                    if winningPlayer.hand.cards(index).suit != CardSuit.Hearts then
                        invalidChoice = false
                    else if heartsBroken then
                        invalidChoice = false
                    else
                        if winningPlayer.hand.cards.filter(_.charAt(1) != "H").isEmpty then
                            invalidChoice = false
                        print("\nHearts cannot be led until a Heart or the QS have been won")
                var winningCard = winningPlayer.hand.cards(index)
                playedCards += winningCard
                winningPlayer.hand.cards.remove(index)
                for player <- players do
                // eachPlayer(winningPlayer.nextPlayer) { player =>
                    if player == winningPlayer then
                        skipRest
                    // HeartsPlayer(player).showInfo(this)
                    println(s"${player.name}'s known information")
                    println(s"hand = ${player.hand.cards}")
                    for p <- players do
                    // eachPlayer(firstPlayer) { p =>
                        println(s"${p.name} has won ${p.wonCards.count} cards")
                    // }
                    println(s"So far, the cards played have been ${playedCards}")
                    // should be forced to follow if possible
                    // some prompt nonsense here instead of this
                    index = 0
                    invalidChoice = true
                    while invalidChoice do
                        print("Which card [index] would you like to play? ")
                        try {
                            index = readLine.toInt
                        } catch {
                            case _ => print("Please enter a number")
                        }
                        try {
                            var card = winningPlayer.hand.cards(index)
                        } catch {
                            case _ => print("\nYou do not have that many cards in your hand")
                        }
                        if player.hand.cards(index).suit == winningCard.suit then
                            invalidChoice = false
                        if player.hand.cards.filter(_.suit == winningCard.suit).isEmpty then
                            invalidChoice = false
                        if invalidChoice then
                            println("You must follow suit if possible")
                    var playedCard = player.hand.cards(index)
                    if playedCard.suit == CardSuit.Hearts then
                        heartsBroken = true
                    else if playedCard.suit == CardSuit.Spades && playedCard.value == 12 then
                        heartsBroken = true
                    playedCards += playedCard
                    player.hand.cards.remove(index)
                    if beats(playedCard, winningCard, winningCard.suit) then
                        winningCard = playedCard
                        winningPlayer = player
                // }
                for wonCard <- playedCards do
                    wonCard.discard(winningPlayer.wonCards, false)
            }
            for player <- players do
                var score = player.scoreStack()
                player.wonCards.discardAll(deck, false)
                if score == 26 then
                    player.points -= 26
                    eachPlayer(firstPlayer) { player =>
                        player.points += 26
                    }
                else
                    player.points += score
            eachPlayer(firstPlayer) { player =>
                print(s"${player.name} Score: ${player.points}")
            }
            deck.shuffle()
            heartsBroken = false
        }
        var winner: Player = firstPlayer
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