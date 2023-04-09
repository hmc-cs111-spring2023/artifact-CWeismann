// from boardGame import *
// from enum import Enum

enum CardSuit {
    case Clubs
    case Hearts
    case Spades
    case Diamonds
}

class PlayingCard(suit: CardSuit, value: Int) extends Card {
    override def toString: String =
        var repStr = ""
        if value > 10 then
            repStr = s"${List('J','Q','K','A')[value-11]}${suit.toString()(0)}"
        else
            repStr = s"${value}${suit}"
        repStr
}
class CardGame extends Game {
    var cards = ListBuffer[PlayingCard]()
    for suit <- CardSuit do
        for value <- CardValue do
            cards += PlayingCard(suit, value)
    var deck = Stack(cards)
    deck.shuffle()
}
class HeartsPlayer extends Player {
    var wonCards = Stack(ListBuffer[PlayingCard])
    // var playedCard = Card()
    // def playCard() = ???
    def scoreStack() =
        score = 0
        for card <- self.wonCards.cards do
            if card.suit == CardSuit.HEARTS then
                score += 1
            else if card.suit == CardSuit.SPADES and card.value == CardValue.QUEEN then
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
    // self.playArea = Location()
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
    override def turn(player: Player): Unit = ???

    // ignoring player count deck modifications
    // game loop
    override def play() =
        round(true) {
            eachPlayer(firstPlayer) {
                if player.points >= endScore then
                    endRounds
            }
            deal(13) from deck
            //swapping would happen here
            //find player with 2 of clubs here
            var winningPlayer = players(0)
            eachPlayer(firstPlayer) {
                (player) => {
                    for card <- player.hand.cards do
                        if card.suit == CardSuit.Clubs and card.value == 2 then
                            winningPlayer = player
                }
            }
            round(firstPlayer.hand.count > 0) {
                eachPlayer(firstPlayer) {
                    (player: Player) => turn(player)
                }
            }
        }
        // while true:
        //     for player <- self.players do
        //         if player.points >= self.endScore then
        //             gameOver = true
        //         player.draw(self.deck, 13)		// magic num here for 4 players
        //     if gameOver then
        //         break
        //     // swapping would happen here
        //     // hand loop
        //     // finding player with 2 of clubs would happen here
        //     winningPlayer = self.players[0]
        //     for player <- self.players do
        //         for card <- player.hand.cards do
        //             if card.suit == CardSuit.CLUBS and card.value == CardValue.TWO then
        //                 winningPlayer = player
        //     while len(self.players[0].hand.cards) != 0                   
        //         self.playedCards = []
        //         currentPlayer = winningPlayer
        //         currentPlayer.showInfo(self)
        //         // should not be able to lead hearts if not broken
        //         while true
        //             index = int(input(s"Which card [index] would you like to lead? "))
        //             if currentPlayer.hand.cards[index].suit != CardSuit.HEARTS then
        //                 break
        //             else self.heartsBroken then
        //                 break
        //             else
        //                 if list(filter(lambda x: x.suit != CardSuit.HEARTS, currentPlayer.hand.cards)) == [] then
        //                     break
        //                 print("Hearts cannot be led until a heart or the QS have been won")
        //         winningCard = currentPlayer.hand.cards[index]
        //         self.playedCards += currentPlayer.hand.cards[index]
        //         currentPlayer.hand.cards.pop(index)
        //         for i <- range(len(self.players)-1) do
        //             currentPlayer = currentPlayer.nextPlayer
        //             currentPlayer.showInfo(self)
        //             // should be forced to follow if possible
        //             while true
        //                 index = int(input(s"Which card [index] would you like to play? "))
        //                 if currentPlayer.hand.cards[index].suit == winningCard.suit then
        //                     break
        //                 if list(filter(lambda x: x.suit == winningCard.suit, currentPlayer.hand.cards)) == [] then
        //                     break
        //                 print("You must follow suit if possible")
        //             playedCard = currentPlayer.hand.cards[index]
        //             if playedCard.suit == CardSuit.HEARTS then
        //                 self.heartsBroken = true
        //             else if playedCard.suit == CardSuit.SPADES and playedCard.value == CardValue.QUEEN:
        //                 self.heartsBroken = true
        //             self.playedCards += currentPlayer.hand.cards[index]
        //             currentPlayer.hand.cards.pop(index)
        //             if self.beats(self.playedCards[-1], winningCard, winningCard.suit) then
        //                 winningCard = self.playedCards[-1]
        //                 winningPlayer = currentPlayer
        //         for wonCard <- self.playedCards do
        //             wonCard.discard(winningPlayer.wonCards, false)
        //     for player <- self.players do
        //         score = player.scoreStack()
        //         player.wonCards.discardAll(self.deck, false)
        //         if score == 26 then
        //             player.points -= 26
        //             for p <- self.players do
        //                 p.points += 26
        //         else
        //             player.points += score
        //     for player <- self.players do
        //         print(s"${player.name} Score: ${player.points}")
        //     self.deck.shuffle()
        //     self.heartsBroken = false
        // var winner = HeartsPlayer()
        // winner.points = self.endScore + 26
        // for player <- self.players do
        //     if player.points < winner.points then
        //         winner = player
        // print(s"Winner: ${winner.name}")
}

@main
def main =
    var newGame = HeartsGame(50)
    newGame.play()
end main