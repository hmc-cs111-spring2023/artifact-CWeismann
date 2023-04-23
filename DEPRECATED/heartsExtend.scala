// DOES NOT WORK -- DEPRECATED

// from boardGame import *
// from enum import Enum

// enum CardSuit {
//     case Clubs
//     case Hearts
//     case Spades
//     case Diamonds
// }

// enum CardValue (value: Int) {
//     case Two extends CardValue(2)
//     case Three extends CardValue(3)
//     // etc.
// }

extension (c: Card) {
    def this(suit: Char, value: Int) =
        this()
        c.suit = suit
        c.value = value
    override def toString: String =
        var repStr = ""
        if c.value > 10 then
            repStr = s"${List('J','Q','K','A')(c.value-11)}${c.suit}"
        else
            repStr = s"${c.value}${c.suit}"
        repStr
}

extension (p: Player) {
    override def this(_name: String) =
        this(_name: _name)
        p.wonCards = Stack()
    def scoreStack() =
        var score = 0
        for card <- p.wonCards.cards do
            if card.suit == 'H' then
                score += 1
            else if card.suit == 'S' and card.value == 12 then
                score += 13
        score
    // move to game?
    def showInfo(game: Game) =
        super.showInfo()
        for player <- game.players do
            print(s"${player.name} has won ${player.wonCards.count} cards")
        print(s"So far, the cards played have been ${game.playedCards}")
}

extension (g: Game) {
    def this(endScore: Int = 50) =
        this()
        g.endScore = endScore
        g.heartsBroken = false
        g.player1 = Player("Alice")
        g.player2 = Player("Bob")
        g.player3 = Player("Carrie")
        g.player4 = Player("David")
        g.player1.nextPlayer = g.player2
        g.player2.nextPlayer = g.player3
        g.player3.nextPlayer = g.player4
        g.player4.nextPlayer = g.player1
        
        g.players = List(g.player1, g.player2, g.player3, g.player4)
        g.firstPlayer = g.players(0)
        g.playedCards = ListBuffer[Card]()
        
        var cards = ListBuffer[Card]
        for suit <- List('C','H','S','D') do
            for value <- 2 to 14 do
                cards += Card(suit, value)
        g.deck = Stack(cards)
        g.deck.shuffle()
    
    def beats(card: Card, other: Card, lead: Char): Boolean =
        if card.suit == lead and other.suit != lead then
            return true
        else if card.suit != lead and other.suit == lead then
            return false
        else if card.value > other.value then
            return true
        else
            return false

    // override def round(condition: => Boolean)(body: => Unit): Unit = ???
    override def turn(player: Player): Unit = ???

    // ignoring player count deck modifications
    // game loop
    override def play() =
        g.round(true) {
            g.endRounds
            // eachPlayer(firstPlayer) { player =>
            //     if player.points >= endScore then
            //         endRounds
            // }
            // deal(13) from deck
            // //swapping would happen here
            // //find player with 2 of clubs here
            // var winningPlayer = players(0)
            // // eachPlayer(firstPlayer) {
            // for player <- players do
            //     for card <- player.hand.cards do
            //         if card.suit == 'C' and card.value == 2 then
            //             winningPlayer = player
            // // }
            // round(firstPlayer.hand.count > 0) {
            //     eachPlayer(firstPlayer) {
            //         turn
            //     }
            //     playedCards = ListBuffer[PlayingCard]()
            //     var currentPlayer = winningPlayer
            //     currentPlayer.showInfo()
            //     // var index = prompt "Which card [index] would you like to lead? "
            //     //             where 
            //     // REPLACE BELOW WITH PROMPT
            //     var index = 0
            //     var invalidChoice = true
            //     while invalidChoice do
            //         print("Which card [index] would you like to lead? ")
            //         try {
            //             index = readLine.toInt
            //         } catch {
            //             print("Please enter a number")
            //         }
            //         if currentPlayer.hand.cards(index).suit != 'H' then
            //             invalidChoice = true
            //         else if heartsBroken then
            //             invalidChoice = true
            //         else
            //             if currentPlayer.hand.cards.filter(_.suit != 'H').isEmpty then
            //                 invalidChoice = true
            //             print("Hearts cannot be led until a Heart or the QS have been won")
            //     winningCard = currentPlayer.hand.cards(index)
            //     playedCards += winningCard
            //     currentPlayer.hand.cards.remove(index)
            //     eachPlayer(currentPlayer.nextPlayer) { player =>
            //         if player == winningPlayer then
            //             skipRest
            //         player.showInfo(this)
            //         // should be forced to follow if possible
            //         // some prompt nonsense here instead of this
            //         index = 0
            //         invalidChoice = true
            //         while invalidChoice do
            //             print("Which card [index] would you like to play? ")
            //             try {
            //                 index = readLine.toInt
            //             } catch {
            //                 print("Please enter a number")
            //             }
            //             if currentPlayer.hand.cards[index].suit == winningCard.suit then
            //                 invalidChoice = false
            //             if currentPlayer.hand.cards.filter(_.suit == winningCard.suit).isEmpty then
            //                 invalidChoice = false
            //             print("You must follow suit if possible")
            //         playedCard = currentPlayer.hand.cards(index)
            //         if playedCard.suit == 'H' then
            //             heartsBroken = true
            //         else if playedCard.suit == 'S' and playedCard.value == 12 then
            //             heartsBroken = true
            //         playedCards += playedCard
            //         currentPlayer.hand.cards.remove(index)
            //         if beats(playedCard, winningCard, winningCard.suit) then
            //             winningCard = playedCard
            //             winningPlayer = currentPlayer
            //     }
            //     for wonCard <- playedCards do
            //         wonCard.discard(winningPlayer.wonCards, false)
            // }
            // eachPlayer(firstPlayer) { player =>
            //     var score = player.scoreStack()
            //     player.wonCards.discardAll(deck, false)
            //     if score == 26 then
            //         player.points -= 26
            //         eachPlayer(firstPlayer) { player =>
            //             player.points += 26
            //         }
            //     else
            //         player.points += score
            // }
            // eachPlayer(firstPlayer) { player =>
            //     print(s"${player.name} Score: ${player.points}")
            // }
            // deck.shuffle()
            // heartsBroken = false
        }
        var winner = Player("")
        winner.points = endScore + 26
        eachPlayer(firstPlayer) { player =>
            if player.points < winner.points then
                winner = player
        }
        print(s"Winner: ${winner.name}")
}

@main
def main =
    var newGame = Game(50)
    newGame.play()
end main