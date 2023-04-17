// DOES NOT WORK -- DEPRECATED

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

extension (c: Card) {
    def this(suit: CardSuit, value: Int) =
        this()
        c.data("suit") = suit
        c.data("value") = value
    override def toString: String =
        var repStr = ""
        if c.data("value") > 10 then
            repStr = s"${List('J','Q','K','A')(value-11)}${suit.toString()(0)}"
        else
            repStr = s"${value}${suit}"
        repStr
}

extension (p: Player) {
    override def this() =
        this()
        data("wonCards") = Stack()
    def scoreStack() =
        var score = 0
        for card <- data("wonCards").cards do
            if card.data("suit") == CardSuit.Hearts then
                score += 1
            else if card.data("suit") == CardSuit.Spades and card.data("value") == 12 then
                score += 13
        return score
    // move to game?
    def showInfo(game: HeartsGame) =
        super.showInfo()
        for player <- game.data("players") do
            print(s"${player.name} has won ${player.data("wonCards").count} cards")
        print(s"So far, the cards played have been ${game.data("playedCards")}")
}

extension (g: Game) {
    def this(endScore: Int = 50) =
        this()
        data("endScore") = endScore
        data("heartsBroken") = false
        var player1 = Player("Alice")
        var player2 = Player("Bob")
        var player3 = Player("Carrie")
        var player4 = Player("David")
        var player1.nextPlayer = player2
        var player2.nextPlayer = player3
        var player3.nextPlayer = player4
        var player4.nextPlayer = player1
        
        data("players") = List(player1, player2, player3, player4)
        data("firstPlayer") = data("players")(0)
        data("playedCards") = ListBuffer[PlayingCard]()
        
        for suit <- CardSuit.values do
            for value <- 2 to 14 do
                cards += PlayingCard(suit, value)
        data("deck") = Stack(cards)
        data("deck").shuffle()
    
    def beats(card: Card, other: Card, lead: CardSuit) =
        if card.data("suit") == lead and other.data("suit") != lead then
            return true
        else if card.data("suit") != lead and other.data("suit") == lead then
            return false
        else if card.data("value") > other.data("value") then
            return true
        else
            return false

    // override def round(condition: => Boolean)(body: => Unit): Unit = ???
    override def turn(player: HeartsPlayer): Unit = ???

    // ignoring player count deck modifications
    // game loop
    override def play() =
        round(true) {
            eachPlayer(data("firstPlayer")) { player =>
                if player.points >= data("endScore") then
                    endRounds
            }
            deal(13) from data("deck")
            //swapping would happen here
            //find player with 2 of clubs here
            var winningPlayer = data("players")(0)
            // eachPlayer(data("firstPlayer")) {
            for player <- data("players") do
                for card <- player.hand.cards do
                    if card.data("suit") == CardSuit.Clubs and card.data("value") == 2 then
                        winningPlayer = player
            // }
            round(data("firstPlayer").hand.count > 0) {
                eachPlayer(data("firstPlayer")) {
                    turn
                }
                data("playedCards") = ListBuffer[PlayingCard]()
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
                        index = readLine.toInt
                    } catch {
                        print("Please enter a number")
                    }
                    if currentPlayer.hand.cards(index).data("suit") != CardSuit.Hearts then
                        invalidChoice = true
                    else if data("heartsBroken") then
                        invalidChoice = true
                    else
                        if currentPlayer.hand.cards.filter(_.data("suit") != CardSuit.Hearts).isEmpty then
                            invalidChoice = true
                        print("Hearts cannot be led until a Heart or the QS have been won")
                winningCard = currentPlayer.hand.cards(index)
                data("playedCards") += winningCard
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
                            index = readLine.toInt
                        } catch {
                            print("Please enter a number")
                        }
                        if currentPlayer.hand.cards[index].data("suit") == winningCard.data("suit") then
                            invalidChoice = false
                        if currentPlayer.hand.cards.filter(_.data("suit") == winningCard.data("suit")).isEmpty then
                            invalidChoice = false
                        print("You must follow suit if possible")
                    playedCard = currentPlayer.hand.cards(index)
                    if playedCard.data("suit") == CardSuit.HEARTS then
                        data("heartsBroken") = true
                    else if playedCard.data("suit") == CardSuit.Spades and playedCard.data("value") == 12 then
                        data("heartsBroken") = true
                    data("playedCards") += playedCard
                    currentPlayer.hand.cards.remove(index)
                    if beats(playedCard, winningCard, winningCard.data("suit")) then
                        winningCard = playedCard
                        winningPlayer = currentPlayer
                }
                for wonCard <- data("playedCards") do
                    wonCard.discard(winningPlayer.data("wonCards"), false)
            }
            eachPlayer(data("firstPlayer")) { player =>
                var score = player.scoreStack()
                player.data("wonCards").discardAll(data("deck"), false)
                if score == 26 then
                    player.points -= 26
                    eachPlayer(data("firstPlayer")) { player =>
                        player.points += 26
                    }
                else
                    player.points += score
            }
            eachPlayer(data("firstPlayer")) { player =>
                print(s"${player.name} Score: ${player.points}")
            }
            data("deck").shuffle()
            data("heartsBroken") = false
        }
        var winner = HeartsPlayer("")
        winner.points = data("endScore") + 26
        eachPlayer(data("firstPlayer")) { player =>
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