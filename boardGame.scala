import scala.collection.mutable.ListBuffer
import scala.util.Random.shuffle as shuffleList

class GameObject {
    // var name = n
    var isFaceUp = false
    var location = this
    var visibleTo = ListBuffer[Player]()
    
    def flip() =
        this.isFaceUp = !this.isFaceUp
    def moveTo(loc: Location) =
        this.location = loc
    
}
class Location extends GameObject {
    // var items = ListBuffer[GameObject]()
    
}
class Stack(cardList: ListBuffer[Card]) extends Location {
    var cards = cardList
    var count = cardList.length
    // alternative empty constructor
    def draw(player: Player): Unit = draw(player, 1)
    def draw(player: Player, num: Int): Unit =
        if this.cards == ListBuffer() then
            print("no cards")
        for i <- 0 to num.min(this.count) do
            this.cards(0).location = player.hand
            player.hand.cards += this.cards(0)
            this.cards = this.cards.tail
            this.count -= 1
    def discard(index: Int, loc: Location): Unit = discard(index, loc, true)
    def discard(index: Int, loc: Location, face: Boolean): Unit =
        var card = this.cards(index)
        card.discard(loc, face)
    def discardAll(loc: Location, face: Boolean): Unit =
        for card <- this.cards do
            card.isFaceUp = face
        this.moveTo(loc)
    def discardAll(loc: Location): Unit = discardAll(loc, true)
    def discardAll(stack: Stack, face: Boolean): Unit =
        for card <- this.cards do
            card.isFaceUp = face
        this.moveTo(stack)
        stack.count += 1
        stack.cards ++= this.cards
        this.count = 0
        this.cards = ListBuffer()
    def discardAll(stack: Stack): Unit = discardAll(stack, true)
    def findCard(): Unit = ???
    def shuffle(): Unit =
        shuffleList(this.cards)
}
class Board extends Location {
    
}
class Card extends GameObject {		// replace with another name?
    def discard(loc: Location): Unit = discard(loc, true)
    def discard(loc: Location, face: Boolean): Unit =
        this.isFaceUp = face
        this.moveTo(loc)
    def discard(stack: Stack): Unit = discard(stack, true)
    def discard(stack: Stack, face: Boolean): Unit =
        this.isFaceUp = face
        this.moveTo(stack)
        stack.cards += this
        stack.count += 1
    // needs better handling of face up/down?
    // possibly some separate Face class?
}
class Piece extends GameObject {

}
class Player(n: String) {
    var name = n
    // var opponents = ListBuffer()
    // var teammates = ListBuffer()
    var hand = Stack(ListBuffer())
    var playArea = Location()
    var points = 0
    var nextPlayer = this
    // will be very difficult to implement
    var validActions = ListBuffer()

    def take(item: GameObject) = ???
        // item.location = this.hand
        // more...
    def draw(stack: Stack): Unit = draw(stack, 1)
    def draw(stack: Stack, count: Int): Unit =
        for i <- 0 to count do
            stack.cards(0).location = this.hand
            this.hand.cards += stack.cards(0)
            stack.cards = stack.cards.tail
            stack.count -= 1
    def showInfo(): Unit =
        // shows everything that the player can see
        print(s"${this.name}'s known information")
        print(s"hand = ${this.hand.cards}")
}
class Game {
    object ContinueException extends Exception
    object BreakException extends Exception

    var players = ListBuffer[Player]()

    def eachPlayer(startingWith: Player)(body: => Unit): Unit =
        var player = startingWith
        do {
            body
            player = player.nextPlayer
        } while (player != startingWith)

    def newRound = throw ContinueException
    def endRounds = throw BreakException
    // reimplement using second definition and an iterator?
    def round(count: Int)(body: => Unit): Unit =
        for i <- 0 to count do
            try {
                body
            } catch {
                case ContinueException => round(count-i)(body)
                case BreakException => round(0)(body)
            }
    def round(condition: => Boolean)(body: => Unit): Unit =
        while (condition) {
            try {
                body
            } catch {
                case ContinueException => round(condition)(body)
                case BreakException => round(false)(body)
            }
        }
    
    def endTurn = throw BreakException
    def turn(player: Player)(body: => Unit): Unit = 
        try {
            body
        } catch {
            case BreakException => {}
        }
    class deal(count: Int) {
        def from(deck: Stack): Unit =
            for player <- players do
                player.draw(deck, count)
    }
    def play(): Unit = round(false) {}
}