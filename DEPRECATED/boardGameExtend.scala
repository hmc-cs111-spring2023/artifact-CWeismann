import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.util.Random.shuffle as shuffleList
import scala.io.StdIn.readLine

class GameObject extends Dynamic {
    var map = Map.empty[String, Any]
    def selectDynamic(name: String) =
        map get name getOrElse sys.error("method not found")
    def updateDynamic(name: String)(value: Any) =
        map += name -> value
    def applyDynamic(name: String)(args: Any*) =
        s"method '$name' called with arguments ${args.mkString("'", "', '", "'")}"
    def applyDynamicNamed(name: String)(args: (String, Any)*) =
        s"method '$name' called with arguments ${args.mkString("'", "', '", "'")}"
    var isFaceUp = false
    var location = this
    var visibleTo = ListBuffer[Player]()
    
    def flip() =
        isFaceUp = !isFaceUp
    def moveTo(loc: Location) =
        location = loc
    
}
class Location extends GameObject {
    // var items = ListBuffer[+GameObject]()
    
}
class Stack(cardList: ListBuffer[Card]) extends Location {
    def this() = this(ListBuffer[Card]())
    var cards = cardList
    var count = cardList.length
    // alternative empty constructor
    def draw(player: Player): Unit = draw(player, 1)
    def draw(player: Player, num: Int): Unit =
        if this.cards == ListBuffer() then
            print("no cards")
        for i <- 0 to num do
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
// type AnyCard <: Card

class Piece extends GameObject {

}
class Player(_name: String) extends Dynamic {
    var map = Map.empty[String, Any]
    def selectDynamic(name: String) =
        map get name getOrElse sys.error("method not found")
    def updateDynamic(name: String)(value: Any) =
        map += name -> value
    def applyDynamic(name: String)(args: Any*) =
        s"method '$name' called with arguments ${args.mkString("'", "', '", "'")}"
    def applyDynamicNamed(name: String)(args: (String, Any)*) =
        s"method '$name' called with arguments ${args.mkString("'", "', '", "'")}"
    // var opponents = ListBuffer()
    // var teammates = ListBuffer()
    var name = _name
    var hand = Stack()
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
    def showInfo(game: Game): Unit =
        // shows everything that the player can see
        print(s"${this.name}'s known information")
        print(s"hand = ${this.hand.cards}")
}
// type AnyPlayer <: Player
class Game extends Dynamic {
    var map = Map.empty[String, Any]
    def selectDynamic(name: String) =
        map get name getOrElse sys.error("method not found")
    def updateDynamic(name: String)(value: Any) =
        map += name -> value
    def applyDynamic(name: String)(args: Any*) =
        s"method '$name' called with arguments ${args.mkString("'", "', '", "'")}"
    def applyDynamicNamed(name: String)(args: (String, Any)*) =
        s"method '$name' called with arguments ${args.mkString("'", "', '", "'")}"
    object ContinueException extends Exception
    object BreakException extends Exception

    var players = ListBuffer[Player]()

    def skipRest = throw BreakException
    def eachPlayer(startingWith: Player)(body: Player => Unit): Unit =
        try {
            var player = startingWith
            body(player)
            player = player.nextPlayer
            while (player != startingWith) {
                body(player)
                player = player.nextPlayer
            }
        } catch {
            case BreakException => {}
        }

    def newRound = throw ContinueException
    def endRounds = throw BreakException
    // reimplement using second definition and an iterator?
    def round(count: Int)(body: => Unit): Unit =
        var i = 0
        try {
            while i < count do
                try {
                    body
                    i += 1
                } catch {
                    case ContinueException => i += 1
                }
        } catch {
            case BreakException => i = count
        }
    def round(condition: => Boolean)(body: => Unit): Unit =
        try {
            while (condition) {
                try {
                    body
                } catch {
                    case ContinueException => round(condition)(body)
                }
            }
        } catch {
            case BreakException => {}
        }
    // not sure about this
    def endTurn = throw BreakException
    def turn(player: Player)(body: => Unit): Unit = 
        try {
            body
        } catch {
            case BreakException => {}
        }
    class prompt(query: String) {
        def where(condition: String => Boolean): String =
            var name = ""
            println(query)
            name = readLine()
            while (!condition(name)) {
                println(query)
                name = readLine()
            }
            name
    }

    class deal(count: Int) {
        def from(deck: Stack): Unit =
            for player <- players do
                player.draw(deck, count)
    }
    def play(): Unit = round(false) {}
}