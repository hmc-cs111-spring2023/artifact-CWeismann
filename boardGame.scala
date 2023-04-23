// package boardgame

import scala.collection.mutable.ListBuffer
import scala.util.Random.shuffle as shuffleList
import scala.io.StdIn.readLine
import scala.language.dynamics

// a GameObject is any non-player part of a game:
// a board, a location on a board, a card, a stack of cards, etc.
class GameObject extends Dynamic {
    // the following functions are part of the Dynamic behavior
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
// a Location is an area where objects can be placed
class Location extends GameObject {
    // var items = ListBuffer[+GameObject]()
    
}
// a Stack is a group of multiple cards, e.g.
// a deck, discard pile, hand of cards, etc.
class Stack[C <: Card](cardList: ListBuffer[C]) extends Location {
    def this() = this(ListBuffer[C]())
    var cards = cardList
    var count = cardList.length
    // override def toString: String = ???
    // draw card(s) from the Stack
    def draw(player: Player): Unit = draw(player, 1)
    def draw(player: Player, num: Int): Unit =
        if this.cards.isEmpty then
            print("no cards")
        for i <- 1 to num do
            cards(0).location = player.hand
            player.hand.cards += cards(0)
            player.hand.count += 1
            cards = cards.tail
            count -= 1
    // discard a card to the Stack
    def discard(index: Int, loc: Location): Unit = discard(index, loc, true)
    def discard(index: Int, loc: Location, face: Boolean): Unit =
        var card = this.cards(index)
        card.discard(loc, face)
    // discard this stack to another Location or Stack
    def discardAll(loc: Location, face: Boolean): Unit =
        for card <- this.cards do
            card.isFaceUp = face
        this.moveTo(loc)
    def discardAll(loc: Location): Unit = discardAll(loc, true)
    def discardAll(stack: Stack[C], face: Boolean): Unit =
        for card <- this.cards do
            card.isFaceUp = face
        this.moveTo(stack)
        stack.count += 1
        stack.cards ++= this.cards
        this.count = 0
        this.cards = ListBuffer()
    def discardAll(stack: Stack[C]): Unit = discardAll(stack, true)
    // find a specific card in the stack
    // TBA
    def findCard(): Unit = ???
    // shuffle the stack
    def shuffle(): Unit =
        this.cards = shuffleList(this.cards)
}
// a Board is a specific type of Location
// that has multiple interconnected Locations
// TBA
class Board extends Location {

}
// a Card is an object that carries some type of information,
// and can be drawn or discarded to Stacks
class Card extends GameObject {		// replace with another name?
    // discard a card to a Location or Stack
    def discard(loc: Location): Unit = discard(loc, true)
    def discard(loc: Location, face: Boolean): Unit =
        this.isFaceUp = face
        this.moveTo(loc)
    def discard(stack: Stack[Card]): Unit = discard(stack, true)
    def discard(stack: Stack[Card], face: Boolean): Unit =
        this.isFaceUp = face
        this.moveTo(stack)
        stack.cards += this
        stack.count += 1
    // needs better handling of face up/down?
    // possibly some separate Face class?
}
type AnyCard <: Card

// a Piece is a type of GameObject with some recognizable characteristic,
// like chess or checkers pieces
// TBA
class Piece extends GameObject {

}
// a Player is a someone who plays a Game
abstract class Player(_name: String) extends Dynamic {
    // var opponents = ListBuffer()
    // var teammates = ListBuffer()
    // the following functions are part of the Dynamic behavior
    var map = Map.empty[String, Any]
    def selectDynamic(name: String) =
        map get name getOrElse sys.error("method not found")
    def updateDynamic(name: String)(value: Any) =
        map += name -> value
    def applyDynamic(name: String)(args: Any*) =
        s"method '$name' called with arguments ${args.mkString("'", "', '", "'")}"
    def applyDynamicNamed(name: String)(args: (String, Any)*) =
        s"method '$name' called with arguments ${args.mkString("'", "', '", "'")}"
    var name = _name
    var hand = Stack[Card]()
    var playArea = Location()
    var points = 0
    var nextPlayer: Player
    // the list of valid actions that can be taken by this player
    // TBA
    var validActions = ListBuffer()

    // place an object in your hand or play area
    // TBA
    def take(item: GameObject) = ???
        // item.location = this.hand
        // more...
    // draw card(s) from a stack
    def draw(stack: Stack[Card]): Unit = draw(stack, 1)
    def draw(stack: Stack[Card], num: Int): Unit =
        for i <- 1 to num do
            stack.cards(0).location = this.hand
            hand.cards += stack.cards(0)
            hand.count += 1
            stack.cards = stack.cards.tail
            stack.count -= 1
    // show the players' known info
    // overloaded in subclasses
    def showInfo(game: Game[Player]): Unit =
        print(s"${this.name}'s known information")
        print(s"hand = ${this.hand.cards}")
}
// type AnyPlayer <: Player
// a Game is a collection of Players and Game objects that makes up a game
class Game[P <: Player] extends Dynamic {
    object ContinueException extends Exception
    object BreakException extends Exception
    // the following functions are part of the Dynamic behavior
    var map = Map.empty[String, Any]
    def selectDynamic(name: String) =
        map get name getOrElse sys.error("method not found")
    def updateDynamic(name: String)(value: Any) =
        map += name -> value
    def applyDynamic(name: String)(args: Any*) =
        s"method '$name' called with arguments ${args.mkString("'", "', '", "'")}"
    def applyDynamicNamed(name: String)(args: (String, Any)*) =
        s"method '$name' called with arguments ${args.mkString("'", "', '", "'")}"

    var players = ListBuffer[P]()

    // ends an eachPlayer loop early
    def skipRest = throw BreakException
    // performs an action on each player
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

    // restarts a round loop
    def newRound = throw ContinueException
    // ends a round loop early
    def endRounds = throw BreakException
    // loops for a number of rounds equal to count
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
    // loops while condition is met
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
    // ends a turn loop early
    def endTurn = throw BreakException
    // just a stub function here
    // TBA
    def turn(player: P)(body: => Unit): Unit = 
        try {
            body
        } catch {
            case BreakException => {}
        }
    // prompts the user to input something, then verifies
    // their input meets some condition
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
    // deals count number of cards from a stack to each player
    class deal(count: Int) {
        def from(deck: Stack[Card]): Unit =
            for player <- players do
                player.draw(deck, count)
    }
    // plays the game
    // overloaded in subclasses
    def play(): Unit = round(false) {}
}