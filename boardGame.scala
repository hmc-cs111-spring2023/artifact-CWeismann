package boardGame

import scala.collection.mutable.ListBuffer
import scala.util.Random.shuffle as shuffleList

class GameObject {
    // var name = n
    var isFaceUp = false
    var location = Location()
    var visibleTo = ListBuffer()
    
    def flip() = {
        this.isFaceUp = !this.isFaceUp
    }
    def moveTo(loc: Location) = {
        this.location = loc
    }
}
class Location extends GameObject {
    
}
class Stack(cardList: ListBuffer[Card]) extends Location {
    var cards = cardList
    var count = cardList.length

    def draw(player: Player): Unit = ???
    def draw(player: Player, num: Int): Unit =
        if this.cards == ListBuffer() then
            print("no cards")
        for i <- 0 to num.min(this.count) do
            this.cards(0).location = player.hand
            player.hand.cards += this.cards(0)
            this.cards = this.cards.tail
            this.count -= 1
    def discard(index: Int, loc: Location): Unit = ???
    def discard(index: Int, loc: Location, face: Boolean): Unit =
        var card = this.cards(index)
        card.discard(loc, face)
    def discardAll(loc: Location): Unit = ???
    def discardAll(loc: Location, face: Boolean): Unit =
        for card <- this.cards do
            card.isFaceUp = face
        this.moveTo(loc)
    def discardAll(stack: Stack): Unit = ???
    def discardAll(stack: Stack, face: Boolean): Unit =
        for card <- this.cards do
            card.isFaceUp = face
        this.moveTo(stack)
        stack.count += 1
        stack.cards ++= this.cards
        this.count = 0
        this.cards = ListBuffer()
    def shuffle(): Unit =
        shuffleList(this.cards)
}
class Board extends Location {
    
}
class Card extends GameObject {		// replace with another name?
    def discard(loc: Location): Unit = ???
    def discard(loc: Location, face: Boolean): Unit =
        this.isFaceUp = face
        this.moveTo(loc)
    def discard(stack: Stack): Unit = ???
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
    var nextPlayer = None
    // will be very difficult to implement
    var validActions = ListBuffer()

    def take(item: GameObject) = ???
        // item.location = this.hand
        // more...
    def draw(stack: Stack) = ???
    def draw(stack: Stack, count: Int) =
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
class Action {

}
class Move {

}
class Game {
    var players = ListBuffer()
}