class GameObject {
    // var name = n
    var isFaceUp = False
    var location = None
    var visibleTo = List()
    
    def flip(): Unit = {
        this.isFaceUp = !this.isFaceUp
    }
    def moveTo(loc: Location): Unit = {
        this.location = loc
    }
}
class Location extends GameObject {
    
}
class Stack(cardList: List(Card)) extends Location {
    var cards = cardList
    var count = len(cardList)

    def draw(player: Player): Unit = ???
    def draw(player: Player, num: Int): Unit =
        if this.cards == List() then
            print("no cards")
        for i <- 0 to num.min(this.count) do
            this.cards(0).location = player.hand
            player.hand.cards += [this.cards(0)]
            this.cards = this.cards.tail
            this.count -= 1
    def discard(index: Int, loc: Location): Unit = ???
    def discard(index: Int, loc: Location, face: Boolean): Unit =
        card = this.cards(index)
        card.discard(loc, face)
    def discardAll(loc: Location): Unit = ???
    def discardAll(loc: Location, face: Boolean): Unit =
        for card <- this.cards do
            card.isFaceUp = face
        this.moveTo(loc)
        if type(loc) == Stack then
            loc.count += 1
            loc.cards += this.cards
        this.count = 0
        this.cards = List()
    def shuffle(): Unit =
        for i <- range(len(this.cards)-1, 0, -1) do
            j = randint(0, i + 1)
            this.cards(i), this.cards(j) = this.cards(j), this.cards(i)
}
class Board extends Location {
    
}
class Card extends GameObject {		// replace with another name?
    def discard(loc: Location): Unit = ???
    def discard(loc: Location, face=True): Unit =
        this.isFaceUp = face
        this.moveTo(loc)
        if type(loc) == Stack then
            loc.cards += (this)
            loc.count += 1
    // needs better handling of face up/down?
    // possibly some separate Face class?
}
class Piece extends GameObject {

}
class Player(n: String) {
    var name = n
    // var opponents = List()
    // var teammates = List()
    var hand = Stack(List())
    var playArea = Location()
    var points = 0
    var nextPlayer = None
    // will be very difficult to implement
    var validActions = List()

    def take(item: GameObject): Unit =
        item.location = this.hand
        pass // more...
    def draw(stack: Stack): Unit = ???
    def draw(stack: Stack, count: Int): Unit =
        for i <- range(count) do
            stack.cards(0).location = this.hand
            this.hand.cards += [stack.cards(0)]
            stack.cards = stack.cards.tail
            stack.count -= 1
    def showInfo(): Unit =
        // shows everything that the player can see
        print(s"${this.name}'s known information")
        print(s"hand = ${this.hand.cards}")
}
class Action = ???
class Move = ???
class Game {
    var players = List()
}