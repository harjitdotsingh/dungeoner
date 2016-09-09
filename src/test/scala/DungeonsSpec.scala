import com.thinkaurelius.titan.core.{TitanFactory, TitanGraph}
import gremlin.scala._
import org.apache.tinkerpop.gremlin.process.traversal.P
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

trait InMemoryConnect {
  def connect(): TitanGraph = {
    import org.apache.commons.configuration.BaseConfiguration
    val conf = new BaseConfiguration()
    conf.setProperty("storage.backend","inmemory")
    TitanFactory.open(conf)
  }
}

/**
  * Dungeons and Dragons classic:
  *
  * Dungeon rooms are nodes, connected by edges.
  * Room nodes contain magic items, connected by edges marked with a minimum level and one or more character classes.
  *
  * Example:
  *
  * The Throneroom of Elizur contains a Frost Brand that requires a Level 7 Paladin to find it.
  */
class DungeonsSpec extends FlatSpec with Matchers with BeforeAndAfterAll with InMemoryConnect {

  val Name = Key[String]("name")
  val IsCursed = Key[String]("isCursed")
  val Room = Key[String]("room")

  val Paladin = Key[String]("paladin")
  val Thief = Key[String]("thief")
  val Monk = Key[String]("monk")
  val Wizard = Key[String]("wizard")
  val Barbarian = Key[String]("barbarian")

  val MinimumLevel = Key[Int]("minLevel")
  val ConnectsTo = Key[String]("connectsTo")
  val Contains = Key[String]("contains")

  var graph: TitanGraph = _

  "a 1st level wizard" should "find a Poison Potion in the Throneroom of Elizur" in {

    // This is a simple example because there is only one item of this level in the room.
    // So, only filter on level (kind of cheating)

    val items = graph.V.hasLabel("room").
      has(Name, "Throneroom of Elizur"). // find the throne room
      outE().hasLabel("contains"). // find contains edges
      has(MinimumLevel, P.lte(1)).inV().toList() // with minimum level

    items.size shouldBe 1
    items.head.value("name").toString shouldBe "Poison Potion"

  }

  "a 1st level paladin-monk" should "find a Trident of Fish Command in Halian's Library" in {

    // the library has two items available to a 1st level paladin
    // but must also be a thief and monk.
    // this time the test demonstrates only a paladin-monk,
    // so we're missing the thief and only find one of the two.

    val items = graph.V.hasLabel("room").
      has(Name, "Halian's Library"). // find the library node
      outE().hasLabel("contains"). // traverse to contains edges
      has(MinimumLevel, P.lte(1)). // filter level
      hasNot(Thief, "thief"). // filter thief
      inV().toList() // find inbound node

    items.size shouldBe 1
    items.head.value("name").toString shouldBe "Trident of Fish Command"

  }

  "a 5th level wizard-monk" should "find the Orb of Dragon Kind in the Crypt of Valinda" in {

    val items = graph.V.hasLabel("room").
      has(Name, "Crypt of Valinda"). // find the crypt
      outE().hasLabel("contains"). // traverse to contains edge
      has(MinimumLevel, P.lte(5)). // filter minimum level
      hasNot(Thief, "thief"). // filter on thief
      inV().toList() // traverse to inbound node

    items.size shouldBe 1
    items.head.value("name").toString shouldBe "The Orb of Dragon Kind"
  }

  "a 6th level thief" should "find an uncursed Sylvan Scimitar in Ulani's Cell" in {

    // first example
    val edges = graph.V.hasLabel("room").
      has(Name, "Ulani's Cell"). // find the cell node
      outE(). // traverse to edges
      hasLabel("contains"). // only contains edges
      has(MinimumLevel, P.lte(6)).toList() // with minimum level of 6

    edges.size shouldBe 3 // all items are available

    val allItems = graph.V.hasLabel("room").
      has(Name, "Ulani's Cell"). // find the cell node
      outE(). // traverse to edges
      hasLabel("contains"). // only contains edges
      has(MinimumLevel, P.lte(6)). // with minimum level of 6
      hasNot(Barbarian, "barbarian"). // that does not contain barbarian or paladin
      hasNot(Paladin, "paladin").inV().toList()

    allItems.size shouldBe 2 // one cursed item and one uncursed

    val uncursed = graph.V.hasLabel("room").
      has(Name, "Ulani's Cell"). // find the cell node
      outE(). // traverse to edges
      hasLabel("contains"). // only contains edges
      has(MinimumLevel, P.lte(6)). // with minimum level of 6
      hasNot(Barbarian, "barbarian"). // no barbarians or paladins
      hasNot(Paladin, "paladin").
      inV(). // traverse to inbound node
      hasNot(IsCursed, "yes").toList() // filter out cursed items

    uncursed.size shouldBe 1
    uncursed.head.value("name").toString shouldBe "Sylvan Scimitar"
  }

  override protected def beforeAll() = {
    graph = connect()

    // create room nodes
    val throneRoom = graph + ("room", Name -> "Throneroom of Elizur")
    val cell = graph + ("room", Name -> "Ulani's Cell")
    val crypt = graph + ("room", Name -> "Crypt of Valinda")
    val library = graph + ("room", Name -> "Halian's Library")

    // create bidirectional room edges
    throneRoom <-- "connectsTo" --> library
    throneRoom <-- "connectsTo" --> crypt
    crypt <-- "connectsTo" --> cell

    // create magic item nodes
    val poisonPotion = graph + ("magicItem", Name -> "Poison Potion", IsCursed -> "yes")
    val talismanGood = graph + ("magicItem", Name -> "Talisman of Pure Good", IsCursed -> "no")
    val thunderHammer = graph + ("magicItem", Name -> "Hammer of Thunderbolts", IsCursed -> "no")
    val frostBrand = graph + ("magicItem", Name -> "Frost Brand", IsCursed -> "no")
    val fishCommand = graph + ("magicItem", Name -> "Trident of Fish Command", IsCursed -> "no")
    val flaskCurse = graph + ("magicItem", Name -> "Flask of Curses", IsCursed -> "yes")
    val scarabDeath = graph + ("magicItem", Name -> "Scarab Of Death", IsCursed -> "yes")
    val deckMany = graph + ("magicItem", Name -> "Deck of Many Things", IsCursed -> "no")
    val orbDragon = graph + ("magicItem", Name -> "The Orb of Dragon Kind", IsCursed -> "no")
    val sunBlade = graph + ("magicItem", Name -> "Sun Blade", IsCursed -> "no")
    val rapier = graph + ("magicItem", Name -> "Rapier of Puncturing", IsCursed -> "yes")
    val scimitar = graph + ("magicItem", Name -> "Sylvan Scimitar", IsCursed -> "no")

    // create directed edges from rooms to magic items
    throneRoom --- ("contains", MinimumLevel -> 1, Wizard -> "wizard") --> poisonPotion
    throneRoom --- ("contains", MinimumLevel -> 3, Wizard -> "wizard", Paladin -> "paladin", Barbarian -> "barbarian", Monk -> "monk") --> talismanGood
    throneRoom --- ("contains", MinimumLevel -> 5, Paladin -> "paladin", Barbarian -> "barbarian") --> thunderHammer
    throneRoom --- ("contains", MinimumLevel -> 7, Paladin -> "paladin") --> frostBrand

    library --- ("contains", MinimumLevel -> 1, Paladin -> "paladin", Monk -> "monk") --> fishCommand
    library --- ("contains", MinimumLevel -> 1, Paladin -> "paladin", Thief -> "thief", Monk -> "monk") --> flaskCurse
    library --- ("contains", MinimumLevel -> 4, Wizard -> "wizard") --> scarabDeath

    crypt --- ("contains", MinimumLevel -> 3, Wizard -> "wizard", Thief -> "thief", Monk -> "monk") --> deckMany
    crypt --- ("contains", MinimumLevel -> 5, Wizard -> "wizard", Monk -> "monk") --> orbDragon

    cell --- ("contains", MinimumLevel -> 1, Paladin -> "paladin", Thief -> "thief", Barbarian -> "barbarian") --> sunBlade
    cell --- ("contains", MinimumLevel -> 5, Thief -> "thief") --> rapier
    cell --- ("contains", MinimumLevel -> 5, Thief -> "thief") --> scimitar
  }

  override protected def afterAll() = {
    graph.close()
  }
}
