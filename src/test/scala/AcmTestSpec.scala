import gremlin.scala.{Vertex, _}
import org.apache.tinkerpop.gremlin.process.traversal.Path
import org.apache.tinkerpop.gremlin.structure.Direction
import org.janusgraph.core.JanusGraphFactory
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

import scala.collection.JavaConverters._


trait InMemoryConnectScala {
  def connect(): ScalaGraph = {
    import org.apache.commons.configuration.BaseConfiguration
    val conf = new BaseConfiguration()
    conf.setProperty("storage.backend", "inmemory")
    JanusGraphFactory.open(conf).asScala
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
sealed trait Edges

case class EmptyEdge() extends Edges

case class StartingVertex(v: Vertex) extends Edges

case class EndingVertex(v: Vertex) extends Edges


case class EdgeOut(val acm: Option[String], vertex: Option[List[EndingVertex]]) extends Edges

class AcmTestSpec extends FlatSpec with Matchers with BeforeAndAfterAll with InMemoryConnectScala {


  val atomEnergy = Key[java.util.List[String]]("atomEnergy")
  val dispOnlyTo = Key[Set[String]]("dispOnlyTo")
  val dissemCtrls = Key[Set[String]]("dissemCtrls")

  val fgiProtect = Key[Set[String]]("fgiProtect")

  val fMissions = Key[Set[String]]("fMissions")
  val dissemCountries = Key[Set[String]]("dissemCountries")
  val fSarId = Key[Set[String]]("fSarId")
  val fSciCtrls = Key[Set[String]]("fSciCtrls")
  val relTo = Key[Set[String]]("relTo")
  val macs = Key[Set[String]]("macs")
  val coi = Key[Set[String]]("coi")
  val coiCtrls = Key[Set[String]]("coiCtrls")

  val dispNM = Key[Set[String]]("dispNM")

  val fAtomEnergy = Key[Set[String]]("fAtomEnergy")

  val fMacs = Key[Set[String]]("fMacs")

  val nonIC = Key[Set[String]]("nonIC")
  val fOCOrg = Key[Set[String]]("fOCOrg")

  val fAccms = Key[Set[String]]("fAccms")
  val ocAttribs = Key[Set[String]]("ocAttribs")
  val regions = Key[Set[String]]("regions")
  val clerances = Key[Set[String]]("fClearance")


  val orgs = Key[Set[String]]("orgs")
  val missions = Key[Set[String]]("missions")
  val fShare = Key[Set[String]]("fShare")
  val fRegions = Key[Set[String]]("fRegions")
  val ownerProd = Key[Set[String]]("ownerProd")


  val Name = Key[String]("name")
  val Type = Key[String]("type")

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

  val acmProp = Key[String]("acmPermission")


  var graph: ScalaGraph = _



  "Pull Permissions from Edges" should "ReturnPath" in {
    // Will not have to allow out(), you will have to force users to do a Out(E).inV()
    val edges = graph.V.hasLabel("room").
      has(Type, "room").outE("contains")

    val path = graph.V.hasLabel("room").
      has(Type, "room").outE().hasLabel("connectsTo").inV().path().by().l()


    println("Without By" + graph.V.hasLabel("room").
      has(Type, "room").outE().hasLabel("connectsTo").inV().path().by().by("acmPermission").l())

    println("*** EdgePath" + path + ": Size" + path.size)


    val results = path.map(reducePath)

    println("R" + results.toString())


    assert (results.size >0)


  }

  "Redact Paths for Which you don't have permission " should "ReturnPath" in {
    // Will not have to allow out(), you will have to force users to do a Out(E).inV()



    // Pulls all Nodes and Edges which have the label and Emit AcmPermissions Edges with an value of 8 means they are unaccessible
    // In our Sample ThroneRoom has an edge with acmPermission of 8 and then has an path from it to WeedRoom.
      // The method redact will Remove the  nodes which have an acmPermission of 8.
    val ppath = graph.V.hasLabel("room").
      has(Type, "room").outE().hasLabel("connectsTo").inV().path().by().by("acmPermission").l()
    //println("*** ppath" + ppath + ": Size" + ppath.size)

    val path = graph.V.hasLabel("room").
      has(Type, "room").outE().hasLabel("connectsTo").inV().path().by().by("acmPermission").l()

    //println("*** EdgePath" + path + ": Size" + path.size)

    //Get the starting nodes which have an edge with acmPermission 8
    // In our Sample this should be 1 only
    val p = path.map(pathToBeRedacted).flatten

    //println("*** Path to Remove" + p + ": Size" + p.size)


    p.size shouldBe 1

    // Remove all the nodes from the Path which start from the Node and have a permission of 8
    val generatedResults = path.map(pp => removeRedacted(p.head, pp))


    // Based on the test data the node with name Halians Library is the one which will be removed from the path
    p.head.property(Name).value() shouldBe("Halian's Library")


    // Compare the size of the path list from the original List. There should be one Node which should be removed.
    // hance the difference between the size would be one.
    val filteredList = generatedResults.filter(l=>l.size >0)
    path.size  - filteredList.size shouldBe 1


  }







  def removeRedacted(vv: Vertex, p: Path): Seq[Path] = {


    var seq: Seq[Path] = Seq.empty
    val l = p.objects()

    if (l.size() == 3) {


      val v = l.get(0).asInstanceOf[Vertex]
      println("Checking for Removal" + v.valueMap,vv.valueMap)
      if (v.id().toString != (vv.id.toString)) {
        seq :+= p
      }
    }

    seq
  }


  def pathToBeRedacted(p: Path): Seq[Vertex] = {

    val l = p.objects()

    var seq: Seq[Vertex] = Seq.empty

    if (l.size() == 3) {
      val e = l.get(1).asInstanceOf[String]
      println(l.get(0).asInstanceOf[Vertex].valueMap,e,l.get(2).asInstanceOf[Vertex].valueMap)
      if (e.equalsIgnoreCase("8")) {
        //println("ignore",l.get(2).asInstanceOf[Vertex].valueMap)
        seq :+= l.get(2).asInstanceOf[Vertex]
      }
    }

    seq

  }


  "Pull Permissions from Edges With By" should "ReturnPath" in {
    // Will not have to allow out(), you will have to force users to do a Out(E).inV()
    val edges = graph.V.hasLabel("room").
      has(Type, "room").outE("contains")

    val path = graph.V.hasLabel("room").
      has(Type, "room").outE().inV().path().by().l()


    println("+++++Without By" + graph.V.hasLabel("room").
      has(Type, "room").outE().inV().path().by().by("acmPermission").l())


  }

  def reducePath(p: Path): Seq[Edges] = {


    val l = p.objects()

    println("p" + p.toString)
    val list = List.empty
    val seq = l.asScala.map(ll => {


      ll match {

        case _: Vertex => {
          val ee = StartingVertex(ll.asInstanceOf[Vertex])
          ee
        }

        case _: Edge => {
          val e = ll.asInstanceOf[Edge]
          val it = e.vertices(Direction.IN).asScala

          val list = it.map(i => EndingVertex(i)).toList
          val ee = EdgeOut(Some(e.property(acmProp).value()), Some(list))
          ee
        }
      }

    })
    seq
  }


  "Pull Permissions" should "Have be 1 " in {


    // These tests were written to get a test the output of  the Graph API
    val edges = graph.V.hasLabel("room").
      has(Name, "Throneroom of Elizur")
      .has(Type, "room").
      outE(). // traverse to edges
      hasLabel("hasACM")

    println("path" + graph.V.hasLabel("room").
      has(Name, "Throneroom of Elizur").
      out())
    println("pathTre" + graph.V.hasLabel("room").
      has(Name, "Throneroom of Elizur").
      out().tree())

    print("fullPath from a vertex" + graph.V.hasLabel("room").path().unfold().toList())

    val l = graph.V.has(Type, "room").path().toList()

    l.foreach(ll => {
      var i = 0;
      println("pathValue" + ll.objects())

      ll.objects().forEach(v => {

        val vv = graph.V(v).outE("hasACM").inV().headOption()
        println(graph.V(v).outE("hasACM").inV())

        vv match {

          case None => println("NO")
          case (v) => println("Value" + v.get.property("permission").value())
        }
      })
    })


    print("fullPath from a vertexTreees" + graph.V.
      outE().inV().path().fold().l())


    println("tree" + graph.V.hasLabel("room").
      has(Name, "Throneroom of Elizur").
      outE(). // traverse to edges
      hasLabel("hasACM").tree().toSet())

    graph.V.hasLabel("room").
      has(Name, "Throneroom of Elizur").
      outE(). // traverse to edges
      hasLabel("hasACM").tree()



  }


  override protected def beforeAll() = {
    graph = connect()

    val acmNodeTrue = createACMNodeWithTrue()
    val acmNodeFalse = createACMNodeWithFalse()


    // create room nodes
    val throneRoom = graph + ("room", Name -> "Throneroom of Elizur", Type -> "room", acmProp -> "1")
    val cell = graph + ("room", Name -> "Ulani's Cell", Type -> "room", acmProp -> "1")
    val crypt = graph + ("room", Name -> "Crypt of Valinda", Type -> "room", acmProp -> "0")
    val library = graph + ("room", Name -> "Halian's Library", Type -> "room", acmProp -> "1")
    val weedRoom = graph + ("room", Name -> "WeedRoom", Type -> "room", acmProp -> "1")
    val biWeedRoom = graph + ( "room",Name -> "BiDirectionalWeedRoom", Type -> "bbRoom", acmProp -> "1")
    val bbRoom = graph + ("room", Name -> "BDRoom", Type -> "bbRoom", acmProp -> "1")
    val dRoom = graph + ("room", Name -> "Droom", Type -> "bbRoom", acmProp -> "1")


    // create bidirectional room edges

    throneRoom <-- ("hasACM", acmProp -> "1") --> acmNodeTrue
    // Throne Room Connects to Library and Library connects to WeedRoom.
    throneRoom --- ("connectsTo", acmProp -> "8") --> library
    throneRoom <-- ("connectsTo", acmProp -> "1") --> crypt
    crypt <-- ("connectsTo", acmProp -> "1") --> cell
    library --- ("connectsTo", acmProp -> "1") --> weedRoom
    throneRoom <-- ("bConnectsTo", acmProp -> "8") --> biWeedRoom

    biWeedRoom --- ("bConnectsTo", acmProp -> "1") --> bbRoom

    throneRoom --- ("bConnectsTo", acmProp -> "1") --> dRoom


    // create magic item nodes
    val poisonPotion = graph + ("magicItem", Name -> "Poison Potion", IsCursed -> "yes", acmProp -> "1")
    val talismanGood = graph + ("magicItem", Name -> "Talisman of Pure Good", IsCursed -> "no", acmProp -> "1")
    val thunderHammer = graph + ("magicItem", Name -> "Hammer of Thunderbolts", IsCursed -> "no", acmProp -> "1")
    val frostBrand = graph + ("magicItem", Name -> "Frost Brand", IsCursed -> "no", acmProp -> "1")
    val fishCommand = graph + ("magicItem", Name -> "Trident of Fish Command", IsCursed -> "no", acmProp -> "1")
    val flaskCurse = graph + ("magicItem", Name -> "Flask of Curses", IsCursed -> "yes", acmProp -> "1")
    val scarabDeath = graph + ("magicItem", Name -> "Scarab Of Death", IsCursed -> "yes", acmProp -> "1")
    val deckMany = graph + ("magicItem", Name -> "Deck of Many Things", IsCursed -> "no", acmProp -> "1")
    val orbDragon = graph + ("magicItem", Name -> "The Orb of Dragon Kind", IsCursed -> "no", acmProp -> "1")
    val sunBlade = graph + ("magicItem", Name -> "Sun Blade", IsCursed -> "no", acmProp -> "1")
    val rapier = graph + ("magicItem", Name -> "Rapier of Puncturing", IsCursed -> "yes", acmProp -> "1")
    val scimitar = graph + ("magicItem", Name -> "Sylvan Scimitar", IsCursed -> "no", acmProp -> "1")


    val acmScimitar = graph + ("acmmagicItem", Name -> "ACM-Sylvan Scimitar", IsCursed -> "no", acmProp -> "1")

    poisonPotion <-- ("hasACM", acmProp -> "1") --> acmNodeTrue
    talismanGood <-- ("hasACM", acmProp -> "1") --> acmNodeFalse
    fishCommand <-- ("hasACM", acmProp -> "1") --> acmNodeTrue
    flaskCurse <-- ("hasACM", acmProp -> "1") --> acmNodeTrue

    // create directed edges from rooms to magic items


    throneRoom --- ("contains", MinimumLevel -> 1, Wizard -> "wizard", acmProp -> "1") --> poisonPotion
    throneRoom --- ("contains", MinimumLevel -> 3, Wizard -> "wizard", Paladin -> "paladin", Barbarian -> "barbarian", Monk -> "monk", acmProp -> "1") --> talismanGood
    throneRoom --- ("contains", MinimumLevel -> 5, Paladin -> "paladin", Barbarian -> "barbarian", acmProp -> "1") --> thunderHammer
    throneRoom --- ("contains", MinimumLevel -> 7, Paladin -> "paladin", acmProp -> "0") --> frostBrand

    library --- ("contains", MinimumLevel -> 1, Paladin -> "paladin", Monk -> "monk", acmProp -> "8") --> fishCommand
    library --- ("contains", MinimumLevel -> 1, Paladin -> "paladin", Thief -> "thief", Monk -> "monk", acmProp -> "0") --> flaskCurse
    library --- ("contains", MinimumLevel -> 4, Wizard -> "wizard", acmProp -> "0") --> scarabDeath

    crypt --- ("contains", MinimumLevel -> 3, Wizard -> "wizard", Thief -> "thief", Monk -> "monk", acmProp -> "1") --> deckMany
    crypt --- ("contains", MinimumLevel -> 5, Wizard -> "wizard", Monk -> "monk", acmProp -> "1") --> orbDragon

    cell --- ("contains", MinimumLevel -> 1, Paladin -> "paladin", Thief -> "thief", Barbarian -> "barbarian", acmProp -> "0") --> sunBlade
    cell --- ("contains", MinimumLevel -> 5, Thief -> "thief", acmProp -> "1") --> rapier
    cell --- ("contains", MinimumLevel -> 5, Thief -> "thief", acmProp -> "1") --> scimitar
  }

  def createACMNodeWithTrue(): Vertex = {

    val acm = graph.addVertex("acm")
    val prop = Key[String]("permission")
    acm.setProperty(prop, "1")


    acm


  }

  def createACMNodeWithFalse(): Vertex = {

    val acm = graph.addVertex("acm")
    val prop = Key[String]("permission")
    acm.setProperty(prop, "0")


    acm


  }

  override protected def afterAll() = {
    graph.close()
  }
}

