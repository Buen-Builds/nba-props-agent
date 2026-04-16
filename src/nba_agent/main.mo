import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Int "mo:base/Int";
import Float "mo:base/Float";
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import Iter "mo:base/Iter";
import Error "mo:base/Error";
import Timer "mo:base/Timer";
import Blob "mo:base/Blob";

persistent actor NBAAgent {

  var registered   : Bool = false;
  var last_slip    : Text = "";
  var total_slips  : Nat  = 0;
  var injury_cache : Text = "";
  var fetch_count  : Nat  = 0;
  var live_props   : Text = "";
  var injured_list : Text = "";

  let RAPIDAPI_KEY = "44159ba4cemsh8d61c5958b50bdcp160e6ejsn842ac4627557";
  let TANK_HOST    = "tank01-fantasy-stats.p.rapidapi.com";
  let WORKER_URL   = "https://prizepicks-proxy.crisbuen.workers.dev/props";

  type HttpHeader   = { name: Text; value: Text };
  type HttpResponse = { status: Nat; headers: [HttpHeader]; body: Blob };
  type HttpRequest  = {
    url: Text; method: { #get }; headers: [HttpHeader];
    body: ?Blob; max_response_bytes: ?Nat64;
    transform: ?{ function: shared query ({ response: HttpResponse; context: Blob }) -> async HttpResponse; context: Blob };
  };
  type IC = actor { http_request: HttpRequest -> async HttpResponse };
  let ic : IC = actor("aaaaa-aa");

  type RegActor = actor { register: (Text, Text, [Text], Nat) -> async Text };
  type MemActor = actor { set: (Text, Text) -> async (); get: (Text) -> async ?Text };
  func mem() : MemActor { actor("hupoq-3aaaa-aaaas-qf4tq-cai") };

  public query func transform(raw: { response: HttpResponse; context: Blob }) : async HttpResponse {
    { status = raw.response.status; body = raw.response.body;
      headers = [{ name = "Content-Type"; value = "application/json" }] }
  };

  public func register_on_agentforge() : async Text {
    let r : RegActor = actor("h2ndy-aqaaa-aaaas-qf4sq-cai");
    try {
      let res = await r.register("NBA Props Agent", "Live autonomous NBA props. Real PrizePicks lines.", ["nba","prizepicks","PRA","RA"], 50_000_000);
      registered := true; "Registered: " # res
    } catch(e) { "Error: " # Error.message(e) }
  };

  type Prop = {
    player: Text; team: Text; stat: Text;
    line: Float; avg: Float; edge: Float;
    hit_rate: Float; blowout_risk: Float;
    confidence: Float; game: Text;
    game_time: Text; odds: Int;
    stat_type: Text; edge_pct: Float;
  };

  func stat_bonus(stat: Text) : Float {
    if      (Text.contains(stat, #text "PRA"))        { 1.30 }
    else if (Text.contains(stat, #text "Rebs+Asts"))  { 1.28 }
    else if (Text.contains(stat, #text "Pts+Rebs"))   { 1.20 }
    else if (Text.contains(stat, #text "Rebounds"))   { 1.18 }
    else if (Text.contains(stat, #text "Blocks"))     { 1.15 }
    else if (Text.contains(stat, #text "Dunks"))      { 1.12 }
    else if (Text.contains(stat, #text "Points"))     { 1.05 }
    else if (Text.contains(stat, #text "3PTM"))       { 0.90 }
    else if (Text.contains(stat, #text "FTM"))        { 0.80 }
    else if (Text.contains(stat, #text "Steals"))     { 0.75 }
    else                                              { 1.0  }
  };

  func calc(h: Float, e: Float, ep: Float, b: Float, o: Int, stat: Text) : Float {
    let en  = Float.min(e / 5.0, 1.0);
    let epn = Float.min(ep / 0.5, 1.0);
    let os  = if (o <= -500) { 1.0 } else if (o <= -300) { 0.7 } else { 0.4 };
    Float.min((h*0.30 + en*0.20 + epn*0.20 + 1.0*0.15 + (1.0-b)*0.10 + os*0.05) * stat_bonus(stat), 1.0)
  };

  func classify(stat: Text) : Text {
    if      (Text.contains(stat, #text "PRA"))        { "PRA" }
    else if (Text.contains(stat, #text "Rebs+Asts"))  { "RA"  }
    else if (Text.contains(stat, #text "Pts+Rebs"))   { "PR"  }
    else if (Text.contains(stat, #text "Rebounds"))   { "REB" }
    else if (Text.contains(stat, #text "Points"))     { "PTS" }
    else if (Text.contains(stat, #text "Blocks"))     { "BLK" }
    else                                              { "OTHER" }
  };

  func mp(pl:Text, tm:Text, st:Text, ln:Float, av:Float, hr:Float, bl:Float, od:Int, gm:Text, gt:Text) : Prop {
    let eg = av - ln;
    let ep = if (ln > 0.0) { eg / ln } else { 0.0 };
    { player=pl; team=tm; stat=st; line=ln; avg=av;
      edge=eg; edge_pct=ep; hit_rate=hr; blowout_risk=bl;
      confidence=calc(hr,eg,ep,bl,od,st);
      game=gm; game_time=gt; odds=od; stat_type=classify(st) }
  };

  func is_injured(player: Text) : Bool {
    Text.contains(injured_list, #text player)
  };

  func assists_only(stat: Text) : Bool {
    let s = Text.toLowercase(stat);
    Text.contains(s, #text "assist") and
    not Text.contains(s, #text "reb") and
    not Text.contains(s, #text "pts") and
    not Text.contains(s, #text "pra")
  };

  func load_props() : [Prop] {[
    mp("Moussa Diabaté", "???", "Rebounds", 9.5, 11.2, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Paolo Banchero", "???", "Rebounds", 8.0, 9.2, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Wendell Carter", "???", "Rebounds", 7.5, 9.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Brandon Miller", "???", "Rebounds", 5.5, 6.8, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Miles Bridges", "???", "Rebounds", 6.0, 7.2, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Kon Knueppel", "???", "Rebounds", 5.0, 5.6, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("LaMelo Ball", "???", "Rebounds", 5.0, 5.6, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Goga Bitadze", "???", "Rebounds", 4.5, 5.1, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Desmond Bane", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Franz Wagner", "???", "Rebounds", 4.0, 4.5, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Jalen Suggs", "???", "Rebounds", 4.0, 4.5, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Ryan Kalkbrenner", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Anthony Black", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Grant Williams", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Coby White", "???", "Rebounds", 1.5, 1.7, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Sion James", "???", "Rebounds", 1.5, 1.7, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Jamal Cain", "???", "Rebounds", 1.5, 1.7, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Tristan Silva", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Mark Williams", "???", "Rebounds", 7.5, 8.5, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Draymond Green", "???", "Rebounds", 5.5, 6.2, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Kristaps Porziņģis", "???", "Rebounds", 6.0, 6.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Brandin Podziemski", "???", "Rebounds", 5.5, 6.2, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Gui Santos", "???", "Rebounds", 5.0, 5.6, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Jordan Goodwin", "???", "Rebounds", 5.0, 5.6, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Al Horford", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Devin Booker", "???", "Rebounds", 4.0, 4.5, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Dillon Brooks", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Jalen Green", "???", "Rebounds", 4.5, 5.1, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Stephen Curry", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Royce O'Neale", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("De'Anthony Melton", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Evan Mobley", "???", "Rebounds", 9.0, 10.2, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Jarrett Allen", "???", "Rebounds", 9.0, 10.2, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Jakob Poeltl", "???", "Rebounds", 7.5, 8.5, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Scottie Barnes", "???", "Rebounds", 7.0, 7.9, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Brandon Ingram", "???", "Rebounds", 4.5, 5.1, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("James Harden", "???", "Rebounds", 4.5, 5.1, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("RJ Barrett", "???", "Rebounds", 5.0, 5.6, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Donovan Mitchell", "???", "Rebounds", 4.0, 4.5, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Dean Wade", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Max Strus", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Sandro Mamukelashvili", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Immanuel Quickley", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Sam Merrill", "???", "Rebounds", 1.5, 1.7, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Ja'Kobe Walter", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Nikola Jokić", "???", "Rebounds", 13.0, 14.7, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Rudy Gobert", "???", "Rebounds", 10.5, 11.9, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Julius Randle", "???", "Rebounds", 6.0, 6.8, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Naz Reid", "???", "Rebounds", 6.0, 6.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Aaron Gordon", "???", "Rebounds", 5.5, 6.2, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Anthony Edwards", "???", "Rebounds", 5.0, 5.6, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Christian Braun", "???", "Rebounds", 4.5, 5.1, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Jamal Murray", "???", "Rebounds", 4.5, 5.1, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Donte DiVincenzo", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Jaden McDaniels", "???", "Rebounds", 4.0, 4.5, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Cameron Johnson", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Ayo Dosunmu", "???", "Rebounds", 1.5, 1.7, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Tim Hardaway", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Karl-Anthony Towns", "???", "Rebounds", 12.0, 13.6, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Jalen Johnson", "???", "Rebounds", 10.0, 11.3, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Mitchell Robinson", "???", "Rebounds", 8.5, 9.6, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Onyeka Okongwu", "???", "Rebounds", 6.5, 7.3, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Dyson Daniels", "???", "Rebounds", 7.0, 7.9, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Josh Hart", "???", "Rebounds", 7.0, 7.9, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("OG Anunoby", "???", "Rebounds", 5.0, 5.6, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Jonathan Kuminga", "???", "Rebounds", 4.5, 5.1, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Nickeil Alexander-Walker", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Jalen Brunson", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Mikal Bridges", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("CJ McCollum", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Miles McBride", "???", "Rebounds", 1.5, 1.7, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Landry Shamet", "???", "Rebounds", 1.5, 1.7, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Alperen Sengun", "???", "Rebounds", 9.0, 10.2, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Amen Thompson", "???", "Rebounds", 7.5, 8.5, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Deandre Ayton", "???", "Rebounds", 8.0, 9.0, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("LeBron James", "???", "Rebounds", 6.5, 7.3, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Jabari Smith", "???", "Rebounds", 6.5, 7.3, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Kevin Durant", "???", "Rebounds", 5.5, 6.2, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Tari Eason", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Rui Hachimura", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Josh Okogie", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Jake LaRavia", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Jaxson Hayes", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Jarred Vanderbilt", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Reed Sheppard", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Luke Kennard", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Marcus Smart", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Andre Drummond", "???", "Rebounds", 9.5, 10.7, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Jayson Tatum", "???", "Rebounds", 8.5, 9.6, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Neemias Queta", "???", "Rebounds", 7.5, 8.5, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Jaylen Brown", "???", "Rebounds", 6.0, 6.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Paul George", "???", "Rebounds", 4.5, 5.1, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("VJ Edgecombe", "???", "Rebounds", 5.5, 6.2, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Nikola Vučević", "???", "Rebounds", 4.5, 5.1, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Adem Bona", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Kelly Oubre", "???", "Rebounds", 4.5, 5.1, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Derrick White", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Payton Pritchard", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Sam Hauser", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Tyrese Maxey", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Quentin Grimes", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Donovan Clingan", "???", "Rebounds", 11.0, 12.0, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Victor Wembanyama", "???", "Rebounds", 11.5, 13.0, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Deni Avdija", "???", "Rebounds", 6.5, 7.3, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Stephon Castle", "???", "Rebounds", 5.5, 6.2, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Robert Williams", "???", "Rebounds", 5.5, 6.2, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Julian Champagnie", "???", "Rebounds", 5.0, 5.6, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Toumani Camara", "???", "Rebounds", 4.5, 5.1, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Jrue Holiday", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("De'Aaron Fox", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Devin Vassell", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Keldon Johnson", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Luke Kornet", "???", "Rebounds", 3.5, 4.0, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Dylan Harper", "???", "Rebounds", 2.5, 2.8, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Scoot Henderson", "???", "Rebounds", 1.5, 1.7, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Shaedon Sharpe", "???", "Rebounds", 1.5, 1.7, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("Harrison Barnes", "???", "Rebounds", 1.5, 1.7, 0.83, 0.20, -500, "Tonight", "TBD"),
    mp("Jerami Grant", "???", "Rebounds", 1.5, 1.7, 0.83, 0.20, -500, "Tonight", "TBD")
    mp("LaMelo Ball", "???", "PRA", 36.5, 42.0, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Paolo Banchero", "???", "PRA", 36.0, 41.0, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Brandon Miller", "???", "PRA", 28.5, 32.0, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Jalen Suggs", "???", "PRA", 23.5, 26.0, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Kon Knueppel", "???", "PRA", 24.5, 27.7, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Desmond Bane", "???", "PRA", 27.5, 31.1, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Franz Wagner", "???", "PRA", 24.5, 27.7, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Miles Bridges", "???", "PRA", 24.5, 24.0, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Moussa Diabaté", "???", "PRA", 19.5, 18.0, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Coby White", "???", "PRA", 18.5, 20.9, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Wendell Carter", "???", "PRA", 19.5, 20.0, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Anthony Black", "???", "PRA", 16.0, 18.1, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Tristan Silva", "???", "PRA", 6.5, 7.3, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Goga Bitadze", "???", "PRA", 9.5, 10.7, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Ryan Kalkbrenner", "???", "PRA", 7.5, 8.5, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Sion James", "???", "PRA", 6.5, 7.3, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Jamal Cain", "???", "PRA", 5.5, 6.2, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Stephen Curry", "???", "PRA", 36.5, 41.2, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Devin Booker", "???", "PRA", 37.5, 37.8, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Jalen Green", "???", "PRA", 26.5, 29.9, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Kristaps Porziņģis", "???", "PRA", 25.5, 28.8, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Brandin Podziemski", "???", "PRA", 23.5, 26.6, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Draymond Green", "???", "PRA", 20.5, 23.2, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Dillon Brooks", "???", "PRA", 22.5, 24.2, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Gui Santos", "???", "PRA", 20.5, 23.2, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Mark Williams", "???", "PRA", 17.0, 19.2, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Jordan Goodwin", "???", "PRA", 15.5, 17.5, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("De'Anthony Melton", "???", "PRA", 15.5, 17.5, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Al Horford", "???", "PRA", 14.5, 16.4, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("Royce O'Neale", "???", "PRA", 13.0, 14.7, 0.85, 0.20, -500, "Tonight", "TBD")
    mp("Donovan Mitchell", "???", "PRA", 37.5, 42.4, 0.85, 0.20, -500, "Tonight", "TBD"),
    mp("James Harden", "???", "PRA", 33.5, 37.9, 0.85, 0.20, -500, "Tonight", "TBD")
  ]};

  public func fetch_injuries() : async Text {
    try {
      let res = await (with cycles = 230_949_972_000) ic.http_request({
        url = "https://tank01-fantasy-stats.p.rapidapi.com/getNBAInjuryList";
        method = #get;
        max_response_bytes = ?Nat64.fromNat(100_000);
        headers = [{ name = "x-rapidapi-key"; value = RAPIDAPI_KEY }, { name = "x-rapidapi-host"; value = TANK_HOST }];
        body = null;
        transform = ?{ function = transform; context = Blob.fromArray([]) };
      });
      let txt = switch (Text.decodeUtf8(res.body)) { case (?t) { t }; case null { "" } };
      injury_cache := txt; fetch_count := fetch_count + 1; injured_list := txt;
      "OK: " # Nat.toText(txt.size()) # " bytes"
    } catch(e) { "error: " # Error.message(e) }
  };

  public func fetch_live_props() : async Text {
    try {
      let res = await (with cycles = 230_949_972_000) ic.http_request({
        url = WORKER_URL; method = #get;
        max_response_bytes = ?Nat64.fromNat(500_000);
        headers = [{ name = "Accept"; value = "application/json" }];
        body = null;
        transform = ?{ function = transform; context = Blob.fromArray([]) };
      });
      let txt = switch (Text.decodeUtf8(res.body)) { case (?t) { t }; case null { "" } };
      live_props := txt; fetch_count := fetch_count + 1;
      "OK: " # Nat.toText(txt.size()) # " bytes"
    } catch(e) { "error: " # Error.message(e) }
  };

  func rank_props(ps: [Prop]) : [Prop] {
    let b = Buffer.fromArray<Prop>(ps);
    let n = b.size();
    var i = 0;
    while (i < n) {
      var j = 0;
      while (j < n - i - 1) {
        if (b.get(j).confidence < b.get(j+1).confidence) {
          let tmp = b.get(j); b.put(j, b.get(j+1)); b.put(j+1, tmp);
        };
        j := j + 1;
      };
      i := i + 1;
    };
    Buffer.toArray(b)
  };

  func one_per_player(ps: [Prop]) : [Prop] {
    let seen = Buffer.Buffer<Text>(20);
    let out  = Buffer.Buffer<Prop>(20);
    for (p in ps.vals()) {
      if (not Buffer.contains<Text>(seen, p.player, Text.equal)) {
        seen.add(p.player); out.add(p);
      };
    };
    Buffer.toArray(out)
  };

  func to_json(ps: [Prop]) : Text {
    let parts = Array.map<Prop, Text>(ps, func(p) {
      "{\"player\":\"" # p.player # "\"," #
      "\"team\":\"" # p.team # "\"," #
      "\"stat\":\"" # p.stat # "\"," #
      "\"stat_type\":\"" # p.stat_type # "\"," #
      "\"line\":" # Float.toText(p.line) # "," #
      "\"avg\":" # Float.toText(p.avg) # "," #
      "\"edge\":" # Float.toText(p.edge) # "," #
      "\"edge_pct\":" # Float.toText(p.edge_pct) # "," #
      "\"hit_rate\":" # Float.toText(p.hit_rate) # "," #
      "\"confidence\":" # Float.toText(p.confidence) # "," #
      "\"game\":\"" # p.game # "\"," #
      "\"game_time\":\"" # p.game_time # "\"," #
      "\"odds\":" # Int.toText(p.odds) # "," #
      "\"direction\":\"OVER\"}"
    });
    "[" # Text.join(",", Iter.fromArray(parts)) # "]"
  };

  public query func get_ranked_props() : async Text { to_json(rank_props(load_props())) };

  public func get_best_slip(n: Nat) : async Text {
    let ranked = rank_props(load_props());
    let filtered = Array.filter<Prop>(ranked, func(p) {
      p.edge > 0.0 and p.confidence > 0.72 and
      not assists_only(p.stat) and p.edge_pct > 0.10 and
      not is_injured(p.player)
    });
    let deduped = one_per_player(filtered);
    let count   = Nat.min(n, deduped.size());
    let picks   = Array.tabulate<Prop>(count, func(i) { deduped[i] });
    total_slips := total_slips + 1;
    let j = to_json(picks);
    last_slip := j;
    ignore mem().set("slip:latest", j);
    j
  };

  public func get_best_6_pick() : async Text { await get_best_slip(6) };
  public func get_best_5_pick() : async Text { await get_best_slip(5) };
  public func get_best_4_pick() : async Text { await get_best_slip(4) };

  public func refresh_data() : async Text {
    let i = await fetch_injuries();
    let p = await fetch_live_props();
    let _s = await get_best_slip(6);
    "Injuries: " # i # " | Props: " # p
  };

  public query func get_injury_cache() : async Text { injury_cache };
  public query func get_live_props()   : async Text { live_props };
  public query func get_last_slip()    : async Text { last_slip };

  public query func get_agent_stats() : async Text {
    "{\"total_slips\":" # Nat.toText(total_slips) #
    ",\"fetch_count\":" # Nat.toText(fetch_count) #
    ",\"registered\":" # (if registered { "true" } else { "false" }) # "}"
  };

  let _timer = Timer.recurringTimer<system>(#seconds(60), func() : async () {
    ignore await fetch_injuries();
    ignore await fetch_live_props();
    ignore await get_best_slip(6);
  });
};
