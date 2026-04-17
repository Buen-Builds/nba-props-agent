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
    mp("LaMelo Ball", "CHA", "PRA", 36.5, 42.0, 0.64, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Paolo Banchero", "ORL", "PRA", 36.0, 41.0, 0.67, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Brandon Miller", "CHA", "PRA", 29.5, 32.0, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jalen Suggs", "???", "PRA", 24.0, 26.0, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Kon Knueppel", "CHA", "PRA", 24.5, 27.7, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Desmond Bane", "ORL", "PRA", 27.0, 30.5, 0.88, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Franz Wagner", "ORL", "PRA", 24.5, 27.7, 0.71, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Miles Bridges", "CHA", "PRA", 24.5, 24.0, 0.62, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Moussa Diabaté", "???", "PRA", 19.5, 18.0, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Coby White", "CHI", "PRA", 19.5, 22.0, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Wendell Carter", "???", "PRA", 19.5, 20.0, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Anthony Black", "ORL", "PRA", 16.0, 18.1, 0.5, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Grant Williams", "???", "PRA", 9.5, 10.7, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Ryan Kalkbrenner", "???", "PRA", 7.5, 8.5, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Tristan Silva", "???", "PRA", 6.5, 7.3, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Goga Bitadze", "???", "PRA", 9.5, 10.7, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jamal Cain", "???", "PRA", 5.5, 6.2, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Stephen Curry", "GSW", "PRA", 36.5, 41.2, 0.53, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Devin Booker", "PHX", "PRA", 37.5, 37.8, 0.76, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jalen Green", "HOU", "PRA", 27.5, 31.1, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Kristaps Porziņģis", "???", "PRA", 25.5, 28.8, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Brandin Podziemski", "GSW", "PRA", 23.5, 26.6, 0.8, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Draymond Green", "???", "PRA", 20.5, 23.2, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Dillon Brooks", "PHX", "PRA", 22.5, 24.2, 0.73, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Gui Santos", "???", "PRA", 20.5, 23.2, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Mark Williams", "???", "PRA", 17.0, 19.2, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jordan Goodwin", "???", "PRA", 15.5, 17.5, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("De'Anthony Melton", "GSW", "PRA", 15.5, 17.5, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Al Horford", "???", "PRA", 15.5, 17.5, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Royce O'Neale", "???", "PRA", 13.0, 14.7, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Donovan Mitchell", "CLE", "PRA", 37.5, 42.4, 0.75, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("James Harden", "LAC", "PRA", 34.5, 39.0, 0.81, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Brandon Ingram", "TOR", "PRA", 30.5, 34.5, 0.64, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Evan Mobley", "CLE", "PRA", 29.0, 32.8, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("RJ Barrett", "TOR", "PRA", 28.5, 32.2, 0.83, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Scottie Barnes", "TOR", "PRA", 32.0, 36.2, 0.89, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jarrett Allen", "CLE", "PRA", 25.5, 28.8, 0.75, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jakob Poeltl", "???", "PRA", 20.0, 22.6, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Immanuel Quickley", "TOR", "PRA", 18.5, 20.9, 0.75, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Sandro Mamukelashvili", "???", "PRA", 13.5, 15.3, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Sam Merrill", "???", "PRA", 12.5, 14.1, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Ja'Kobe Walter", "???", "PRA", 11.5, 13.0, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Max Strus", "???", "PRA", 13.5, 15.3, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Dean Wade", "???", "PRA", 9.5, 10.7, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Nikola Jokić", "???", "PRA", 51.5, 58.0, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Anthony Edwards", "MIN", "PRA", 36.5, 41.2, 0.54, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jamal Murray", "DEN", "PRA", 35.0, 39.5, 0.62, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Julius Randle", "MIN", "PRA", 31.5, 35.6, 0.67, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Aaron Gordon", "???", "PRA", 23.5, 26.6, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jaden McDaniels", "???", "PRA", 21.5, 24.3, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Rudy Gobert", "???", "PRA", 22.5, 25.4, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Naz Reid", "???", "PRA", 19.5, 22.0, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Cameron Johnson", "???", "PRA", 24.5, 27.7, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Christian Braun", "???", "PRA", 18.5, 20.9, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Donte DiVincenzo", "???", "PRA", 18.5, 20.9, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Tim Hardaway", "???", "PRA", 14.5, 16.4, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Ayo Dosunmu", "CHI", "PRA", 16.5, 18.6, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jalen Johnson", "ATL", "PRA", 39.5, 44.6, 0.71, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jalen Brunson", "NYK", "PRA", 38.5, 37.2, 0.8, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Karl-Anthony Towns", "NYK", "PRA", 35.5, 35.8, 0.75, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Nickeil Alexander-Walker", "ATL", "PRA", 27.5, 31.1, 0.64, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("CJ McCollum", "WAS", "PRA", 25.0, 28.2, 0.7, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Dyson Daniels", "???", "PRA", 22.5, 25.4, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Josh Hart", "???", "PRA", 22.5, 25.4, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Onyeka Okongwu", "???", "PRA", 23.5, 26.6, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("OG Anunoby", "NYK", "PRA", 22.5, 25.4, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Mikal Bridges", "NYK", "PRA", 19.5, 22.0, 0.5, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jonathan Kuminga", "???", "PRA", 15.5, 17.5, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Miles McBride", "???", "PRA", 10.5, 11.9, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Mitchell Robinson", "???", "PRA", 14.0, 15.8, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Landry Shamet", "???", "PRA", 7.5, 8.5, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Gabe Vincent", "???", "PRA", 3.5, 4.0, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("LeBron James", "LAL", "PRA", 41.5, 46.9, 0.75, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Alperen Sengun", "HOU", "PRA", 35.0, 39.5, 0.77, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Kevin Durant", "HOU", "PRA", 35.5, 39.8, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Amen Thompson", "HOU", "PRA", 31.5, 35.6, 0.67, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Deandre Ayton", "???", "PRA", 22.5, 25.4, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jabari Smith", "???", "PRA", 22.5, 25.4, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Reed Sheppard", "HOU", "PRA", 18.5, 20.9, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Luke Kennard", "???", "PRA", 19.0, 21.5, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Marcus Smart", "???", "PRA", 18.0, 20.3, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Rui Hachimura", "???", "PRA", 18.5, 20.9, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Tari Eason", "???", "PRA", 19.5, 22.0, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jaxson Hayes", "???", "PRA", 13.5, 15.3, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jake LaRavia", "???", "PRA", 13.5, 15.3, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jarred Vanderbilt", "???", "PRA", 9.5, 10.7, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Josh Okogie", "???", "PRA", 9.0, 10.2, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jayson Tatum", "???", "PRA", 38.5, 47.8, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Tyrese Maxey", "PHI", "PRA", 36.5, 41.2, 0.72, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jaylen Brown", "BOS", "PRA", 36.5, 41.2, 0.68, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Paul George", "PHI", "PRA", 28.0, 31.6, 0.5, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("VJ Edgecombe", "???", "PRA", 25.5, 28.8, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Derrick White", "BOS", "PRA", 22.5, 25.4, 0.86, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Kelly Oubre", "???", "PRA", 20.0, 22.6, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Payton Pritchard", "BOS", "PRA", 21.5, 24.3, 0.67, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Andre Drummond", "???", "PRA", 18.5, 20.9, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Neemias Queta", "???", "PRA", 19.5, 22.0, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Quentin Grimes", "PHI", "PRA", 14.5, 16.4, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Nikola Vučević", "???", "PRA", 17.0, 19.2, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Sam Hauser", "???", "PRA", 12.0, 13.6, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Victor Wembanyama", "SAS", "PRA", 43.5, 49.2, 0.54, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Deni Avdija", "POR", "PRA", 38.0, 46.2, 0.65, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Stephon Castle", "???", "PRA", 30.5, 34.5, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("De'Aaron Fox", "SAS", "PRA", 27.5, 31.1, 0.9, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Donovan Clingan", "???", "PRA", 24.5, 22.0, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jrue Holiday", "POR", "PRA", 25.5, 22.0, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Toumani Camara", "POR", "PRA", 19.0, 21.5, 0.5, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Devin Vassell", "SAS", "PRA", 17.5, 19.8, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Keldon Johnson", "???", "PRA", 17.5, 19.8, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Scoot Henderson", "???", "PRA", 16.0, 22.0, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Shaedon Sharpe", "POR", "PRA", 14.5, 16.4, 0.75, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Julian Champagnie", "SAS", "PRA", 16.5, 18.6, 0.67, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jerami Grant", "POR", "PRA", 13.5, 15.3, 0.89, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Dylan Harper", "SAS", "PRA", 14.5, 16.4, 0.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Robert Williams", "???", "PRA", 11.5, 13.0, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Harrison Barnes", "???", "PRA", 6.5, 7.3, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Luke Kornet", "???", "PRA", 8.5, 9.6, 0.85, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("LaMelo Ball", "CHA", "Rebs+Asts", 13.0, 15.2, 0.64, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Moussa Diabaté", "???", "Rebs+Asts", 12.5, 14.0, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Paolo Banchero", "ORL", "Rebs+Asts", 13.0, 14.8, 0.67, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jalen Suggs", "???", "Rebs+Asts", 9.5, 11.2, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Wendell Carter", "???", "Rebs+Asts", 9.5, 12.0, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Brandon Miller", "CHA", "Rebs+Asts", 8.5, 8.2, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Desmond Bane", "ORL", "Rebs+Asts", 7.5, 8.5, 0.88, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Miles Bridges", "CHA", "Rebs+Asts", 9.0, 8.2, 0.62, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Franz Wagner", "ORL", "Rebs+Asts", 7.0, 7.9, 0.71, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Kon Knueppel", "CHA", "Rebs+Asts", 8.0, 9.0, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Goga Bitadze", "???", "Rebs+Asts", 5.5, 6.2, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Coby White", "CHI", "Rebs+Asts", 5.0, 5.6, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Anthony Black", "ORL", "Rebs+Asts", 4.5, 5.1, 0.5, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Grant Williams", "???", "Rebs+Asts", 3.5, 4.0, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Ryan Kalkbrenner", "???", "Rebs+Asts", 3.5, 4.0, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Sion James", "???", "Rebs+Asts", 2.5, 2.8, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jamal Cain", "???", "Rebs+Asts", 1.5, 1.7, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Tristan Silva", "???", "Rebs+Asts", 2.5, 2.8, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Draymond Green", "???", "Rebs+Asts", 12.0, 13.6, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Devin Booker", "PHX", "Rebs+Asts", 9.5, 11.0, 0.76, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Brandin Podziemski", "GSW", "Rebs+Asts", 9.0, 10.2, 0.8, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Stephen Curry", "GSW", "Rebs+Asts", 7.5, 10.2, 0.53, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Gui Santos", "???", "Rebs+Asts", 7.5, 8.5, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Kristaps Porziņģis", "???", "Rebs+Asts", 7.5, 8.5, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jalen Green", "HOU", "Rebs+Asts", 8.0, 9.0, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Mark Williams", "???", "Rebs+Asts", 8.5, 9.6, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Al Horford", "???", "Rebs+Asts", 7.5, 8.5, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Jordan Goodwin", "???", "Rebs+Asts", 7.0, 7.9, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Dillon Brooks", "PHX", "Rebs+Asts", 5.5, 7.2, 0.73, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("De'Anthony Melton", "GSW", "Rebs+Asts", 4.5, 5.1, 1.0, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Royce O'Neale", "???", "Rebs+Asts", 4.5, 5.1, 0.87, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("Scottie Barnes", "TOR", "Rebs+Asts", 14.0, 15.8, 0.89, 0.20, -500, "CHA @ ORL", "11:30PM"),
    mp("James Harden", "LAC", "Rebs+Asts", 12.5, 14.1, 0.81, 0.20, -500, "CHA @ ORL", "11:30PM"),
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
