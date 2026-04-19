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
    if      (Text.contains(stat, #text "PRA"))             { 1.30 }
    else if (Text.contains(stat, #text "Rebs+Asts"))       { 1.28 }
    else if (Text.contains(stat, #text "PF_TO"))           { 1.25 }
    else if (Text.contains(stat, #text "Pts+Rebs"))        { 1.20 }
    else if (Text.contains(stat, #text "Rebounds"))        { 1.18 }
    else if (Text.contains(stat, #text "Personal Fouls"))  { 1.18 }
    else if (Text.contains(stat, #text "Blocks"))          { 1.15 }
    else if (Text.contains(stat, #text "Turnovers"))       { 1.15 }
    else if (Text.contains(stat, #text "Dunks"))           { 1.12 }
    else if (Text.contains(stat, #text "Points"))          { 1.05 }
    else if (Text.contains(stat, #text "3PTM"))            { 0.90 }
    else if (Text.contains(stat, #text "FTM"))             { 0.80 }
    else if (Text.contains(stat, #text "Steals"))          { 0.75 }
    else                                                   { 1.0  }
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
    mp("Josh Hart", "???", "PRA", 26.5, 29.9, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Dyson Daniels", "???", "PRA", 21.5, 24.3, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jonathan Kuminga", "???", "PRA", 19.5, 22.0, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Deandre Ayton", "???", "PRA", 21.5, 24.3, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jabari Smith", "HOU", "PRA", 18.5, 23.0, 0.85, 0.15, -500, "HOU @ LAL", "08:30PM"),
    mp("Luke Kennard", "???", "PRA", 21.5, 24.3, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Marcus Smart", "???", "PRA", 18.5, 20.9, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Rui Hachimura", "???", "PRA", 19.5, 22.0, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Tari Eason", "HOU", "PRA", 12.0, 16.5, 0.88, 0.20, -500, "HOU @ LAL", "08:30PM"),
    mp("Jaxson Hayes", "???", "PRA", 12.0, 13.6, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jake LaRavia", "LAL", "PRA", 7.5, 10.5, 0.87, 0.20, -500, "HOU @ LAL", "08:30PM"),
    mp("Jarred Vanderbilt", "???", "PRA", 7.5, 8.5, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Josh Okogie", "???", "PRA", 11.5, 13.0, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Clint Capela", "???", "PRA", 7.5, 8.5, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Aaron Holiday", "???", "PRA", 6.5, 7.3, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Dorian Finney-Smith", "???", "PRA", 5.5, 6.2, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jayson Tatum", "BOS", "PRA", 38.5, 47.8, 0.8, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Tyrese Maxey", "PHI", "PRA", 35.5, 40.1, 0.72, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Jaylen Brown", "BOS", "PRA", 36.0, 40.7, 0.68, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Paul George", "PHI", "PRA", 28.5, 32.2, 0.5, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Derrick White", "BOS", "PRA", 22.5, 25.4, 0.86, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Kelly Oubre", "???", "PRA", 20.5, 23.2, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Payton Pritchard", "BOS", "PRA", 21.5, 24.3, 0.67, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Andre Drummond", "???", "PRA", 18.5, 20.9, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Neemias Queta", "???", "PRA", 18.5, 20.9, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Quentin Grimes", "PHI", "PRA", 14.5, 16.4, 1.0, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Nikola Vučević", "???", "PRA", 15.5, 17.5, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Sam Hauser", "???", "PRA", 12.0, 13.6, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Baylor Scheierman", "???", "PRA", 10.5, 11.9, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Dominick Barlow", "???", "PRA", 6.5, 7.3, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Adem Bona", "???", "PRA", 4.5, 5.1, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Shai Gilgeous-Alexander", "OKC", "PRA", 41.5, 45.0, 0.74, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Devin Booker", "PHX", "PRA", 34.5, 37.8, 0.76, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Chet Holmgren", "OKC", "PRA", 26.5, 29.9, 1.0, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Jalen Williams", "OKC", "PRA", 27.5, 31.1, 1.0, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Dillon Brooks", "PHX", "PRA", 21.5, 24.2, 0.73, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Isaiah Hartenstein", "???", "PRA", 18.5, 20.9, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jordan Goodwin", "PHX", "PRA", 16.5, 18.6, 0.8, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Ajay Mitchell", "???", "PRA", 16.5, 18.6, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Grayson Allen", "PHX", "PRA", 12.5, 14.1, 0.8, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Cason Wallace", "???", "PRA", 10.5, 11.9, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Luguentz Dort", "???", "PRA", 11.5, 13.0, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Royce O'Neale", "PHX", "PRA", 11.5, 13.0, 0.8, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Cade Cunningham", "DET", "PRA", 42.5, 48.0, 0.68, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jalen Duren", "DET", "PRA", 34.5, 39.0, 0.6, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Paolo Banchero", "ORL", "PRA", 35.5, 41.0, 0.67, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Desmond Bane", "ORL", "PRA", 27.5, 31.1, 0.88, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Franz Wagner", "ORL", "PRA", 24.5, 27.7, 0.71, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jalen Suggs", "ORL", "PRA", 23.5, 26.0, 0.75, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Tobias Harris", "???", "PRA", 21.5, 24.3, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Ausar Thompson", "???", "PRA", 17.5, 19.8, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Duncan Robinson", "???", "PRA", 15.5, 17.5, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Wendell Carter", "ORL", "PRA", 18.5, 20.0, 0.75, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Anthony Black", "ORL", "PRA", 15.5, 17.5, 0.5, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Daniss Jenkins", "???", "PRA", 12.5, 14.1, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Isaiah Stewart", "???", "PRA", 11.5, 13.0, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Tristan Silva", "???", "PRA", 7.5, 8.5, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Victor Wembanyama", "SAS", "PRA", 43.5, 49.2, 0.54, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Deni Avdija", "POR", "PRA", 37.5, 46.2, 0.65, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Stephon Castle", "SAS", "PRA", 30.5, 34.5, 0.75, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("De'Aaron Fox", "SAS", "PRA", 27.5, 31.1, 0.9, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Donovan Clingan", "POR", "PRA", 23.5, 22.0, 0.75, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Jrue Holiday", "POR", "PRA", 25.5, 22.0, 1.0, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Toumani Camara", "POR", "PRA", 18.5, 20.9, 0.5, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Devin Vassell", "SAS", "PRA", 17.5, 19.8, 1.0, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Keldon Johnson", "???", "PRA", 16.5, 18.6, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Scoot Henderson", "POR", "PRA", 15.0, 22.0, 0.8, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Shaedon Sharpe", "POR", "PRA", 14.5, 16.4, 0.75, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Julian Champagnie", "SAS", "PRA", 16.5, 18.6, 0.67, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Jerami Grant", "POR", "PRA", 14.5, 16.4, 0.89, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Dylan Harper", "SAS", "PRA", 14.5, 16.4, 0.0, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Robert Williams", "???", "PRA", 11.5, 13.0, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Harrison Barnes", "???", "PRA", 11.5, 13.0, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Luke Kornet", "???", "PRA", 8.5, 9.6, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jakob Poeltl", "???", "PRA", 16.5, 18.6, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Max Strus", "???", "PRA", 14.5, 16.4, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Dean Wade", "???", "PRA", 9.5, 10.7, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jaden McDaniels", "???", "PRA", 21.5, 24.3, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Rudy Gobert", "???", "PRA", 23.5, 26.6, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Naz Reid", "???", "PRA", 18.5, 20.9, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Christian Braun", "???", "PRA", 18.5, 20.9, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Donte DiVincenzo", "???", "PRA", 16.5, 18.6, 0.85, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Dyson Daniels", "???", "Rebs+Asts", 16.5, 18.6, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Josh Hart", "???", "Rebs+Asts", 17.5, 19.8, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jonathan Kuminga", "???", "Rebs+Asts", 7.5, 8.5, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Mitchell Robinson", "???", "Rebs+Asts", 5.5, 6.2, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Miles McBride", "???", "Rebs+Asts", 4.5, 5.1, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Deandre Ayton", "???", "Rebs+Asts", 8.5, 9.6, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Luke Kennard", "???", "Rebs+Asts", 8.0, 9.0, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jabari Smith", "HOU", "Rebs+Asts", 7.0, 9.8, 0.87, 0.15, -500, "HOU @ LAL", "08:30PM"),
    mp("Marcus Smart", "???", "Rebs+Asts", 7.0, 7.9, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Tari Eason", "HOU", "Rebs+Asts", 4.5, 6.8, 0.88, 0.25, -500, "HOU @ LAL", "08:30PM"),
    mp("Jake LaRavia", "LAL", "Rebs+Asts", 5.5, 7.8, 0.87, 0.20, -500, "HOU @ LAL", "08:30PM"),
    mp("Josh Okogie", "???", "Rebs+Asts", 5.5, 6.2, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Rui Hachimura", "???", "Rebs+Asts", 3.5, 4.0, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jaxson Hayes", "???", "Rebs+Asts", 5.0, 5.6, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jarred Vanderbilt", "???", "Rebs+Asts", 3.5, 4.0, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Clint Capela", "???", "Rebs+Asts", 4.5, 5.1, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jayson Tatum", "BOS", "Rebs+Asts", 14.5, 13.2, 0.8, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Andre Drummond", "???", "Rebs+Asts", 11.5, 13.0, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jaylen Brown", "BOS", "Rebs+Asts", 10.5, 11.9, 0.68, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Neemias Queta", "???", "Rebs+Asts", 9.5, 10.7, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Tyrese Maxey", "PHI", "Rebs+Asts", 9.5, 10.7, 0.72, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Derrick White", "BOS", "Rebs+Asts", 8.5, 9.6, 0.86, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Paul George", "PHI", "Rebs+Asts", 9.0, 10.2, 0.5, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Payton Pritchard", "BOS", "Rebs+Asts", 7.5, 8.5, 0.67, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Nikola Vučević", "???", "Rebs+Asts", 6.5, 7.3, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Kelly Oubre", "???", "Rebs+Asts", 5.5, 6.2, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Quentin Grimes", "PHI", "Rebs+Asts", 4.5, 5.1, 1.0, 0.20, -500, "PHI @ BOS", "05:00PM"),
    mp("Sam Hauser", "???", "Rebs+Asts", 4.5, 5.1, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Baylor Scheierman", "???", "Rebs+Asts", 4.5, 5.1, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Dominick Barlow", "???", "Rebs+Asts", 2.5, 2.8, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Chet Holmgren", "OKC", "Rebs+Asts", 10.5, 11.9, 1.0, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Devin Booker", "PHX", "Rebs+Asts", 10.5, 11.0, 0.76, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Isaiah Hartenstein", "???", "Rebs+Asts", 10.5, 11.9, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Shai Gilgeous-Alexander", "OKC", "Rebs+Asts", 10.5, 9.8, 0.74, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Jalen Williams", "OKC", "Rebs+Asts", 9.5, 10.7, 1.0, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Jordan Goodwin", "PHX", "Rebs+Asts", 6.5, 7.3, 0.8, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Ajay Mitchell", "???", "Rebs+Asts", 6.0, 6.8, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Royce O'Neale", "PHX", "Rebs+Asts", 5.5, 6.2, 0.8, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Cason Wallace", "???", "Rebs+Asts", 4.0, 4.5, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Dillon Brooks", "PHX", "Rebs+Asts", 5.0, 7.2, 0.73, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Luguentz Dort", "???", "Rebs+Asts", 3.5, 4.0, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Grayson Allen", "PHX", "Rebs+Asts", 3.5, 4.0, 0.8, 0.20, -500, "PHX @ OKC", "07:30PM"),
    mp("Cade Cunningham", "DET", "Rebs+Asts", 16.0, 18.1, 0.68, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jalen Duren", "DET", "Rebs+Asts", 14.5, 16.4, 0.6, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Paolo Banchero", "ORL", "Rebs+Asts", 12.5, 14.8, 0.67, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Tobias Harris", "???", "Rebs+Asts", 6.5, 7.3, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Ausar Thompson", "???", "Rebs+Asts", 8.0, 9.0, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Desmond Bane", "ORL", "Rebs+Asts", 6.5, 7.3, 0.88, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Jalen Suggs", "ORL", "Rebs+Asts", 9.0, 11.2, 0.75, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Wendell Carter", "ORL", "Rebs+Asts", 7.5, 12.0, 0.75, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Franz Wagner", "ORL", "Rebs+Asts", 6.5, 7.3, 0.71, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Anthony Black", "ORL", "Rebs+Asts", 5.5, 6.2, 0.5, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Daniss Jenkins", "???", "Rebs+Asts", 5.0, 5.6, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Isaiah Stewart", "???", "Rebs+Asts", 4.5, 5.1, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Duncan Robinson", "???", "Rebs+Asts", 4.0, 4.5, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Tristan Silva", "???", "Rebs+Asts", 2.5, 2.8, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Victor Wembanyama", "SAS", "Rebs+Asts", 15.5, 17.5, 0.54, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Deni Avdija", "POR", "Rebs+Asts", 13.5, 14.8, 0.65, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Donovan Clingan", "POR", "Rebs+Asts", 13.0, 18.2, 0.75, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Stephon Castle", "SAS", "Rebs+Asts", 13.0, 14.7, 0.75, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("De'Aaron Fox", "SAS", "Rebs+Asts", 9.5, 10.7, 0.9, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Jrue Holiday", "POR", "Rebs+Asts", 9.5, 9.2, 1.0, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Devin Vassell", "SAS", "Rebs+Asts", 5.5, 6.2, 1.0, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Toumani Camara", "POR", "Rebs+Asts", 6.5, 7.3, 0.5, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Julian Champagnie", "SAS", "Rebs+Asts", 6.5, 7.3, 0.67, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Robert Williams", "???", "Rebs+Asts", 6.5, 7.3, 0.87, 0.20, -500, "ORL @ DET", "10:30PM"),
    mp("Dylan Harper", "SAS", "Rebs+Asts", 5.5, 6.2, 0.0, 0.20, -500, "POR @ SAS", "01:00AM"),
    mp("Scoot Henderson", "POR", "Rebs+Asts", 5.0, 8.4, 0.8, 0.20, -500, "POR @ SAS", "01:00AM"),
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
      let key = p.player # "|" # p.stat;
      if (not Buffer.contains<Text>(seen, key, Text.equal)) {
        seen.add(key); out.add(p);
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
  public func get_best_10_pick() : async Text { await get_best_slip(10) };

  public func get_goblin_slip() : async Text {
    let ranked = rank_props(load_props());
    let goblins = Array.filter<Prop>(ranked, func(p) {
      p.line <= 5.0 and p.confidence > 0.72 and p.edge > 0.0
    });
    let deduped = one_per_player(goblins);
    let count = Nat.min(6, deduped.size());
    let picks = Array.tabulate<Prop>(count, func(i) { deduped[i] });
    total_slips := total_slips + 1;
    let j = to_json(picks);
    last_slip := j;
    j
  };

  public func get_value_slip() : async Text {
    let ranked = rank_props(load_props());
    let value = Array.filter<Prop>(ranked, func(p) {
      p.edge_pct > 0.15 and p.confidence > 0.72
    });
    let deduped = one_per_player(value);
    let count = Nat.min(6, deduped.size());
    let picks = Array.tabulate<Prop>(count, func(i) { deduped[i] });
    total_slips := total_slips + 1;
    let j = to_json(picks);
    last_slip := j;
    j
  };

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
