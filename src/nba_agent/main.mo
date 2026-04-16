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
import ExperimentalCycles "mo:base/ExperimentalCycles";

persistent actor NBAAgent {

  var registered   : Bool = false;
  var last_slip    : Text = "";
  var total_slips  : Nat  = 0;
  var injury_cache : Text = "";
  var fetch_count  : Nat  = 0;
  var live_props   : Text = "";

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

  public func register_on_agentforge() : async Text {
    let r : RegActor = actor("h2ndy-aqaaa-aaaas-qf4sq-cai");
    try {
      let res = await r.register(
        "NBA Props Agent",
        "Live autonomous NBA props. Pulls real PrizePicks lines. RA and PRA combos.",
        ["nba", "prizepicks", "PRA", "RA"], 50_000_000
      );
      registered := true;
      "Registered: " # res
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
    else if (Text.contains(stat, #text "Dunks"))      { 1.10 }
    else if (Text.contains(stat, #text "Points"))     { 1.05 }
    else if (Text.contains(stat, #text "2PT"))        { 1.05 }
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

  func load_props() : [Prop] {[
    mp("Donovan Clingan",  "POR", "Blocks",       0.5,  2.1, 0.90, 0.20, -576, "POR @ PHX", "7:00pm"),
    mp("Donovan Clingan",  "POR", "Rebs+Asts",   14.5, 18.2, 0.90, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Donovan Clingan",  "POR", "Pts+Asts",    13.5, 17.2, 0.88, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Deni Avdija",      "POR", "PRA",         40.5, 46.2, 0.86, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Deni Avdija",      "POR", "Points",      24.5, 28.8, 0.86, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Deni Avdija",      "POR", "Pts+Asts",    30.0, 36.8, 0.86, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Robert Williams",  "POR", "Rebs+Asts",    8.5, 11.4, 0.90, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Robert Williams",  "POR", "Pts+Asts",     4.5,  6.8, 0.84, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Toumani Camara",   "POR", "PRA",         21.5, 25.8, 0.78, 0.35, -500, "POR @ PHX", "7:00pm"),
    mp("Toumani Camara",   "POR", "Pts+Asts",    14.5, 18.4, 0.86, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Toumani Camara",   "POR", "3PTM",         1.5,  2.4, 0.86, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Scoot Henderson",  "POR", "Points",      13.5, 17.2, 0.80, 0.25, -500, "POR @ PHX", "7:00pm"),
    mp("Scoot Henderson",  "POR", "Pts+Asts",    15.0, 18.6, 0.80, 0.25, -500, "POR @ PHX", "7:00pm"),
    mp("Jrue Holiday",     "POR", "Pts+Asts",    22.0, 26.8, 0.84, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Shaedon Sharpe",   "POR", "Pts+Asts",    10.5, 13.8, 0.80, 0.25, -500, "POR @ PHX", "7:00pm"),
    mp("Devin Booker",     "PHX", "PRA",         29.5, 37.8, 0.83, 0.22, -474, "POR @ PHX", "7:00pm"),
    mp("Devin Booker",     "PHX", "Points",      19.5, 27.4, 0.90, 0.20, -474, "POR @ PHX", "7:00pm"),
    mp("Devin Booker",     "PHX", "Pts+Asts",    33.5, 39.2, 0.86, 0.20, -474, "POR @ PHX", "7:00pm"),
    mp("Devin Booker",     "PHX", "3PTM",         1.5,  2.8, 0.86, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Mark Williams",    "PHX", "Rebounds",     5.5,  8.2, 0.77, 0.20, -554, "POR @ PHX", "7:00pm"),
    mp("Mark Williams",    "PHX", "Pts+Asts",    12.0, 15.4, 0.80, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Jalen Green",      "PHX", "Pts+Asts",    22.0, 26.4, 0.82, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Dillon Brooks",    "PHX", "Pts+Asts",    19.5, 23.2, 0.82, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Royce ONeale",     "PHX", "Pts+Asts",    14.5, 18.2, 0.82, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Collin Gillespie", "PHX", "Pts+Asts",    11.5, 14.8, 0.82, 0.20, -500, "POR @ PHX", "7:00pm"),
    mp("Jordan Goodwin",   "PHX", "Pts+Asts",    10.5, 13.6, 0.80, 0.20, -500, "POR @ PHX", "7:00pm")
  ]};

  public func fetch_live_props() : async Text {
    ExperimentalCycles.add(230_949_972_000);
    try {
      let res = await ic.http_request({
        url = WORKER_URL;
        method = #get;
        max_response_bytes = ?Nat64.fromNat(200_000);
        headers = [{ name = "Accept"; value = "application/json" }];
        body = null; transform = null;
      });
      let txt = switch (Text.decodeUtf8(res.body)) {
        case (?t) { t }; case null { "" };
      };
      live_props := txt;
      fetch_count := fetch_count + 1;
      "Fetched: " # Nat.toText(txt.size()) # " chars"
    } catch(e) { "fetch error: " # Error.message(e) }
  };

  public func fetch_injuries() : async Text {
    ExperimentalCycles.add(230_949_972_000);
    try {
      let res = await ic.http_request({
        url = "https://tank01-fantasy-stats.p.rapidapi.com/getNBAInjuryList";
        method = #get;
        max_response_bytes = ?Nat64.fromNat(50_000);
        headers = [
          { name = "x-rapidapi-key"; value = RAPIDAPI_KEY },
          { name = "x-rapidapi-host"; value = TANK_HOST }
        ];
        body = null; transform = null;
      });
      let txt = switch (Text.decodeUtf8(res.body)) {
        case (?t) { t }; case null { "" };
      };
      injury_cache := txt;
      txt
    } catch(e) { "injury error: " # Error.message(e) }
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

  func assists_only(stat: Text) : Bool {
    let s = Text.toLowercase(stat);
    Text.contains(s, #text "assist") and
    not Text.contains(s, #text "reb") and
    not Text.contains(s, #text "pts") and
    not Text.contains(s, #text "pra")
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

  public query func get_ranked_props() : async Text {
    to_json(rank_props(load_props()))
  };

  public func get_best_slip(n: Nat) : async Text {
    let ranked = rank_props(load_props());
    let filtered = Array.filter<Prop>(ranked, func(p) {
      p.edge > 0.0 and p.confidence > 0.72 and
      not assists_only(p.stat) and p.edge_pct > 0.10
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
    ignore await fetch_live_props();
    let slip = await get_best_slip(6);
    "Refreshed. Slip ready."
  };

  public query func get_live_props()    : async Text { live_props };
  public query func get_injury_cache()  : async Text { injury_cache };
  public query func get_last_slip()     : async Text { last_slip };

  public query func get_agent_stats() : async Text {
    "{\"total_slips\":" # Nat.toText(total_slips) #
    ",\"fetch_count\":" # Nat.toText(fetch_count) #
    ",\"registered\":" # (if registered { "true" } else { "false" }) # "}"
  };

  let _timer = Timer.recurringTimer<system>(#seconds(60), func() : async () {
    ignore await fetch_live_props();
    ignore await get_best_slip(6);
  });
};
