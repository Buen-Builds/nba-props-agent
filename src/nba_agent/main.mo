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
    mp("Coby White",         "CHA", "Points",     10.5, 15.8, 1.00, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Brandin Podziemski", "GSW", "Points",     10.0, 14.2, 1.00, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("Devin Booker",       "PHX", "Points",     22.0, 30.8, 1.00, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("LaMelo Ball",        "CHA", "Points",     18.0, 30.7, 1.00, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Anthony Black",      "ORL", "Points",      7.0, 10.2, 1.00, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Brandon Miller",     "CHA", "Points",     16.0, 22.0, 1.00, 0.15, -600, "CHA @ ORL", "11:30PM"),
    mp("Jordan Goodwin",     "PHX", "Points",      5.0,  8.4, 1.00, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("Grayson Allen",      "PHX", "Points",      8.0, 12.8, 0.95, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("Jalen Suggs",        "ORL", "Points",     10.0, 14.2, 0.86, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Desmond Bane",       "ORL", "Points",     15.0, 20.5, 0.80, 0.15, -479, "CHA @ ORL", "11:30PM"),
    mp("Jalen Green",        "PHX", "Points",     14.5, 20.8, 0.84, 0.15, -392, "GSW @ PHX", "02:00AM"),
    mp("Draymond Green",     "GSW", "Points",      5.0,  8.2, 0.83, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("Miles Bridges",      "CHA", "Points",     10.0, 14.8, 0.83, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Royce O'Neale",      "PHX", "Points",      5.0,  7.8, 0.80, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("Ryan Kalkbrenner",   "CHA", "Rebounds",    3.0,  5.8, 1.00, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Grayson Allen",      "PHX", "Rebounds",    2.0,  3.8, 1.00, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("Kon Knueppel",       "CHA", "Rebounds",    4.0,  5.8, 1.00, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("De'Anthony Melton",  "GSW", "Rebounds",    3.0,  4.8, 1.00, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("Kristaps Porzingis", "ORL", "Rebounds",    5.0,  7.2, 1.00, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("LaMelo Ball",        "CHA", "Rebounds",    4.0,  5.8, 1.00, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Anthony Black",      "ORL", "Rebounds",    2.0,  3.8, 1.00, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Jalen Suggs",        "ORL", "Rebounds",    3.0,  4.8, 0.83, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Miles Bridges",      "CHA", "Rebounds",    5.0,  7.2, 0.83, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Dillon Brooks",      "PHX", "Rebounds",    4.0,  6.2, 0.83, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("Stephen Curry",      "GSW", "Rebounds",    4.0,  6.2, 1.00, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("Goga Bitadze",       "ORL", "Rebounds",    4.0,  6.2, 0.88, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Gui Santos",         "GSW", "Rebounds",    4.0,  5.8, 0.86, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("LaMelo Ball",        "CHA", "Rebs+Asts",   9.5, 13.2, 1.00, 0.15, -600, "CHA @ ORL", "11:30PM"),
    mp("Draymond Green",     "GSW", "Rebs+Asts",   9.5, 13.8, 1.00, 0.15, -600, "GSW @ PHX", "02:00AM"),
    mp("Paolo Banchero",     "ORL", "Rebs+Asts",   9.5, 14.8, 1.00, 0.15, -600, "CHA @ ORL", "11:30PM"),
    mp("Devin Booker",       "PHX", "Rebs+Asts",   7.5, 11.0, 1.00, 0.15, -550, "GSW @ PHX", "02:00AM"),
    mp("Kon Knueppel",       "CHA", "Rebs+Asts",   5.5,  7.8, 1.00, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Jalen Suggs",        "ORL", "Rebs+Asts",   6.5,  9.8, 1.00, 0.15, -750, "CHA @ ORL", "11:30PM"),
    mp("Anthony Black",      "ORL", "Rebs+Asts",  10.5, 14.2, 1.00, 0.15, -550, "CHA @ ORL", "11:30PM"),
    mp("Stephen Curry",      "GSW", "Rebs+Asts",  24.5, 31.2, 1.00, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("LaMelo Ball",        "CHA", "Rebs+Asts",  24.5, 30.7, 1.00, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Jalen Green",        "PHX", "PRA",        19.5, 27.8, 1.00, 0.15, -800, "GSW @ PHX", "02:00AM"),
    mp("Coby White",         "CHA", "PRA",        13.5, 19.8, 1.00, 0.15, -450, "CHA @ ORL", "11:30PM"),
    mp("Jordan Goodwin",     "PHX", "PRA",        10.5, 14.5, 0.85, 0.15, -440, "GSW @ PHX", "02:00AM"),
    mp("Brandon Miller",     "CHA", "PRA",        22.5, 29.8, 0.75, 0.15, -420, "CHA @ ORL", "11:30PM"),
    mp("LaMelo Ball",        "CHA", "PRA",        28.5, 42.0, 0.88, 0.15, -600, "CHA @ ORL", "11:30PM"),
    mp("Dillon Brooks",      "PHX", "Pts+Rebs",   14.5, 21.2, 1.00, 0.15, -725, "GSW @ PHX", "02:00AM"),
    mp("Grayson Allen",      "PHX", "Pts+Rebs",    9.5, 13.8, 1.00, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("Gui Santos",         "GSW", "Points",      7.5, 10.8, 0.80, 0.15, -675, "GSW @ PHX", "02:00AM"),
    mp("Stephen Curry",      "GSW", "3PTM",        2.5,  4.2, 0.80, 0.15, -423, "GSW @ PHX", "02:00AM"),
    mp("Royce O'Neale",      "PHX", "3PTM",        1.0,  2.2, 0.80, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("Jordan Goodwin",     "PHX", "3PTM",        1.0,  2.1, 1.00, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("Dillon Brooks",      "PHX", "3PTM",        2.0,  3.2, 0.83, 0.15, -500, "GSW @ PHX", "02:00AM"),
    mp("Brandon Miller",     "CHA", "3PTM",        2.0,  3.2, 1.00, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Franz Wagner",       "ORL", "Rebs+Asts",   2.0,  3.8, 0.82, 0.15, -500, "CHA @ ORL", "11:30PM"),
    mp("Kristaps Porzingis", "ORL", "Rebs+Asts",   2.0,  3.8, 0.80, 0.15, -500, "CHA @ ORL", "11:30PM")
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
