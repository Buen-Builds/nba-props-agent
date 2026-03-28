import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Int "mo:base/Int";
import Float "mo:base/Float";
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import Iter "mo:base/Iter";
import Error "mo:base/Error";

persistent actor NBAAgent {

  var registered  : Bool = false;
  var last_slip   : Text = "";
  var total_slips : Nat  = 0;

  type RegActor = actor { register: (Text, Text, [Text], Nat) -> async Text };
  type MemActor = actor { set: (Text, Text) -> async (); get: (Text) -> async ?Text };
  func mem() : MemActor { actor("hupoq-3aaaa-aaaas-qf4tq-cai") };

  public func register_on_agentforge() : async Text {
    let r : RegActor = actor("h2ndy-aqaaa-aaaas-qf4sq-cai");
    try {
      let res = await r.register(
        "NBA Props Agent",
        "Ranked PrizePicks slips. RA and PRA combos. No rookies. No assists-only.",
        ["nba","prizepicks","PRA","RA"],
        50_000_000
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

  func calc_score(h: Float, e: Float, ep: Float, b: Float, o: Int, stat: Text) : Float {
    let en  = Float.min(e / 5.0, 1.0);
    let epn = Float.min(ep / 0.5, 1.0);
    let os  = if (o <= -500) { 1.0 } else if (o <= -300) { 0.7 } else { 0.4 };
    let sb  = stat_bonus(stat);
    let raw = h * 0.30 + en * 0.20 + epn * 0.20 + 1.0 * 0.15 + (1.0 - b) * 0.10 + os * 0.05;
    Float.min(raw * sb, 1.0)
  };

  func classify(stat: Text) : Text {
    if      (Text.contains(stat, #text "PRA"))        { "PRA" }
    else if (Text.contains(stat, #text "Rebs+Asts"))  { "RA"  }
    else if (Text.contains(stat, #text "Pts+Rebs"))   { "PR"  }
    else if (Text.contains(stat, #text "Rebounds"))   { "REB" }
    else if (Text.contains(stat, #text "Points"))     { "PTS" }
    else                                              { "OTHER" }
  };

  func make_prop(pl:Text, tm:Text, st:Text, ln:Float, av:Float, hr:Float, bl:Float, od:Int, gm:Text, gt:Text) : Prop {
    let eg = av - ln;
    let ep = if (ln > 0.0) { eg / ln } else { 0.0 };
    { player=pl; team=tm; stat=st; line=ln; avg=av;
      edge=eg; edge_pct=ep; hit_rate=hr; blowout_risk=bl;
      confidence=calc_score(hr,eg,ep,bl,od,st);
      game=gm; game_time=gt; odds=od; stat_type=classify(st) }
  };

  func load_props() : [Prop] {[
    make_prop("Nikola Jokic",       "DEN","PRA",          24.5,51.3,0.95,0.15,-620,"DEN vs DAL","7:00pm"),
    make_prop("Neemias Queta",      "BOS","Rebs+Asts",     7.5,13.4,0.92,0.10,-500,"OKC @ BOS", "4:30pm"),
    make_prop("Donovan Clingan",    "POR","Rebs+Asts",    14.5,18.2,0.90,0.20,-500,"POR vs MIL","7:00pm"),
    make_prop("Evan Mobley",        "CLE","Rebs+Asts",    12.5,15.8,0.90,0.15,-500,"CLE vs MIA","4:30pm"),
    make_prop("Josh Giddey",        "CHI","Rebs+Asts",    14.5,17.8,0.88,0.25,-500,"CHI vs PHI","4:00pm"),
    make_prop("Deni Avdija",        "POR","Rebs+Asts",    11.5,14.8,0.88,0.20,-500,"POR vs MIL","7:00pm"),
    make_prop("Isaiah Hartenstein", "OKC","Rebs+Asts",     7.5,13.4,0.91,0.10,-650,"OKC @ BOS", "4:30pm"),
    make_prop("SGA",                "OKC","Rebs+Asts",     7.5, 9.8,0.90,0.10,-650,"OKC @ BOS", "4:30pm"),
    make_prop("Jaylen Brown",       "BOS","Rebs+Asts",     9.5,11.2,0.88,0.10,-500,"OKC @ BOS", "4:30pm"),
    make_prop("Jayson Tatum",       "BOS","Rebounds",      6.5, 8.6,0.88,0.10,-500,"OKC @ BOS", "4:30pm"),
    make_prop("LeBron James",       "LAL","Rebs+Asts",     9.5,12.6,0.90,0.20,-596,"LAL vs BKN","7:30pm"),
    make_prop("Austin Reaves",      "LAL","Rebs+Asts",     8.5,10.8,0.87,0.20,-500,"LAL vs BKN","7:30pm"),
    make_prop("Luka Doncic",        "LAL","Points",       30.5,33.4,0.85,0.20,-500,"LAL vs BKN","7:30pm"),
    make_prop("Pascal Siakam",      "IND","Rebounds",      3.5, 6.6,0.94,0.30,-550,"IND vs CHO","4:00pm"),
    make_prop("Nic Claxton",        "BKN","Rebounds",      3.5, 7.1,0.93,0.25,-600,"LAL vs BKN","7:30pm"),
    make_prop("Andrew Wiggins",     "MIA","Pts+Rebs",     11.5,16.8,0.93,0.20,-650,"MIA vs CLE","4:30pm"),
    make_prop("CJ McCollum",        "ATL","Rebs+Asts",     6.5, 8.4,0.87,0.20,-500,"BOS vs ATL","4:30pm"),
    make_prop("Andrew Nembhard",    "IND","Rebs+Asts",    10.5,13.1,0.87,0.25,-500,"IND vs CHO","4:00pm"),
    make_prop("Matas Buzelis",      "CHI","PRA",          19.5,24.1,0.85,0.25,-500,"CHI vs PHI","4:00pm"),
    make_prop("Derrick Jones Jr",   "LAC","Rebs+Asts",     5.5, 7.2,0.86,0.20,-500,"LAC vs UTA","9:00pm"),
    make_prop("Rudy Gobert",        "MIN","Rebs+Asts",     8.5,13.1,0.88,0.25,-600,"MIN vs HOU","6:30pm"),
    make_prop("Kawhi Leonard",      "LAC","Points",       19.5,28.2,0.85,0.20,-550,"LAC vs UTA","9:00pm"),
    make_prop("Jay Huff",           "IND","Dunks",         0.5, 2.2,0.87,0.30,-500,"IND vs CHO","4:00pm"),
    make_prop("Nikola Jokic",       "DEN","Rebs+Asts",    15.5,23.3,0.94,0.15,-620,"DEN vs DAL","7:00pm"),
    make_prop("Joel Embiid",        "PHI","Rebs+Asts",    10.5,13.2,0.85,0.20,-500,"CHI vs PHI","4:00pm"),
    make_prop("Jaime Jaquez",       "MIA","Rebs+Asts",     4.5, 6.8,0.84,0.20,-500,"CLE vs MIA","4:30pm"),
    make_prop("James Harden",       "CLE","Rebs+Asts",    13.5,13.0,0.82,0.15,-700,"CLE vs MIA","4:30pm"),
    make_prop("Derrick White",      "BOS","Pts+Rebs",     14.5,21.8,0.82,0.10,-500,"OKC @ BOS", "4:30pm")
  ]};

  func rank_props(ps: [Prop]) : [Prop] {
    let b = Buffer.fromArray<Prop>(ps);
    let n = b.size();
    var i = 0;
    while (i < n) {
      var j = 0;
      while (j < n - i - 1) {
        if (b.get(j).confidence < b.get(j+1).confidence) {
          let tmp = b.get(j);
          b.put(j, b.get(j+1));
          b.put(j+1, tmp);
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
        seen.add(p.player);
        out.add(p);
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
    let ranked   = rank_props(load_props());
    let filtered = Array.filter<Prop>(ranked, func(p) {
      p.edge > 0.0 and
      p.confidence > 0.72 and
      not assists_only(p.stat) and
      p.edge_pct > 0.10
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

  public query func get_agent_stats() : async Text {
    "{\"total_slips\":" # Nat.toText(total_slips) #
    ",\"registered\":" # (if registered { "true" } else { "false" }) # "}"
  };

  public query func get_last_slip() : async Text { last_slip };
};
