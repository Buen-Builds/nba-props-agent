import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Int "mo:base/Int";
import Float "mo:base/Float";
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";
import Time "mo:base/Time";
import Debug "mo:base/Debug";
import Error "mo:base/Error";
import Blob "mo:base/Blob";
import Nat64 "mo:base/Nat64";
import ExperimentalCycles "mo:base/ExperimentalCycles";

persistent actor NBAAgent {

  type Prop = {
    player       : Text;
    team         : Text;
    stat         : Text;
    line         : Float;
    direction    : Text;
    avg          : Float;
    edge         : Float;
    hit_rate     : Float;
    blowout_risk : Float;
    role_score   : Float;
    confidence   : Float;
    game         : Text;
    game_time    : Text;
    odds         : Int;
  };

  stable var registered_on_agentforge : Bool = false;
  stable var last_slip_json           : Text = "";
  stable var total_slips_generated    : Nat  = 0;

  type RegistryActor = actor {
    register : (Text, Text, [Text], Nat) -> async Text;
  };

  type MemoryActor = actor {
    set : (Text, Text) -> async ();
    get : (Text) -> async ?Text;
  };

  func memory() : MemoryActor {
    actor("hupoq-3aaaa-aaaas-qf4tq-cai")
  };

  public func register_on_agentforge() : async Text {
    let registry : RegistryActor = actor("h2ndy-aqaaa-aaaas-qf4sq-cai");
    try {
      let result = await registry.register(
        "NBA Props Agent",
        "Autonomous NBA player prop analysis. Fetches live stats and outputs ranked PrizePicks power play slips with confidence scores.",
        ["nba", "sports-betting", "prizepicks", "props-analysis"],
        50_000_000
      );
      registered_on_agentforge := true;
      "Registered on AgentForge: " # result
    } catch (e) {
      "Registration error: " # Error.message(e)
    }
  };

  public func store_slip(slip_json: Text) : async () {
    let mem = memory();
    await mem.set("slip:latest", slip_json);
    last_slip_json := slip_json;
  };

  public func get_stored_slip() : async ?Text {
    let mem = memory();
    await mem.get("slip:latest")
  };

  func score(hit_rate: Float, edge: Float, role: Float, blowout: Float, odds: Int) : Float {
    let edge_norm  = Float.min(edge / 5.0, 1.0);
    let odds_score = if (odds <= -500) { 1.0 }
                     else if (odds <= -300) { 0.7 }
                     else { 0.4 };
    (hit_rate * 0.35) + (edge_norm * 0.25) + (role * 0.20) +
    ((1.0 - blowout) * 0.15) + (odds_score * 0.05)
  };

  func load_props() : [Prop] {
    let raw : [(Text,Text,Text,Float,Float,Float,Float,Int,Text,Text)] = [
      ("Rudy Gobert",        "MIN", "FTM",          1.5, 4.5, 0.95, 0.25, -600, "MIN vs HOU", "6:30pm"),
      ("Rudy Gobert",        "MIN", "Blks+Stls",    1.5, 2.4, 0.92, 0.25, -600, "MIN vs HOU", "6:30pm"),
      ("Rudy Gobert",        "MIN", "Def Rebounds", 7.5, 9.8, 0.85, 0.25, -600, "MIN vs HOU", "6:30pm"),
      ("Rudy Gobert",        "MIN", "2PT Att",      7.5, 9.2, 0.82, 0.25, -600, "MIN vs HOU", "6:30pm"),
      ("Neemias Queta",      "BOS", "Steals",       0.5, 0.8, 0.88, 0.10, -500, "OKC @ BOS",  "4:30pm"),
      ("Neemias Queta",      "BOS", "Blks+Stls",    1.5, 2.1, 0.85, 0.10, -500, "OKC @ BOS",  "4:30pm"),
      ("Neemias Queta",      "BOS", "Off Rebounds", 3.0, 3.5, 0.80, 0.10, -500, "OKC @ BOS",  "4:30pm"),
      ("Jay Huff",           "IND", "Dunks",        0.5, 2.2, 0.87, 0.30, -500, "LAL @ IND",  "4:00pm"),
      ("Jay Huff",           "IND", "2PT Att",      3.5, 5.8, 0.82, 0.30, -500, "LAL @ IND",  "4:00pm"),
      ("Jay Huff",           "IND", "Rebs+Asts",    5.5, 5.8, 0.72, 0.35, -450, "LAL @ IND",  "4:00pm"),
      ("Derrick White",      "BOS", "3PTM",         1.5, 3.1, 0.83, 0.10, -500, "OKC @ BOS",  "4:30pm"),
      ("Derrick White",      "BOS", "FTM",          1.5, 2.8, 0.78, 0.10, -450, "OKC @ BOS",  "4:30pm"),
      ("Kristaps Porzingis", "GSW", "PRA",         26.5,34.0, 0.75, 0.20, -500, "BKN @ GSW",  "7:00pm")
    ];
    Array.map<(Text,Text,Text,Float,Float,Float,Float,Int,Text,Text), Prop>(raw, func(r) {
      let (player,team,stat,line,avg,hit_rate,blowout,odds,game,game_time) = r;
      let edge = avg - line;
      {
        player; team; stat; line; direction="OVER"; avg; edge;
        hit_rate; blowout_risk=blowout; role_score=1.0;
        confidence = score(hit_rate, edge, 1.0, blowout, odds);
        game; game_time; odds
      }
    })
  };

  func rank(props: [Prop]) : [Prop] {
    let buf = Buffer.fromArray<Prop>(props);
    let n = buf.size();
    var i = 0;
    while (i < n) {
      var j = 0;
      while (j < n - i - 1) {
        if (buf.get(j).confidence < buf.get(j+1).confidence) {
          let tmp = buf.get(j);
          buf.put(j, buf.get(j+1));
          buf.put(j+1, tmp);
        };
        j += 1;
      };
      i += 1;
    };
    Buffer.toArray(buf)
  };

  func to_json(props: [Prop]) : Text {
    let parts = Array.map<Prop,Text>(props, func(p) {
      "{\"player\":\"" # p.player # "\",\"team\":\"" # p.team #
      "\",\"stat\":\"" # p.stat # "\",\"line\":" # Float.toText(p.line) #
      ",\"direction\":\"" # p.direction # "\",\"avg\":" # Float.toText(p.avg) #
      ",\"edge\":" # Float.toText(p.edge) # ",\"hit_rate\":" # Float.toText(p.hit_rate) #
      ",\"confidence\":" # Float.toText(p.confidence) #
      ",\"blowout_risk\":" # Float.toText(p.blowout_risk) #
      ",\"odds\":" # Int.toText(p.odds) # ",\"game\":\"" # p.game #
      "\",\"game_time\":\"" # p.game_time # "\"}"
    });
    "[" # Text.join(",", Iter.fromArray(parts)) # "]"
  };

  public query func get_ranked_props() : async Text {
    to_json(rank(load_props()))
  };

  public func get_best_slip(n: Nat) : async Text {
    let all = rank(load_props());
    let good = Array.filter<Prop>(all, func(p) { p.edge > 0.0 and p.confidence > 0.70 });
    let count = Nat.min(n, good.size());
    let picks = Array.tabulate<Prop>(count, func(i) { good[i] });
    total_slips_generated += 1;
    let j = to_json(picks);
    last_slip_json := j;
    ignore store_slip(j);
    j
  };

  public func get_best_6_pick() : async Text { await get_best_slip(6) };
  public func get_best_5_pick() : async Text { await get_best_slip(5) };

  public query func get_agent_stats() : async Text {
    "{\"total_slips\":" # Nat.toText(total_slips_generated) #
    ",\"registered\":" # (if registered_on_agentforge { "true" } else { "false" }) # "}"
  };

  public query func get_last_slip() : async Text { last_slip_json };

};
