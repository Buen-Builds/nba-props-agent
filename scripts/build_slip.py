import urllib.request, json, re
from datetime import datetime, timezone

WORKER = "https://prizepicks-proxy.crisbuen.workers.dev"

def fetch(path):
    req = urllib.request.Request(f"{WORKER}{path}", headers={'User-Agent': 'Mozilla/5.0'})
    return json.loads(urllib.request.urlopen(req).read())

def log(msg):
    print(f"[{datetime.now().strftime('%H:%M:%S')}] {msg}")

# Load player intelligence database
import os
db_path = '/home/agentforge/nba-agent/scripts/player_db.json'
player_db = {}
if os.path.exists(db_path):
    player_db = json.load(open(db_path))

avgs = {
    "LaMelo Ball": {"PRA": 42.0, "Rebs+Asts": 15.2, "Points": 28.0},
    "Paolo Banchero": {"PRA": 41.0, "Rebs+Asts": 14.8, "Points": 26.0, "Rebounds": 9.2},
    "Nikola Jokic": {"PRA": 58.0, "Rebs+Asts": 23.0, "Points": 29.0},
    "Nikola Joki\u0107": {"PRA": 58.0, "Rebs+Asts": 23.0, "Points": 29.0},
    "Shai Gilgeous-Alexander": {"PRA": 45.0, "Rebs+Asts": 9.8, "Points": 33.0},
    "Deni Avdija": {"PRA": 46.2, "Rebs+Asts": 14.8, "Points": 28.8},
    "Donovan Clingan": {"PRA": 22.0, "Rebs+Asts": 18.2, "Rebounds": 12.0},
    "Devin Booker": {"PRA": 37.8, "Points": 27.4, "Rebs+Asts": 11.0},
    "Jayson Tatum": {"PRA": 47.8, "Rebs+Asts": 13.2, "Points": 28.0},
    "Giannis Antetokounmpo": {"PRA": 52.0, "Rebs+Asts": 16.0, "Points": 32.0},
    "Karl-Anthony Towns": {"PRA": 35.8, "Rebs+Asts": 14.2, "Points": 22.0},
    "Jalen Brunson": {"PRA": 37.2, "Rebs+Asts": 10.1, "Points": 26.0},
    "Stephen Curry": {"PRA": 41.2, "Rebs+Asts": 10.2, "Points": 26.4},
    "Anthony Edwards": {"PRA": 41.2, "Rebs+Asts": 9.8, "Points": 27.4},
    "Donovan Mitchell": {"PRA": 42.4, "Rebs+Asts": 9.2, "Points": 28.2},
    "Brandon Miller": {"PRA": 32.0, "Rebs+Asts": 8.2, "Points": 22.0, "Rebounds": 6.8},
    "Jalen Suggs": {"PRA": 26.0, "Rebs+Asts": 11.2, "Points": 16.0},
    "Wendell Carter": {"PRA": 20.0, "Rebs+Asts": 12.0, "Rebounds": 9.8},
    "Miles Bridges": {"PRA": 24.0, "Rebs+Asts": 8.2, "Points": 18.0, "Rebounds": 7.2},
    "Kevin Durant": {"PRA": 39.8, "Rebs+Asts": 11.4, "Points": 27.4},
    "Dillon Brooks": {"PRA": 24.2, "Points": 18.4, "Rebs+Asts": 7.2},
    "Jrue Holiday": {"PRA": 22.0, "Points": 16.8, "Rebs+Asts": 9.2},
    "Scoot Henderson": {"PRA": 22.0, "Points": 17.2, "Rebs+Asts": 8.4},
    "Moussa Diabat\xe9": {"PRA": 18.0, "Rebs+Asts": 14.0, "Rebounds": 11.2},
    "Moussa Diabate": {"PRA": 18.0, "Rebs+Asts": 14.0, "Rebounds": 11.2},
}

hit_rates = {
    "PRA": 0.85, "Rebs+Asts": 0.87, "Pts+Rebs": 0.84,
    "Rebounds": 0.83, "Points": 0.82, "Dunks": 0.87
}

skip_stats = {"Assists", "Free Throws Made", "Steals", "Personal Fouls", "Turnovers"}
skip_players = {"Cooper Flagg", "Ace Bailey", "Keyonte George"}

def get_next_games():
    try:
        schedule = fetch("/schedule")
        games = schedule.get('games', [])
        now_utc = datetime.now(timezone.utc)
        upcoming = []
        for g in games:
            try:
                tip = datetime.fromisoformat(g['time'].replace('Z','+00:00'))
                if tip > now_utc:
                    upcoming.append(g)
            except: pass
        if upcoming:
            log(f"Games still to play: {len(upcoming)}")
            return upcoming, schedule.get('date','')
        # Also check next 7 days from NBA CDN
    except Exception as e:
        log(f"Worker schedule error: {e}")

    log("Checking NBA CDN for next 7 days of games...")
    res = urllib.request.urlopen(urllib.request.Request(
        'https://cdn.nba.com/static/json/staticData/scheduleLeagueV2.json',
        headers={'User-Agent': 'Mozilla/5.0'}
    ))
    data = json.loads(res.read())
    dates = data['leagueSchedule']['gameDates']
    today = datetime.now()
    sorted_dates = sorted(dates, key=lambda x: x['gameDate'])
    all_upcoming = []
    game_date = ''
    for d in sorted_dates:
        try:
            dt = datetime.strptime(d['gameDate'].strip(), '%m/%d/%Y %H:%M:%S')
            if dt.date() >= today.date() and dt.date() <= (today + __import__('datetime').timedelta(days=7)).date() and d['games']:
                for g in d['games']:
                    tip = g.get('gameDateTimeUTC','')
                    try:
                        tip_dt = datetime.fromisoformat(tip.replace('Z','+00:00'))
                        if tip_dt > datetime.now(timezone.utc):
                            all_upcoming.append({
                                'home': g['homeTeam']['teamCity']+' '+g['homeTeam']['teamName'],
                                'away': g['awayTeam']['teamCity']+' '+g['awayTeam']['teamName'],
                                'homeAbbr': g['homeTeam']['teamTricode'],
                                'awayAbbr': g['awayTeam']['teamTricode'],
                                'time': tip
                            })
                            if not game_date:
                                game_date = d['gameDate']
                    except: pass
        except: pass
    if all_upcoming:
        log(f"Found {len(all_upcoming)} upcoming games in next 7 days")
        return all_upcoming, game_date
    return [], ''

def run():
    log("=== NBA Agent Build Slip ===")
    games, date = get_next_games()
    if not games:
        log("No upcoming games found")
        return

    for g in games:
        try:
            tip = datetime.fromisoformat(g['time'].replace('Z','+00:00')).strftime('%I:%M %p PT')
        except:
            tip = 'TBD'
        log(f"  {g['away']} @ {g['home']} — {tip}")

    game_label = {}
    for g in games:
        try:
            tip = datetime.fromisoformat(g['time'].replace('Z','+00:00')).strftime('%I:%M%p')
        except:
            tip = 'TBD'
        label = f"{g['awayAbbr']} @ {g['homeAbbr']}"
        game_label[g['homeAbbr']] = (label, tip)
        game_label[g['awayAbbr']] = (label, tip)

    injured = set()
    try:
        inj_data = fetch("/injuries")
        for p in inj_data.get('injuries', []):
            if isinstance(p, dict):
                name = p.get('player', p.get('name',''))
                status = str(p.get('status', p.get('injury',''))).lower()
                if 'out' in status:
                    injured.add(name)
        log(f"Players OUT: {len(injured)}")
    except Exception as e:
        log(f"Injury error: {e}")

    props_data = fetch("/props")
    props = props_data.get('props', [])
    log(f"Total props: {len(props)}")

    lines = []
    seen = set()
    skipped = 0
    for p in props:
        player = p['player']
        stat = p['stat']
        line = float(p['line'])
        if stat in skip_stats: continue
        if player in skip_players: continue
        if player in injured:
            skipped += 1
            continue
        # Skip players not playing tonight
        pdb_check = player_db.get(player, {})
        player_team = pdb_check.get('team', '???')
        if game_label and player_team != '???' and player_team not in game_label:
            continue
        key = f"{player}_{stat}"
        if key in seen: continue
        seen.add(key)
        avg = avgs.get(player, {}).get(stat, round(line * 1.13, 1))
        hr = hit_rates.get(stat, 0.82)
        safe = player.replace('"','').replace('\\','')
        pdb = player_db.get(player, {})
        team = pdb.get('team', '???')
        hr_adj = pdb.get('consistency', hr)
        # Match player to correct game using their team
        if team != '???' and team in game_label:
            game_info = game_label[team]
        elif game_label:
            game_info = list(game_label.values())[0]
        else:
            game_info = ('Tonight', 'TBD')
        lines.append(f'    mp("{safe}", "{team}", "{stat}", {line}, {avg}, {hr_adj}, 0.20, -500, "{game_info[0]}", "{game_info[1]}"),')

    log(f"Props after filtering: {len(lines)} (skipped {skipped} injured)")

    mo_path = '/home/agentforge/nba-agent/src/nba_agent/main.mo'
    content = open(mo_path).read()
    start = content.find('func load_props')
    end = content.find('};', start) + 2
    old = content[start:end]
    prop_block = '\n'.join(lines[:150])
    new = f'func load_props() : [Prop] {{[\n{prop_block}\n  ]}};'
    content = content.replace(old, new)
    content = re.sub(r'(mp\([^)]+\))\n(\s+mp\()', r'\1,\n\2', content)
    open(mo_path, 'w').write(content)
    log(f"Written {min(len(lines),150)} props to main.mo")
    log("Done — now run: dfx deploy nba_agent --network ic --mode reinstall")


def push_slip_to_worker():
    import subprocess, json as _json, re as _re
    log("Pushing slip to Cloudflare Worker...")
    result = subprocess.run(
        ['dfx', 'canister', 'call', 'p6ttk-lyaaa-aaaah-avb2q-cai', 'get_best_6_pick', '--network', 'ic'],
        capture_output=True, text=True, cwd='/home/agentforge/nba-agent'
    )
    raw = result.stdout
    open('/tmp/slip_raw.txt', 'w').write(raw)
    subprocess.run(['python3', '/tmp/push_slip.py'])


if __name__ == "__main__":
    run()
    push_slip_to_worker()

