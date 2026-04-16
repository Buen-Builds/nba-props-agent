import urllib.request, json, subprocess
from datetime import datetime

WORKER = "https://prizepicks-proxy.crisbuen.workers.dev"

def fetch(path):
    req = urllib.request.Request(f"{WORKER}{path}", headers={'User-Agent': 'Mozilla/5.0'})
    return json.loads(urllib.request.urlopen(req).read())

def log(msg):
    print(f"[{datetime.now().strftime('%H:%M:%S')}] {msg}")

def run():
    log("Fetching today's schedule...")
    schedule = fetch("/schedule")
    games = schedule.get('games', [])
    date = schedule.get('date', 'Unknown')
    log(f"Next games: {date}")
    
    teams_playing = set()
    for g in games:
        log(f"  {g['away']} @ {g['home']} — {g['time']}")
        teams_playing.add(g['homeAbbr'])
        teams_playing.add(g['awayAbbr'])
    
    log(f"Teams playing: {teams_playing}")

    log("Fetching props from Worker...")
    props_data = fetch("/props")
    props = props_data.get('props', [])
    log(f"Total props in Worker: {len(props)}")

    log("Fetching injuries...")
    try:
        injuries = fetch("/injuries")
        inj_list = injuries.get('injuries', [])
        log(f"Injuries found: {len(inj_list)}")
    except Exception as e:
        log(f"Injury fetch failed: {e}")
        inj_list = []

    injured_players = set()
    for p in inj_list:
        if isinstance(p, dict):
            name = p.get('player', p.get('name', ''))
            status = p.get('status', p.get('injury', ''))
            if any(s in str(status).lower() for s in ['out', 'dtd', 'questionable']):
                injured_players.add(name)

    log(f"Injured/questionable: {len(injured_players)}")

    with open('/tmp/nba_schedule.json', 'w') as f:
        json.dump({'date': date, 'games': games, 'teams': list(teams_playing)}, f)
    
    with open('/tmp/nba_injured.txt', 'w') as f:
        f.write('\n'.join(injured_players))

    log("Done. Schedule and injuries saved.")
    log(f"Run 'dfx deploy' to update canister with today's games.")
    
    return games, teams_playing, injured_players

if __name__ == "__main__":
    run()
