import urllib.request, json, os
from datetime import datetime

WORKER_URL = "https://prizepicks-proxy.crisbuen.workers.dev/props"

def fetch_props():
    req = urllib.request.Request(
        WORKER_URL,
        headers={
            'User-Agent': 'Mozilla/5.0',
            'Accept': 'application/json'
        }
    )
    data = json.loads(urllib.request.urlopen(req).read())
    return data.get('props', [])

def log(msg):
    print(f"[{datetime.now().strftime('%H:%M:%S')}] {msg}")

def run():
    log("Fetching live props...")
    props = fetch_props()
    pra = [p for p in props if p['stat'] == 'PRA']
    ra  = [p for p in props if p['stat'] == 'Rebs+Asts']
    log(f"Got {len(pra)} PRA + {len(ra)} RA props")

    with open("/tmp/latest_props.json", "w") as f:
        json.dump(props, f, indent=2)

    log("Top 10 PRA lines tonight:")
    for p in sorted(pra, key=lambda x: x['line'])[:10]:
        print(f"  {p['player']} {p['stat']} {p['line']}")

    return props

if __name__ == "__main__":
    run()
