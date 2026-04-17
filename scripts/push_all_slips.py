import json, re, subprocess

def get_slip(method):
    result = subprocess.run(
        ['dfx', 'canister', 'call', 'p6ttk-lyaaa-aaaah-avb2q-cai', method, '--network', 'ic'],
        capture_output=True, text=True, cwd='/home/agentforge/nba-agent'
    )
    raw = result.stdout
    match = re.search(r'"(\[.*\])"', raw, re.DOTALL)
    if match:
        json_str = match.group(1).replace('\\"', '"')
        json_str = re.sub(r'\\([^"\\/bfnrtu])', r'\1', json_str)
        try:
            return json.loads(json_str)
        except: pass
    return []

slips = {
    'power': get_slip('get_best_6_pick'),
    'value': get_slip('get_value_slip'),
    'goblin': get_slip('get_goblin_slip'),
}

for name, picks in slips.items():
    payload = json.dumps({"picks": picks, "type": name})
    subprocess.run(['curl', '-s', '-X', 'POST',
        f'https://prizepicks-proxy.crisbuen.workers.dev/slip?type={name}',
        '-H', 'Content-Type: application/json',
        '-d', payload])
    print(f"{name}: {len(picks)} picks pushed")

print("All slips pushed!")
