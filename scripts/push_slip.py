import json, re, subprocess
raw = open('/tmp/slip_raw.txt').read()
match = re.search(r'"(\[.*\])"', raw, re.DOTALL)
if match:
    json_str = match.group(1).replace('\\"', '"')
    json_str = re.sub(r'\\([^"\\/bfnrtu])', r'\1', json_str)
    try:
        picks = json.loads(json_str)
        open('/tmp/slip_payload.json', 'w').write(json.dumps({"picks": picks}))
        subprocess.run(['curl', '-s', '-X', 'POST',
            'https://prizepicks-proxy.crisbuen.workers.dev/slip',
            '-H', 'Content-Type: application/json',
            '-d', '@/tmp/slip_payload.json'])
        print(f"\nPushed {len(picks)} picks")
        for p in picks:
            print(f'  {p["player"]} | {p["team"]} | {p["game"]}')
    except Exception as e:
        print(f"Error: {e}")
