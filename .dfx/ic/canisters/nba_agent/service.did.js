export const idlFactory = ({ IDL }) => {
  const HttpHeader = IDL.Record({ 'value' : IDL.Text, 'name' : IDL.Text });
  const HttpResponse = IDL.Record({
    'status' : IDL.Nat,
    'body' : IDL.Vec(IDL.Nat8),
    'headers' : IDL.Vec(HttpHeader),
  });
  return IDL.Service({
    'fetch_injuries' : IDL.Func([], [IDL.Text], []),
    'fetch_live_props' : IDL.Func([], [IDL.Text], []),
    'get_agent_stats' : IDL.Func([], [IDL.Text], ['query']),
    'get_best_10_pick' : IDL.Func([], [IDL.Text], []),
    'get_best_4_pick' : IDL.Func([], [IDL.Text], []),
    'get_best_5_pick' : IDL.Func([], [IDL.Text], []),
    'get_best_6_pick' : IDL.Func([], [IDL.Text], []),
    'get_best_slip' : IDL.Func([IDL.Nat], [IDL.Text], []),
    'get_goblin_slip' : IDL.Func([], [IDL.Text], []),
    'get_injury_cache' : IDL.Func([], [IDL.Text], ['query']),
    'get_last_slip' : IDL.Func([], [IDL.Text], ['query']),
    'get_live_props' : IDL.Func([], [IDL.Text], ['query']),
    'get_ranked_props' : IDL.Func([], [IDL.Text], ['query']),
    'get_value_slip' : IDL.Func([], [IDL.Text], []),
    'refresh_data' : IDL.Func([], [IDL.Text], []),
    'register_on_agentforge' : IDL.Func([], [IDL.Text], []),
    'transform' : IDL.Func(
        [
          IDL.Record({
            'context' : IDL.Vec(IDL.Nat8),
            'response' : HttpResponse,
          }),
        ],
        [HttpResponse],
        ['query'],
      ),
  });
};
export const init = ({ IDL }) => { return []; };
