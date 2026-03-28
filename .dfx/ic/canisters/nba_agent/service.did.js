export const idlFactory = ({ IDL }) => {
  return IDL.Service({
    'fetch_injuries' : IDL.Func([], [IDL.Text], []),
    'get_agent_stats' : IDL.Func([], [IDL.Text], ['query']),
    'get_best_4_pick' : IDL.Func([], [IDL.Text], []),
    'get_best_5_pick' : IDL.Func([], [IDL.Text], []),
    'get_best_6_pick' : IDL.Func([], [IDL.Text], []),
    'get_best_slip' : IDL.Func([IDL.Nat], [IDL.Text], []),
    'get_injury_cache' : IDL.Func([], [IDL.Text], ['query']),
    'get_last_slip' : IDL.Func([], [IDL.Text], ['query']),
    'get_ranked_props' : IDL.Func([], [IDL.Text], ['query']),
    'refresh_data' : IDL.Func([], [IDL.Text], []),
    'register_on_agentforge' : IDL.Func([], [IDL.Text], []),
  });
};
export const init = ({ IDL }) => { return []; };
