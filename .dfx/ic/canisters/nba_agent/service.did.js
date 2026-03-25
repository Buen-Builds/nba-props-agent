export const idlFactory = ({ IDL }) => {
  return IDL.Service({
    'get_agent_stats' : IDL.Func([], [IDL.Text], ['query']),
    'get_best_5_pick' : IDL.Func([], [IDL.Text], []),
    'get_best_6_pick' : IDL.Func([], [IDL.Text], []),
    'get_best_slip' : IDL.Func([IDL.Nat], [IDL.Text], []),
    'get_last_slip' : IDL.Func([], [IDL.Text], ['query']),
    'get_ranked_props' : IDL.Func([], [IDL.Text], ['query']),
    'get_stored_slip' : IDL.Func([], [IDL.Opt(IDL.Text)], []),
    'register_on_agentforge' : IDL.Func([], [IDL.Text], []),
    'store_slip' : IDL.Func([IDL.Text], [], []),
  });
};
export const init = ({ IDL }) => { return []; };
