import type { Principal } from '@icp-sdk/core/principal';
import type { ActorMethod } from '@icp-sdk/core/agent';
import type { IDL } from '@icp-sdk/core/candid';

export interface _SERVICE {
  'get_agent_stats' : ActorMethod<[], string>,
  'get_best_4_pick' : ActorMethod<[], string>,
  'get_best_5_pick' : ActorMethod<[], string>,
  'get_best_6_pick' : ActorMethod<[], string>,
  'get_best_slip' : ActorMethod<[bigint], string>,
  'get_last_slip' : ActorMethod<[], string>,
  'get_ranked_props' : ActorMethod<[], string>,
  'register_on_agentforge' : ActorMethod<[], string>,
}
export declare const idlFactory: IDL.InterfaceFactory;
export declare const init: (args: { IDL: typeof IDL }) => IDL.Type[];
