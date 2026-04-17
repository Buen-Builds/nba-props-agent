import type { Principal } from '@icp-sdk/core/principal';
import type { ActorMethod } from '@icp-sdk/core/agent';
import type { IDL } from '@icp-sdk/core/candid';

export interface HttpHeader { 'value' : string, 'name' : string }
export interface HttpResponse {
  'status' : bigint,
  'body' : Uint8Array | number[],
  'headers' : Array<HttpHeader>,
}
export interface _SERVICE {
  'fetch_injuries' : ActorMethod<[], string>,
  'fetch_live_props' : ActorMethod<[], string>,
  'get_agent_stats' : ActorMethod<[], string>,
  'get_best_10_pick' : ActorMethod<[], string>,
  'get_best_4_pick' : ActorMethod<[], string>,
  'get_best_5_pick' : ActorMethod<[], string>,
  'get_best_6_pick' : ActorMethod<[], string>,
  'get_best_slip' : ActorMethod<[bigint], string>,
  'get_goblin_slip' : ActorMethod<[], string>,
  'get_injury_cache' : ActorMethod<[], string>,
  'get_last_slip' : ActorMethod<[], string>,
  'get_live_props' : ActorMethod<[], string>,
  'get_ranked_props' : ActorMethod<[], string>,
  'get_value_slip' : ActorMethod<[], string>,
  'refresh_data' : ActorMethod<[], string>,
  'register_on_agentforge' : ActorMethod<[], string>,
  'transform' : ActorMethod<
    [{ 'context' : Uint8Array | number[], 'response' : HttpResponse }],
    HttpResponse
  >,
}
export declare const idlFactory: IDL.InterfaceFactory;
export declare const init: (args: { IDL: typeof IDL }) => IDL.Type[];
