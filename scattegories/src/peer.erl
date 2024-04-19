-module(peer).

-export([init_peer/2, get_me_peer/1, get_peer_nodes/1]).

-record (peer, {node, name, data}).

init_peer(Node, Name) ->
    #peer{node=Node, name=Name, data=not_ready}.

get_peer_nodes(Peers) ->
    lists:map(fun (Peer=#peer{node=PeerNode}) -> PeerNode end, Peers).

get_me_peer(Peers) ->
    {value, MePeer} = lists:search(fun (#peer{node=Node}) -> Node == node() end, Peers).
    MePeer.

set_peer_data(SetData, #peer{node=SetPeerNode}, Peers) ->
    lists:map(fun (Peer=#peer{node=PeerNode) -> case PeerNode of SetPeerNode -> Peer#peer{data=SetData}; _ -> Peer end, Peers).

get_data(Peers)
    lists:map(fun (#peer{data=Data}) -> Data end, Peers).
