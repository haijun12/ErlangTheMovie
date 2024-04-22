-module(gamepeer).

-export([init/2, get_me_peer/1, get_peer_nodes/1, set_peer_data/3, set_all_data/2, shift_data/1, get_data/1, get_username_data_old/1, get_username_data/1, remove_peer/2, get_peer_data/1]).

-export([add_peer_points/2, get_current_points/1, check_valid_vote/2]).
-record (peer, {node, name, data, data_old, points}).

%% -define(DEBUG(Format, Args), io:format("[DEBUG] [gamepeer.erl] " ++ Format, Args)).
-define(DEBUG(Format, Args), void).

init(Node, Name) ->
    #peer{node=Node, name=Name, data=not_ready, data_old=ready, points=0}.

get_me_peer(Peers) ->
    {value, MePeer} = lists:search(fun (#peer{node=Node}) -> Node == node() end, Peers),
    MePeer.

get_peer_nodes(Peers) ->
    lists:map(fun (#peer{node=PeerNode}) -> PeerNode end, Peers).

get_peer_data(#peer{data=Data}) -> Data.

set_peer_data(SetData, #peer{node=SetPeerNode}, Peers) ->
    lists:map(fun (Peer=#peer{node=PeerNode}) ->
                  case PeerNode of
                      SetPeerNode -> Peer#peer{data=SetData};
                      _ -> Peer
                  end
              end, Peers).

add_peer_points(Username, Peers) ->
     lists:map(fun (Peer=#peer{name=PeerName, points=Points}) ->
                  case PeerName of
                      Username -> Peer#peer{points=Points + 1};
                      _ -> Peer
                  end
              end, Peers).

get_current_points(Peers) -> 
    NamePoints = lists:map(fun (#peer{name=Name, points=Points}) -> {Name, Points} end, Peers),
    lists:sort(fun ({_, P1}, {_, P2}) -> P1 < P2 end, NamePoints).

check_valid_vote(Input, Peers) ->
    lists:any(fun (#peer{name=Name, node=Node}) -> Name == Input andalso Node =/= node() end, Peers).

set_all_data(SetData, Peers) ->
    lists:map(fun (Peer) -> Peer#peer{data=SetData} end, Peers).

shift_data(Peers) ->
    lists:map(fun (Peer=#peer{data=Data}) -> Peer#peer{data_old=Data} end, Peers).

get_data(Peers) ->
    lists:map(fun (#peer{data=Data}) -> Data end, Peers).

get_username_data(Peers) ->
    lists:map(fun (#peer{name=Name, data=Data}) -> {Name, Data} end, Peers).

get_username_data_old(Peers) ->
    lists:map(fun (#peer{name=Name, data_old=DataOld}) -> {Name, DataOld} end, Peers).

remove_peer(#peer{node=RemoveNode}, Peers) ->
    lists:filter(fun (#peer{node=Node}) -> RemoveNode =/= Node end, Peers).
