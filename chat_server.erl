-module(chat_server).

-behaviour(gen_server).

-type user_name() :: atom().
-type client() :: {pid(), tag(), user_name()}.
-type tag() :: term().
-type message() :: {user_name(), string()}.

-define(SERVER, ?MODULE).
-record(chat_room, { roomname = "", clients = []::[client()], messages = []::[message()], pid = "" }).
-record(state, {rooms = [], count = ""}).
% record(state, { clients = []::[client()], messages = []::[message()] }).

%% API
-export([start_link/0, start/0, stop/0, chatRoomLoop/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).


start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
  	start_link().

stop() ->
  	gen_server:stop(?MODULE).

init([]) ->
	io:format("Initialising.. ~n"),
	io:format("registered processes are ~p ~n", [registered()]),
	{ok, #state{ rooms = [], count = 0}}.

handle_call(Request, From, State) ->
	{Pid, Tag} = From,
	io:format("Received request ~p from ~p ~n", [Request, From]),
	#state{rooms = Rooms, count = Count} = State,
	io:format("Current state ~p ~n~n", [State]),
	case Request of
		{getRoomList, Username} ->
			RoomNames = lists:map(fun get_room_name/1, Rooms),
			io:format("Server returning roomnames ~p ~n", [RoomNames]),
			{reply, RoomNames, State};
		{createRoom, Username} ->
			RoomPid = spawn((fun()-> chatRoomLoop() end)),
			io:format("Server created room with pid ~p ~n", [RoomPid]),
			erlang:monitor(process, Pid),
			RoomName = Count+1,
			Client = {Pid, Tag, Username},
			Clients = [Client],
			NewRooms = [ #chat_room{ roomname = RoomName, clients = Clients, messages = [], pid = RoomPid} | Rooms],
			{reply, RoomName, State#state{rooms = NewRooms, count = Count+1}};
		{join, Roomname, Username} ->
			erlang:monitor(process, Pid),
			Client = {Pid, Tag, Username},
			{NewRooms, Messages} = find_room_and_add_client(Rooms, Client, Roomname),
			io:format("Added client to room, NewRooms ~p ~n~n", [NewRooms]),
			{reply, {history, Messages}, State#state{rooms = NewRooms}};
		{say, Input} ->
			% io:format("~n~n~n~n~n~n say,input Rooms ~p ~n", [Rooms]),
			case get_meta_information(Pid, Rooms) of
				undefined -> {reply, user_not_registered, State};
				{Username, Roompid, Clients} ->
					Roompid ! {say, Input, Username, Clients},
					NewRooms = add_message_to_room(Username, Input, Roompid, Rooms),
					io:format("Message sent to chat room ~p ~n", [Roompid]),
					{reply, user_said_something, State#state{rooms = NewRooms}}
			end;
		_ ->
			{reply, unprocessable_request, State}
	end.


handle_cast(Request, State) ->
	case Request of
		{part, Pid} ->
			io:format("~n~n~n~n Quit request received ~n"),
  			NewRooms = remove_client_from_room(Pid, State#state.rooms),
  			{noreply, State#state{rooms = NewRooms}};
		_ ->
			{noreply, State}
	end.

handle_info(Info, State) ->
	case Info of
    	{'DOWN', _Ref, process, Pid, Why} ->
	      io:format("~p has died because of ~p, removing it from registered clients~n", [Pid, Why]),
	      NewRooms = remove_client_from_room(Pid, State#state.rooms),
  		  {noreply, State#state{rooms = NewRooms}};
    	_ ->
	      io:format("Received info: ~p~n", [Info]),
	      {noreply, State}
  	end.

terminate(_Reason, _State) ->
  io:format("Terminating~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



% Chat room process function

chatRoomLoop() ->
	io:format("chatRoomLoop started~n"),
	receive
		{say, Input, Username, Clients} ->
			write_to_all_clients(Username, Input, Clients),
			chatRoomLoop();
		_ ->
			chatRoomLoop()
	end.



%Processing Helper methods

get_clients_without_pid(Pid, Clients) ->
	% io:format("get_clients_without_pid Clients ~p ~n", [Clients]),
	% io:format("get_clients_without_pid Pid ~p ~n", [Pid]),
	lists:filter(fun(C) -> pid_doesnt_exist_in_client(C, Pid) end, Clients).

write_to_all_clients(Username, Text, Clients) ->
	Pids = lists:map(fun get_client_pid/1, Clients),
	SendMessageToPid = fun(Pid) ->
		io:format("Sending message from ~p to process ~p~n", [Username, Pid]),
		Pid ! {said, Username, Text}
	end,
	lists:foreach(SendMessageToPid, Pids).

%Add kill functionality for chat room if no clients left
remove_client_from_room(Pid, Rooms) ->
	OtherRooms = lists:filter(fun(C) -> room_does_not_have_client_pid(C, Pid) end, Rooms),
	% io:format("OtherRooms ~p ~n", [OtherRooms]),
	RoomOfInterestList = lists:filter(fun(C) -> room_has_client_pid(C, Pid) end, Rooms),
	% io:format("RoomOfInterestList ~p ~n", [RoomOfInterestList]),
	case RoomOfInterestList of
		[]-> OtherRooms;
		List -> 
			RoomOfInterest = lists:nth(1, List),
			NewClients = get_clients_without_pid(Pid, RoomOfInterest#chat_room.clients),
			case NewClients of
				[] -> 
					OtherRooms;
				_ -> 
					[#chat_room{roomname = RoomOfInterest#chat_room.roomname, clients = NewClients, messages = RoomOfInterest#chat_room.messages, pid = RoomOfInterest#chat_room.pid} | OtherRooms ]
			end
	end.

find_room_and_add_client(Rooms, Client, Roomname) ->
	OtherRooms = lists:filter(fun(C) -> room_does_not_have_roomname(C, Roomname) end, Rooms),
	RoomOfInterestList = lists:filter(fun(C) -> room_has_roomname(C, Roomname) end, Rooms),
	RoomOfInterest = lists:nth(1, RoomOfInterestList),
	NewClients = [ Client | RoomOfInterest#chat_room.clients ],
	NewRooms = [ #chat_room{roomname = RoomOfInterest#chat_room.roomname, clients = NewClients, messages = RoomOfInterest#chat_room.messages, pid = RoomOfInterest#chat_room.pid} | OtherRooms ],
	{NewRooms, RoomOfInterest#chat_room.messages}.

% Returns {Username, Roompid, Clients}
get_meta_information(Pid, Rooms) ->
	% io:format("get_meta_information Rooms ~p ~n", [Rooms]),
	% io:format("get_meta_information Pid ~p ~n", [Pid]),
	RoomOfInterestList = lists:filter(fun(C) -> room_has_client_pid(C, Pid) end, Rooms),
	case RoomOfInterestList of
		[] -> undefined;
		List -> 
			RoomOfInterest = lists:nth(1, List),
			Clients = RoomOfInterest#chat_room.clients,
			{get_username(Pid, Clients), RoomOfInterest#chat_room.pid, Clients}
	end.


add_message_to_room(Username, Input, Roompid, Rooms) ->
	OtherRooms = lists:filter(fun(C) -> room_does_not_have_pid(C, Roompid) end, Rooms),
	% io:format("OtherRooms ~p ~n", [OtherRooms]),
	RoomOfInterestList = lists:filter(fun(C) -> room_has_pid(C, Roompid) end, Rooms),
	% io:format("RoomOfInterestList ~p ~n", [RoomOfInterestList]),
	case RoomOfInterestList of
		[]-> OtherRooms;
		List -> 
			RoomOfInterest = lists:nth(1, List),
			NewMessages = add_message(Username, Input, RoomOfInterest#chat_room.messages),
			case NewMessages of
				[] -> 
					OtherRooms;
				_ -> 
					[#chat_room{roomname = RoomOfInterest#chat_room.roomname, clients = RoomOfInterest#chat_room.clients, messages = NewMessages, pid = RoomOfInterest#chat_room.pid} | OtherRooms ]
			end
	end.

add_message(Username, Input, Messages) ->
	NewMessages = [ {Username, Input} | Messages],
	case length(NewMessages) > 5 of
		true ->
			lists:reverse(tl(lists:reverse(NewMessages)));
		false ->
			NewMessages
	end.




%Room Helper methods

get_room_name(Room) -> 
  	Room#chat_room.roomname.

room_does_not_have_roomname(Room, Roomname) ->
	{Roomname_as_int, _} = string:to_integer(Roomname),
	Room#chat_room.roomname /= Roomname_as_int.

room_has_roomname(Room, Roomname) ->
	{Roomname_as_int, _} = string:to_integer(Roomname),
	Room#chat_room.roomname == Roomname_as_int.

room_does_not_have_pid(Room, RoomPid) ->
	Room#chat_room.pid /= RoomPid.
	
room_has_pid(Room, RoomPid) ->
	Room#chat_room.pid == RoomPid.

room_does_not_have_client_pid(Room, Pid) ->
	#chat_room{roomname = _, clients = Clients, messages = _, pid = _} = Room,
	% io:format("room_does_not_have_client_pid Room ~p ~n", [Room]),
	% io:format("room_does_not_have_client_pid Pid ~p ~n", [Pid]),
	lists:all(fun(C) -> pid_doesnt_exist_in_client(C, Pid) end, Clients).

room_has_client_pid(Room, Pid) ->
	% io:format("room_has_client_pid Room ~p ~n", [Room]),
	#chat_room{roomname = _, clients = Clients, messages = _, pid = _} = Room,
	% io:format("room_has_client_pid Clients ~p ~n", [Clients]),
	lists:any(fun(C) -> pid_exists_in_client(C, Pid) end, Clients).



%Client Helper methods

get_username(Pid, Clients) ->
	case lists:keyfind(Pid, 1, Clients) of
    	{_Pid, _Tag, Username} -> Username;
    	false -> false
  	end.

get_client_pid(Client) -> 
  case Client of
    {Pid, _Tag, _Username} -> Pid
  end.

pid_doesnt_exist_in_client(Client, Pid) ->
	get_client_pid(Client) /= Pid.

pid_exists_in_client(Client, Pid) ->
	% io:format("pid_exists_in_client Client ~p ~n", [Client]),
	get_client_pid(Client) == Pid.	
