-module(chat_client).

-export([start/2, messagingLoop/1]).

start(Username, Server) ->
	MessagingClientPid = spawn_link(?MODULE, messagingLoop, [Server]),
	join_loop(Username, MessagingClientPid),
	input_loop(MessagingClientPid).

join_loop(Username, MessagingClientPid) ->
	MessagingClientPid ! {getRoomList, Username},
	io:format("Please enter the room number you want to join. To create a new room, enter 'new' ~n"),
	Input = string:strip(io:get_line(""), both, $\n),
	case Input of
		"new" ->
			MessagingClientPid ! {createRoom, Username};
		Text ->
			case is_valid_room_number(Text) of
				true -> 
					MessagingClientPid ! {join, Text, Username};
				false ->
					io:format("Invalid input, please try again ~n"),
					join_loop(Username, MessagingClientPid)
			end	 
	end.

input_loop(MessagingClientPid) ->
	Input = string:strip(io:get_line(""), both, $\n),
	case Input of
		"quit" ->
			io:format("Quitting the chat room~n"),
			MessagingClientPid ! part;
		_ ->
			MessagingClientPid ! {say, Input},
			input_loop(MessagingClientPid)
	end.

messagingLoop(Server) ->
	receive
		{getRoomList, Username} ->
			Roomlist = gen_server:call(Server, {getRoomList, Username}),
			case Roomlist of
				[]->
					io:format("No existing rooms found. ~n Type 'new' to create a room ~n ~n"), 
					messagingLoop(Server);
				List -> 
					printRoomList(List),
					messagingLoop(Server)
			end;
		{createRoom, Username} ->
			Roomname = gen_server:call(Server, {createRoom, Username}),
			io:format("------------ You have joined room# ~p. Say Hi !~n", [Roomname]),
			messagingLoop(Server);
		{join, Roomname, Username} ->
			{history, Messages} = gen_server:call(Server, {join, Roomname, Username}),
			io:format("------------ You have joined room# ~p. Say Hi ! ~n", [Roomname]),
			printHistory(Messages),
			messagingLoop(Server);
		{say, Input} ->
			%io:format("Sending message to the server ~n"),
			gen_server:call(Server, {say, Input}),
			messagingLoop(Server);
		{said, Username, Input} ->
			printMessage(Username, Input),
			messagingLoop(Server);
		part ->
			io:format("Sending request to quit ~n"),
			gen_server:cast(Server, {part, self()}),
			init:stop()
	end.

printMessage(Username, Text) ->
	io:format("    ~p     : ~p ~n", [Username, Text]).

printHistory(History) ->
	%io:format("History ~p~n~n", [History]),
	FormatPrintMessages = fun(Text) ->
		case Text of
			{Username, Input} -> printMessage(Username, Input)
		end	
	end,
	lists:foreach(FormatPrintMessages, lists:reverse(History)).

printRoomList(List) ->
	FormatPrintRooms = fun(Text) ->
		io:format("Room Number# ~p ~n", [Text])	
	end,
	lists:foreach(FormatPrintRooms, List).

% TODO : Enhance this function to compare with existing room numbers.
% Current assumption : User always enters an existing number.
is_valid_room_number(Text) ->
	true.


