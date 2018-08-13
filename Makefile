PROJECT = chat_room

.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<
.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean 

MODS = chat_server chat_client

compile: $(MODS)

all: compile

chat_server: $(chat_server)
	erlc -W chat_client.erl
	${ERL} -pa . -sname serv -setcookie pass -s


chat_client: compile
