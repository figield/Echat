all:
	erlc *.erl

server: all
	erl -s echat_server start1 -sname server1 -setcookie secretcookie

server1: all
	erl -s echat_server start1 -sname server1 -setcookie secretcookie

server2: all
	erl -s echat_server start2 -sname server2 -setcookie secretcookie

clean:
	rm -f *.beam
	rm -f *~
	rm -f *.dump

