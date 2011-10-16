all:
	erlc *.erl

client: all
	erl -s echat_client start

clean:
	rm -f *.beam
	rm -f *~
	rm -f *.dump

