help:
	@echo "Run examples:"
	@echo "  make [port=<port>] echo   # echo server"
	@echo "  make [port=<port>] chat   # chat server"

echo:
	env CLASSPATH=$(CLASSPATH):./src clj examples/echo_server.clj $(port)
chat:
	env CLASSPATH=$(CLASSPATH):./src clj examples/chat_server.clj $(port)
