help:
	@echo "Run examples:"
	@echo "  make [port=<port>] echo   # echo server"

echo:
	env CLASSPATH=$(CLASSPATH):./src clj examples/echo_server.clj $(port)
