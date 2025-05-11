SERVER := service/server.rkt

.PHONY: run

run: $(SERVER)
	racket $(SERVER)
