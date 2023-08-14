FROM erlang:26-alpine

RUN mkdir /project
WORKDIR /project

COPY apps apps/
COPY config config/
COPY rebar.config .

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++ && \
    apk add --no-cache g++ && \
    apk add --no-cache git

RUN rebar3 release

EXPOSE 8080

CMD ["/project/_build/default/rel/todo/bin/todo", "foreground"]
