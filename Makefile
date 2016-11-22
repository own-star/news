PROJECT = news
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy jsx
dep_cowboy = git https://github.com/ninenines/cowboy.git 1.0.4
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.8.0

include erlang.mk
