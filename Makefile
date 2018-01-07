PROJECT = versa
PROJECT_DESCRIPTION = Versa DLT Project
PROJECT_VERSION = 0.1.0

# Whitespace to be used when creating files from templates.
SP = 2

DEPS = lager sync
LOCAL_DEPS = vaccord \
						 mnesia crypto public_key inets sasl

include erlang.mk
