# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))
PROJECT := $(subst -,_,$(PROJECT))

RUST_SRC_OUTPUT ?= $(CURDIR)/../priv/libsql_nif.so

.DEFAULT_GOAL := $(RUST_SRC_OUTPUT)

$(RUST_SRC_OUTPUT):
	@mkdir -p $(BASEDIR)/priv/
	@cargo build --release
	@cp target/release/liblibsql_nif.dylib $(RUST_SRC_OUTPUT)

clean:
	@cargo clean
	@rm -rf $(RUST_SRC_OUTPUT)
