REBAR = ./rebar
DEPS_DIR ?= $(CURDIR)/deps
export DEPS_DIR

default: compile

all: deps compile

compile:
	$(REBAR) compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

distclean: clean 
	$(REBAR) delete-deps

test-compile: test-deps
	$(REBAR) -C rebar.test.config compile

test-deps:
	$(REBAR) -C rebar.test.config get-deps

CT_RUN = ct_run \
	-noshell \
	-pa ebin $(DEPS_DIR)/*/ebin \
	-dir test \
	-logdir logs

test: test-compile
	@mkdir -p logs/
	@$(CT_RUN) -suite pipeline_SUITE

docs: deps
	$(REBAR) skip_deps=true doc

dialyzer: compile
	@dialyzer -Wno_return -c ebin

.PHONY: all deps test
