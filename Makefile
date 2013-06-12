REBAR_REPO := https://github.com/basho/rebar.git
REBAR_VERSION := 2.0.0
REBAR_REPO_DIR := rebar
REBAR := $(REBAR_REPO_DIR)/rebar
DIALYZER_PLT := .dialyzer.plt
PRODUCTION_ERLS := $(wildcard src/*.erl)
PRODUCTION_BEAMS := $(addprefix ebin/, $(notdir $(PRODUCTION_ERLS:.erl=.beam)))

.PHONY: doc clean clean-all

all: compile doc

$(REBAR): $(REBAR_REPO_DIR)
	cd $(REBAR_REPO_DIR); ./bootstrap

$(REBAR_REPO_DIR):
	git clone $(REBAR_REPO) $(REBAR_REPO_DIR)
	cd $(REBAR_REPO_DIR); git checkout -q $(REBAR_VERSION)

get-deps: $(REBAR)
	$(REBAR) get-deps

compile: get-deps
	$(REBAR) compile

check: xref dialyzer

dialyzer: compile $(DIALYZER_PLT)
	dialyzer --plt $(DIALYZER_PLT) $(PRODUCTION_BEAMS) \
		 -Wunmatched_returns \
		 -Werror_handling \
		 -Wunderspecs \
		 -Wrace_conditions

$(DIALYZER_PLT):
	dialyzer --build_plt --output_plt $(DIALYZER_PLT) \
		 deps/samerlib/ebin \
		 --apps stdlib kernel tools syntax_tools compiler erts \

xref: compile
	$(REBAR) xref

# Run the tests with and without cover as it affects quite a bit to the
# behaviour
test: test-without-cover test-with-cover

test-with-cover: compile
	COVER_ENABLED=true $(REBAR) eunit skip_deps=true

test-without-cover: compile
	$(REBAR) eunit skip_deps=true

doc: $(REBAR)
	$(REBAR) doc skip_deps=true

clean:
	$(REBAR) clean

clean-all: clean
	$(REBAR) delete-deps
	rm -rf $(REBAR_REPO_DIR)
