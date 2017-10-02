PROJECT = luerl_demo
PROJECT_DESCRIPTION = Luerl by example tutorial demo
PROJECT_VERSION = 0.1.0
DEPS = lager luerl esdl2
dep_esdl2 = git https://github.com/ninenines/esdl2
ERLC_OPTS = +debug_info
include erlang.mk
# trying to ident with 4 spaces here.
SP = 4
# Compile flags
ERLC_COMPILE_OPTS= +'{parse_transform, lager_transform}'
# Append these settings
ERLC_OPTS += $(ERLC_COMPILE_OPTS)
TEST_ERLC_OPTS += $(ERLC_COMPILE_OPTS)
