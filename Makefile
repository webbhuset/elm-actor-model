
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

SNIPPETS = $(patsubst example/%.elm,%,$(call rwildcard,example/snippets/,*Sandbox.elm))

all:
	elm make

snippets: $(SNIPPETS)


$(SNIPPETS):
	cd example; elm make $@.elm --output /dev/null

docs:
	elm make --docs=docs.json
