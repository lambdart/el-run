# Emacs invocation
EMACS_COMMAND := emacs

# do not load an init file: -q
# avoid processing X resources: -Q
# send messages to stderr: --batch
EMACS := $(EMACS_COMMAND) -Q -q --batch

EVAL   := $(EMACS) --eval
SCRIPT := $(EMACS) --script

# Compiled files
ELC := $(EL:.el=.elc)

.PHONY: all

# Compile all files at once
batch-compile:
	$(EMACS) $(LOADPATH) -f batch-byte-compile $(INIT)

all: batch-compile

$(ELC): %.elc: %.el
	$(EMACS) -f batch-byte-compile $<

# Compile needed files
compile: $(ELC)
