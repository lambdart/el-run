# Emacs invocation
EMACS = emacs

# Remove command
RM = rm -f

# Define Compile Command (CC)
# Don't load an init file: -q
# Avoid processing X resources: -Q
# Send messages to stderr: --batch
# Call batch-byte-compile function: -f
CC := $(EMACS) -Q -q --batch -f batch-byte-compile

# Expand the source code files
EL != ls *.el

# Compiled files
ELC := $(EL:.el=.elc)

# transform lisp text (.el) files in byte compiled (.elc) files
$(ELC): $(EL)
	$(CC) ${.ALLSRC}

# Entry Point
all: compile

# Compile needed files
compile: $(ELC)

clean:
	${RM} *.elc
