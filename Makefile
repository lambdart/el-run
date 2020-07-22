# Emacs invocation
# Don't load an init file: -q
# Avoid processing X resources: -Q
# Send messages to stderr: --batch
EMACS = emacs -Q -q --batch

# Remove command
RM = rm

# Define Compile Command (COMPILE)
# Call batch-byte-compile function: -f
COMPILE := -f batch-byte-compile

# Expand the source code files
EL != ls *.el

# Compiled files
ELC := $(EL:.el=.elc)

# Entry Point
all: compile

# Compile needed files
compile: $(ELC)

# Translate pure Elisp (.el) to byte compile (.elc)
$(ELC): $(EL)
	${EMACS} ${COMPILE} ${.ALLSRC}

# Remove {}.elc files
clean:
	${RM} ${ELC}
