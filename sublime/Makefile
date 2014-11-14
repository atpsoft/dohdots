KEYMAP_FILE = Default\ \(OSX\).sublime-keymap
INPUT_DIR = ./root/
OUTPUT_DIR = ./output/
OUTPUT_FILE = ${KEYMAP_FILE}
MYNAME = $(shell whoami)
DATE=$(shell date +%I:%M%p)
HR=\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#\#


#
# BUILD KEYMAP FILE
#

build:
	@echo "\n${HR}"
	@echo "Building Sublime Keymap Files..."
	@echo "${HR}\n"
	@ruby parse.rb


# #
# # WATCH Sublime Default
# #

watch:
	echo "Watching keybindings file...";
	# watchr -e "watch('less/.*\.less') { system 'make' }"
	watchr -e "watch('${MYNAME}/${KEYMAP_FILE}') { system 'make' }"

