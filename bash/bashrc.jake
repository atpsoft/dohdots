export EDITOR='subl -w'

# dwc develevopment
export RACK_ENV=development

# Aliases

alias la='ls -la'
alias ll='ls -l'
alias ..="cd .."
alias ...="cd ../../"


alias lsfs="cd ~/src/lsfs"
alias aiview="~/src/lsfs/apps/agent_interface/views"
alias aipublic="~/src/lsfs/apps/agent_interface/public"

alias srcdir="cd ~/src"
alias workdir="cd ~/BTSync/main/docs/work"

# ssh dwcs -L 9306:10.0.4.11:3306 -nN

#--------------------------------------------
# Aliases and this file:
#--------------------------------------------
# showa: to remind yourself of an alias (given some part of it)
showa () { /usr/bin/grep -i -a1 $@ ~/.bashrc.user | grep -v '^\s*$' ; }

# sourcea: to source this file (to make changes active after editing)
alias sourcea='source ~/.bashrc.user'

# VeraCrypt
alias veracrypt='/Applications/VeraCrypt.app/Contents/MacOS/VeraCrypt --text'

#-----------------------
# Correct common typos:
#-----------------------
alias mann='man'
alias 'cd..'='cd ..'

#------------------------------
# Terminal & shell management:
#------------------------------
# fix_stty: restore terminal settings when they get completely screwed up
alias fix_stty='stty sane'

# cic: make tab-completion case-insensitive
alias cic='set completion-ignore-case On'

# show_options: display bash options settings
alias show_options='shopt'

# use 'tput' to access terminfo - e.g. 'tput cols' to get number of columns

# name tabs to any custom name
nametab() { printf "\033]0;"$@"\007\003"; }

# escape codes helper
if [ -f ~/BTSync/main/bin/iterm_helpers.sh ]; then
  . ~/BTSync/main/bin/iterm_helpers.sh
fi

#--------------------------
# File & folder management:
#--------------------------
# various 'ls' shortcuts
# ll ()  { /bin/ls -aOl "$@" | /usr/bin/more ; }
lll () { /bin/ls -aOle "$@" | /usr/bin/more ; }
lt ()  { /bin/ls -lt "$@" | /usr/bin/more ; }
lsr () { /bin/ls -l "$@"/..namedfork/rsrc ; }

# The following aliases (save & show) are for saving frequently used directories
# You can save a directory using an abbreviation of your choosing. Eg. save ms
# You can subsequently move to one of the saved directories by using cd with
# the abbreviation you chose. Eg. cd ms  (Note that no '$' is necessary.)
# (I got this technique from Michael Boyle in the late 1980's at Visual Edge)
alias sdirs='source ~/.dirs'
alias show='cat ~/.dirs'
save () { /usr/bin/sed "/$@/d" ~/.dirs > ~/.dirs1; \mv ~/.dirs1 ~/.dirs; echo "$@"=\"`pwd`\" >> ~/.dirs; source ~/.dirs ; }

# Initialization for the above 'save' facility:
# source the .sdirs file:
touch ~/.dirs
source ~/.dirs

# set the bash option so that no '$' is required when using the above facility
shopt -s cdable_vars

# cdf: cd's to frontmost window of Finder
cdf ()
{
    currFolderPath=$( /usr/bin/osascript <<"    EOT"
        tell application "Finder"
            try
                set currFolder to (folder of the front window as alias)
            on error
                set currFolder to (path to desktop folder as alias)
            end try
            POSIX path of currFolder
        end tell
    EOT
    )
    echo "cd to \"$currFolderPath\""
    cd "$currFolderPath"
}

# rm_DS_Store_files: removes all .DS_Store file from the current dir and below
alias rm_DS_Store_files='find . -name .DS_Store -exec rm {} \;'

# zipf: to create a ZIP archive of a file or folder
zipf () { zip -r "$1".zip "$1" ; }

# numFiles: number of (non-hidden) files in current directory
alias numFiles='echo $(ls -1 | wc -l)'

# showTimes: show the modification, metadata-change, and access times of a file
showTimes () { stat -f "%N:   %m %c %a" "$@" ; }

# finderComment: show the SpotLight comment for a file
finderComment () { mdls "$1" | grep kMDItemFinderComment ; }

# to remove filename extensions in bash: ${file%\.[^.]*}


# Show/Hide files in Finder
alias showFiles='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
alias hideFiles='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'

#------------------------
# Git & repo management:
#------------------------

# unalias gpl
function gpl() {
  if [ "$PWD" == "/Users/$USER/src/lsfs" ]
  then
     git pl
     cd ../bootstrap_ai
     git pl
     cd ../bootstrap_mi
     git pl
     cd ~/src/lsfs
  else
     git pl
  fi
}

# git commands to lsfs & bootstrap repos
function gitai {
  if [ "$PWD" == "/Users/$USER/src/lsfs" ]
  then
     git "$@"
     cd ../bootstrap_ai
     git "$@"
     cd ~/src/lsfs
  else
    echo "You must be in /Users/$USER/src/lsfs"
  fi
}
function gitccy {
  if [ "$PWD" == "/Users/$USER/src/lsfs" ]
  then
     git "$@"
     cd ../bootstrap_ccy
     git "$@"
     cd ~/src/lsfs
  else
    echo "You must be in /Users/$USER/src/lsfs"
  fi
}
function gitmls {
  if [ "$PWD" == "/Users/$USER/src/lsfs" ]
  then
     git "$@"
     cd ../bootstrap_mls
     git "$@"
     cd ~/src/lsfs
  else
    echo "You must be in /Users/$USER/src/lsfs"
  fi
}

#---------------------
# Image Manipulation:
#---------------------

# imagemagick
# export MAGICK_HOME="$HOME/Dropbox/bin/ImageMagick"
# export PATH="$MAGICK_HOME/bin:$PATH"
# export DYLD_LIBRARY_PATH="$MAGICK_HOME/lib/"

