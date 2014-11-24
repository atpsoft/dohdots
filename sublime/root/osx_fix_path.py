import sublime, sublime_plugin
from os import environ
from subprocess import Popen, PIPE

def plugin_loaded():
    command = "/usr/bin/login -fql $USER $SHELL -l -c 'printf \"%s\" \"$PATH\"'"
    path = Popen(command, stdout=PIPE, shell=True).stdout.read().decode("utf-8")
    environ['PATH'] = path
