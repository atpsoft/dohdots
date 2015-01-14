import sublime_plugin
import sublime
import imp
from . import dohutils
global dohConsoleActive
dohConsoleActive = False
dohutils.hide_console()

class ToggleKeepConsoleActiveCommand(sublime_plugin.TextCommand):
    def __init__(self, view):
        super(ToggleKeepConsoleActiveCommand, self).__init__(view)

    def run(self, edit):
        global dohConsoleActive
        dohConsoleActive = not dohConsoleActive
        imp.reload(dohutils)
        dohutils.show_console(dohConsoleActive)

class ToggleLogCommandsCommand(sublime_plugin.TextCommand):
    def __init__(self, view):
        super(ToggleLogCommandsCommand, self).__init__(view)
        self.logging_commands = False

    def run(self, edit):
        self.logging_commands = not self.logging_commands
        sublime.log_commands(self.logging_commands)

class ActivateViews(sublime_plugin.EventListener):
    def __init__(self):
        self.handling_event = False

    def on_activated(self, view):
        global dohConsoleActive

        if self.handling_event == True:
            return
        self.handling_event = True
        # if the view being activated is > 400 in size, it's not likely a panel, so lets show the console if it isn't already
        if view.viewport_extent()[1] > 400 and dohConsoleActive:
            dohutils.show_console()
        self.handling_event = False
