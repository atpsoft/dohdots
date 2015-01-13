import sublime_plugin
import sublime
import dohutils
import imp

class DebugSublimeCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        imp.reload(dohutils)
        dohutils.toggle_debug_mode()



class ActivateViews(sublime_plugin.EventListener):
    def __init__(self):
        self.handling_event = False

    def on_activated(self, view):
        if self.handling_event == True:
            return
        self.handling_event = True
        if view.viewport_extent()[1] > 50:
            dohutils.show_console_if_debugging()
        self.handling_event = False
