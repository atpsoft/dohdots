import sublime
import sublime_plugin


class OnnCycleFocusGroup(sublime_plugin.WindowCommand):
    def run(self):
        window = sublime.active_window()
        num = window.num_groups()
        active = window.active_group()
        if active == (num - 1):
            next = 0
        else:
            next = active + 1
        window.focus_group(next)
