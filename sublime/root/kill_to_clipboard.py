import sublime_plugin
import sublime


class KillLocation:
    def __init__(self):
        self.kill_points = []
        self.kill_id = 0
        self.locked = False

    def check_for_cursor_change(self, view, save):
        view_id = view.id()
        if view_id != self.kill_id:
            print "kill_to_clipboard: check_for_cursor_change; view id changed, setting locked to true"
            self.locked = True

        compare_points = []
        for selreg in view.sel():
            compare_points.append(selreg.begin())

        if compare_points != self.kill_points:
            print "kill_to_clipboard: check_for_cursor_change; points changed, setting locked to true"
            self.locked = True

        if save:
            print "kill_to_clipboard: setting kill points to " + str(self.kill_points)
            print "kill_to_clipboard: setting kill_id to " + str(self.kill_id)
            self.kill_points = compare_points
            self.kill_id = view_id

    def lock(self):
        self.locked = True

    def unlock(self):
        self.locked = False

    def is_locked(self):
        return self.locked

kill_location = KillLocation()


class KillToClipboardCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        kill_location.check_for_cursor_change(self.view, True)

        self.view.run_command("move_to", {"to": "hardeol", "extend": True})
        text_items = []
        for selreg in self.view.sel():
            if selreg.empty():
                selreg = sublime.Region(selreg.a, selreg.a + 1)
            text_items.append(self.view.substr(selreg))
        text = "\n".join(text_items)
        print "kill_to_clipboard: selected text " + str(text)

        if kill_location.is_locked():
            print "kill_to_clipbard: is locked; replacing clipboard contents"
            sublime.set_clipboard(text)
        else:
            print "kill_to_clipbard: is NOT locked; appending to clipboard"
            sublime.set_clipboard(sublime.get_clipboard() + text)

        kill_location.unlock()
        print "kill_to_clipboard: done with clipboard; set to unlock"
        self.view.run_command("right_delete")


class YankFromClipboardCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        print "kill_to_clipboard: locking inside yank"
        kill_location.lock()
        self.view.run_command("paste")


class KillToClipboardEvents(sublime_plugin.EventListener):
    def on_activated(self, view):
        # print "kill_to_clipboard: locking in on_activated event"
        kill_location.lock()

    def on_selection_modified(self, view):
        if not kill_location.is_locked():
            kill_location.check_for_cursor_change(view, False)
