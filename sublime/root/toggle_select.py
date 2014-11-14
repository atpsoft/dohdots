import sublime
import sublime_plugin

dohSearching = False


class DohStartSelectCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):
        global dohSearching
        dohSearching = False
        cursor = [s for s in self.view.sel()]
        self.view.add_regions("doh_start", cursor, "doh_start", "dot", sublime.HIDDEN | sublime.PERSISTENT)
        sublime.status_message("toggle select ON")


class DohCancelSelectCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):
        global dohSearching
        dohSearching = False
        doh_start = self.view.get_regions("doh_start")
        if not doh_start:
            end_point = self.view.sel()[0].b
            self.view.sel().clear()
            self.view.sel().add(sublime.Region(end_point, end_point))
            return

        regions = []
        for cursor_region in self.view.sel():
            new_cursor = cursor_region.b
            regions.append(sublime.Region(new_cursor, new_cursor))

        self.view.erase_regions("doh_start")
        self.view.sel().clear()
        for regobj in regions:
            self.view.sel().add(regobj)

        sublime.status_message("toggle select canceled")


class DohCutCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):
        global dohSearching
        dohSearching = False
        self.view.run_command("cut")
        self.view.erase_regions("doh_start")


class DohCopyCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):
        global dohSearching
        dohSearching = False
        self.view.run_command("copy")
        self.view.run_command("doh_cancel_select")


class DohShowFindCommand(sublime_plugin.WindowCommand):
    def run(self, **args):
        global dohSearching
        dohSearching = True
        self.window.run_command("show_panel", {"panel": "find", "reverse": False, "in_selection": False})


class DohShowReverseFindCommand(sublime_plugin.WindowCommand):
    def run(self, **args):
        global dohSearching
        dohSearching = True
        self.window.run_command("show_panel", {"panel": "find", "reverse": True, "in_selection": False})


class DohFindNextCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):
        global dohSearching
        dohSearching = True
        self.view.window().run_command("find_next")


class DohToggleSelectEvents(sublime_plugin.EventListener):
    def __init__(self, *args, **kwargs):
        sublime_plugin.EventListener.__init__(self, *args, **kwargs)

    def on_query_context(self, view, key, operator, operand, match_all):
        if (key == "doh_toggle_select") and (operator == sublime.OP_EQUAL):
            doh_start = view.get_regions("doh_start")
            return (bool(doh_start) == operand)
        else:
            return None

    def on_selection_modified(self, view):
        global dohSearching

        doh_start = view.get_regions("doh_start")
        if not doh_start:
            return

        first_cursor = view.sel()[0]
        if first_cursor.empty() and (first_cursor.begin() != doh_start[0].begin()):
            view.erase_regions("doh_start")
            return

        if not dohSearching:
            return

        # now that we have a searching flag, I think this will never be needed,
        # but keep it around for now, just in case
        # block_select_count = 0
        # for region in view.sel():
        #     if not region.empty():
        #         block_select_count += 1
        #     if block_select_count > 1:
        #         print "multiple block selects, exiting"
        #         return

        num = min(len(doh_start), len(view.sel()))
        regions = []

        for index in xrange(num):
            regions.append(view.sel()[index].cover(doh_start[index]))

        for index in xrange(num, len(view.sel())):
            regions.append(view.sel()[index])

        view.sel().clear()
        for regobj in regions:
            view.sel().add(regobj)

    def on_activated(self, view):
        global dohSearching
        if view.get_regions("doh_start"):
            dohSearching = False
