import sublime
import sublime_plugin

onnSearching = False


class OnnStartSelectCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):
        global onnSearching
        onnSearching = False
        cursor = [s for s in self.view.sel()]
        self.view.add_regions("onn_start", cursor, "onn_start", "dot", sublime.HIDDEN | sublime.PERSISTENT)
        sublime.status_message("toggle select ON")


class OnnCancelSelectCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):
        global onnSearching
        onnSearching = False
        onn_start = self.view.get_regions("onn_start")
        if not onn_start:
            end_point = self.view.sel()[0].b
            self.view.sel().clear()
            self.view.sel().add(sublime.Region(end_point, end_point))
            return

        regions = []
        for cursor_region in self.view.sel():
            new_cursor = cursor_region.b
            regions.append(sublime.Region(new_cursor, new_cursor))

        self.view.erase_regions("onn_start")
        self.view.sel().clear()
        for regobj in regions:
            self.view.sel().add(regobj)

        sublime.status_message("toggle select canceled")


class OnnCutCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):
        global onnSearching
        onnSearching = False
        self.view.run_command("cut")
        self.view.erase_regions("onn_start")


class OnnCopyCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):
        global onnSearching
        onnSearching = False
        self.view.run_command("copy")
        self.view.run_command("onn_cancel_select")


class OnnShowFindCommand(sublime_plugin.WindowCommand):
    def run(self, **args):
        global onnSearching
        onnSearching = True
        self.window.run_command("show_panel", {"panel": "find", "reverse": False, "in_selection": False})


class OnnShowReverseFindCommand(sublime_plugin.WindowCommand):
    def run(self, **args):
        global onnSearching
        onnSearching = True
        self.window.run_command("show_panel", {"panel": "find", "reverse": True, "in_selection": False})


class OnnFindNextCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):
        global onnSearching
        onnSearching = True
        self.view.window().run_command("find_next")


class OnnToggleSelectEvents(sublime_plugin.EventListener):
    def __init__(self, *args, **kwargs):
        sublime_plugin.EventListener.__init__(self, *args, **kwargs)

    def on_query_context(self, view, key, operator, operand, match_all):
        if (key == "onn_toggle_select") and (operator == sublime.OP_EQUAL):
            onn_start = view.get_regions("onn_start")
            return (bool(onn_start) == operand)
        else:
            return None

    def on_selection_modified(self, view):
        global onnSearching

        onn_start = view.get_regions("onn_start")
        if not onn_start:
            return

        first_cursor = view.sel()[0]
        if first_cursor.empty() and (first_cursor.begin() != onn_start[0].begin()):
            view.erase_regions("onn_start")
            return

        if not onnSearching:
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

        num = min(len(onn_start), len(view.sel()))
        regions = []

        for index in xrange(num):
            regions.append(view.sel()[index].cover(onn_start[index]))

        for index in xrange(num, len(view.sel())):
            regions.append(view.sel()[index])

        view.sel().clear()
        for regobj in regions:
            view.sel().add(regobj)

    def on_activated(self, view):
        global onnSearching
        if view.get_regions("onn_start"):
            onnSearching = False
