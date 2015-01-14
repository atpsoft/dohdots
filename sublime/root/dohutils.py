import sublime
# use like this:
# import dohutils
# dohutils.print_methods(view)
# note that changes to this file appear to not be immediately reflected, even though it says it is reloading the plugin
# the only way I could see to reflect the changes was quitting sublime

def get_methods(object):
    return [method for method in dir(object) if callable(getattr(object, method))]

def print_methods(object):
    print("methods: " + str(get_methods(object)))

def show_console(active=True):
    if not active:
        hide_console()
    else:
        window = sublime.active_window()
        sublime.active_window().run_command("show_panel", {"panel": "console"})
        # keep the focus where it was before showing the panel
        window.focus_group(window.active_group())

def hide_console():
    sublime.active_window().run_command("hide_panel", {"panel": "console"})

def console(str):
    print(str)
    show_console()
