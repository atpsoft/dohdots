import sublime
# use like this:
# import dohutils
# dohutils.print_methods(view)
# note that changes to this file appear to not be immediately reflected, even though it says it is reloading the plugin
# the only way I could see to reflect the changes was quitting sublime
if dohDebugging == None:
    dohDebugging = False

def get_methods(object):
    return [method for method in dir(object) if callable(getattr(object, method))]

def print_methods(object):
    print("methods: " + str(get_methods(object)))

def show_console():
    window = sublime.active_window()
    sublime.active_window().run_command("show_panel", {"panel": "console"})
    # keep the focus where it was before showing the panel
    window.focus_group(window.active_group())

def hide_console():
    sublime.active_window().run_command("hide_panel", {"panel": "console"})


def toggle_debug_mode():
    global dohDebugging
    if dohDebugging == False:
        dohDebugging = True
    elif dohDebugging == True:
        dohDebugging = False

    if dohDebugging:
        sublime.log_commands(True)
        show_console()
    else:
        sublime.log_commands(False)
        hide_console()

def show_console_if_debugging():
    if dohDebugging == True:
        show_console()

def console(str):
    print(str)
    show_console()
