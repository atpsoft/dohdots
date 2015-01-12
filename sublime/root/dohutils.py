
# use like this:
# import dohutils
# dohutils.print_methods(view)
# note that changes to this file appear to not be immediately reflected, even though it says it is reloading the plugin
# the only way I could see to reflect the changes was quitting sublime

def get_methods(object):
    return [method for method in dir(object) if callable(getattr(object, method))]

def print_methods(object):
    print("methods: " + str(get_methods(object)))
