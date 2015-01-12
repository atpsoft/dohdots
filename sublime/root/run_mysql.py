import sublime, sublime_plugin, sys, os
import subprocess
import time
import threading
sys.path.append(os.path.dirname(os.path.realpath(__file__)))
import pymysql

class AppendText(sublime_plugin.TextCommand):
    def run(self, edit, timestamp, text):
        if timestamp:
            timestr = time.strftime("%Y-%m-%d %H:%M:%S ==> ", time.localtime())
        else:
            timestr = ""
        self.view.insert(edit, self.view.size(), timestr + text + "\n")
        self.view.show(self.view.size())


class AsciiTableBuilder:
    def determine_field_widths(self, rows, headers):
        widths = []
        for column_index in range(0, len(headers)):
            max_width = len(headers[column_index])
            for values in rows:
                value_len = len(values[column_index])
                if value_len > max_width:
                    max_width = value_len
            widths.append(max_width)
        return widths

    def build_padded_headers(self, headers):
        max_header_size = 0
        for curr_header in headers:
            curr_header_size = len(curr_header)
            if curr_header_size > max_header_size:
                max_header_size = curr_header_size

        padded = []
        for curr_header in headers:
            needed = max_header_size - len(curr_header)
            spaces = ' ' * needed
            padded.append(spaces + curr_header)
        return padded

    def build_separator(self, widths):
        str = '+'
        for col_width in widths:
            str += ('-' * (col_width + 2)) + '+'
        return str

    def value_to_string(self, value):
        if value == None:
            return 'NULL'
        return str(value)

    def row_to_line(self, values, widths):
        str = '|'
        for column_index in range(0, len(widths)):
            max_width = widths[column_index]
            value_str = values[column_index]
            value_str = values[column_index]
            value_len = len(value_str)
            pad_len = (max_width - value_len) + 1
            str += (' ' * pad_len) + value_str + ' |'
        return (str + "\n")

    def convert_all_values_to_strings(self, old_rows):
        new_rows = []
        for old_values in old_rows:
            new_values = []
            for one_old in old_values:
                new_values.append(self.value_to_string(one_old))
            new_rows.append(new_values)
        return new_rows

    def build_line_per_row(self, rows, headers):
        rows = self.convert_all_values_to_strings(rows)
        widths = self.determine_field_widths(rows, headers)
        separator = self.build_separator(widths) + "\n"
        retval = ''
        retval += separator
        retval += self.row_to_line(headers, widths)
        retval += separator
        for values in rows:
            retval += self.row_to_line(values, widths)
        retval += separator
        return retval

    def build_line_per_field(self, rows, headers):
        rows = self.convert_all_values_to_strings(rows)
        padded_headers = self.build_padded_headers(headers)
        field_count = len(padded_headers)
        separator = ('*' * 25)
        retval = ''
        row_number = 0
        for values in rows:
            row_number += 1
            retval += separator + ' row ' + str(row_number) + ' ' + separator + "\n"
            for column_index in range(0, field_count):
                retval += padded_headers[column_index] + ": " + values[column_index] + "\n"
        return retval


class QueryRunnerThread(threading.Thread):
    RECONNECT_MYSQL_ERRORS = frozenset([2006, 2013])

    def __init__(self, query_core, stmt, table_builder):
        self.query_core = query_core
        self.stmt = stmt
        self.table_builder = table_builder
        threading.Thread.__init__(self)

    def run(self):
        dbconn = self.query_core.get_connection(False)
        if dbconn == None:
            self.query_core.output_text(False, "unable to connect to database")
            return

        error_code = 0
        error, output = self.run_query_once(dbconn)
        if error != None:
            error_code = error.args[0]
        self.query_core.output_text(False, output)

        if not (error_code in self.RECONNECT_MYSQL_ERRORS):
            return

        dbconn = self.query_core.get_connection(True)
        error, output = self.run_query_once(dbconn)
        self.query_core.output_text(False, output)

    def run_query_once(self, dbconn):
        self.query_core.output_text(True, self.stmt)

        cursor = dbconn.cursor()
        output = ""
        error = None
        try:
            start_time = time.time()
            cursor.execute(self.stmt)
            elapsed_amt = round(time.time() - start_time, 2)
            elapsed_str = str(elapsed_amt) + ' sec'
            if cursor.description == None:
                if self.stmt.lower().find("use") == 0:
                    return (None, 'database changed\n')
                insert_id = cursor.lastrowid
                if insert_id > 0:
                    return (None, 'last insert id: ' + str(insert_id) + ' (' + elapsed_str + ')\n')
                return (None, str(cursor.rowcount) + ' rows affected (' + elapsed_str + ')\n')
            data = cursor.fetchall()
            headers = []
            for header_detail in cursor.description:
                headers.append(header_detail[0])
            row_count = len(data)
            if row_count == 0:
                return (None, "no rows (" + elapsed_str + ")\n")

            if self.stmt[-2] == ';':
                output = self.table_builder.build_line_per_field(data, headers)
            else:
                output = self.table_builder.build_line_per_row(data, headers)
            output += str(row_count) + " rows (" + elapsed_str + ")\n"
        except Exception as excpt:
            error = excpt
            output = str(excpt) + "\n"
        return (error, output)


class QueryCore:
    # note: set, use are allowed as both a read or write
    READ_CMDS = frozenset(['select','use','describe','desc','explain','show','set'])
    WRITE_CMDS = frozenset(['update','delete','insert','replace','use','load','create','alter','truncate','commit','set','drop','rename'])

    def __init__(self, source_view):
        self.source_view = source_view
        self.output_view = None
        self.source_tab_name = None
        self.table_builder = AsciiTableBuilder()
        self.selected_profile = None
        self.connection_params = None
        self.dbconn = None
        self.stmt = None
        self.allow_read_stmts = False
        self.allow_write_stmts = False

    def update_output_view_name(self):
        name = 'mysql (%s): %s' % (self.selected_profile, self.source_tab_name)
        self.output_view.set_name(name)

    def output_text(self, include_timestamp, text):
        self.output_view.run_command("append_text", {'timestamp': include_timestamp, 'text': text})

    def pick_database(self):
        self.ui_connection_list = []
        for connection in sublime.load_settings('doh.sublime-settings').get('connections'):
            self.ui_connection_list.append([connection.get('name'), 'Host: ' + connection.get('host')])
        window = sublime.active_window()
        window.show_quick_panel(self.ui_connection_list, self.database_was_picked)

    def database_was_picked(self, picked):
        if picked < 0:
            self.clear_selected_profile()
            return

        doh_settings = sublime.load_settings('doh.sublime-settings')
        connections_list = doh_settings.get('connections')

        database = self.ui_connection_list[picked][0]
        found_connection = None
        for connection in connections_list:
            if connection.get('name') == database:
                found_connection = connection
        self.set_selected_profile(database, found_connection)
        self.start_query()

    def connect_to_database(self):
        self.dbconn = None
        vals = self.connection_params
        msg = "connecting to %s on %s:%s as %s" % (vals.get('db'), vals.get('host'), vals.get('port'), vals.get('user'))
        self.output_text(True, msg)
        if vals.get('theme'):
            self.source_view.settings().set('color_scheme', vals.get('theme'))
            self.output_view.settings().set('color_scheme', vals.get('theme'))

        try:
            self.dbconn = pymysql.connect(vals.get('host'), vals.get('user'), vals.get('pass'), vals.get('db'), vals.get('port'))
            self.dbconn.cursor().execute('SET autocommit=1,sql_safe_updates=1,sql_select_limit=500,max_join_size=1000000')
        except Exception as excpt:
            self.output_text(True, str(excpt) + "\n")
        return self.dbconn

    def is_query_allowed(self):
        first_word = self.stmt.partition(' ')[0]
        read_ok = (first_word in self.READ_CMDS) and self.allow_read_stmts
        write_ok = (first_word in self.WRITE_CMDS) and self.allow_write_stmts
        if (read_ok or write_ok):
            return True
        if not self.allow_read_stmts:
            self.output_text(True, "unable to execute read statements with that command")
        if not self.allow_write_stmts:
            self.output_text(True, "unable to execute write statements with that command")
        return False

    def start_query(self):
        if not self.is_query_allowed():
            return
        thread = QueryRunnerThread(self, self.stmt, self.table_builder)
        thread.start()

    def save_view(self, view, source_tab_name):
        self.output_view = view
        self.source_tab_name = source_tab_name
        self.selected_profile = self.output_view.settings().get('selected_profile')
        self.connection_params = self.output_view.settings().get('connection_params')
        self.dbconn = None

    def has_output_view(self):
        return (self.output_view != None) and (self.output_view.window() != None)

    def get_connection(self, force_new):
        if (self.dbconn == None) or force_new:
            self.connect_to_database()
        return self.dbconn

    def set_selected_profile(self, database, connection_params):
        self.output_view.settings().set('selected_profile', database)
        self.selected_profile = database
        self.output_view.settings().set('connection_params', connection_params)
        self.connection_params = connection_params
        self.dbconn = None
        self.update_output_view_name()

    def clear_selected_profile(self):
        self.output_view.settings().erase('selected_profile')
        self.selected_profile = None
        self.output_view.settings().erase('connection_params')
        self.connection_params = None
        self.dbconn = None

    def has_selected_profile(self):
        return (self.selected_profile != None)

    def save_stmt(self, stmt, allow_read, allow_write):
        self.stmt = stmt
        self.allow_read_stmts = allow_read
        self.allow_write_stmts = allow_write


class RunMysqlCommand(sublime_plugin.TextCommand):
    SQLSTMT_STARTS = frozenset(['select','update','delete','insert','replace','use','load','describe','desc','explain','create','alter','truncate','show','commit','set','drop','rename'])

    def __init__(self, view):
        super(RunMysqlCommand, self).__init__(view)
        self.query_core = None
        self.view.settings().set('parent_view', True)

    def run(self, edit, **args):
        if self.query_core == None:
            self.query_core = QueryCore(self.view)

        if self.view.settings().get('run_mysql_source_file') != None:
            edit = self.view.begin_edit()
            self.view.insert(edit, self.view.size(), "unable to run queries from an output view (for now)" + "\n")
            self.view.end_edit(edit)
            self.view.show(self.view.size())
            return

        current_file = self.view.file_name()
        self.current_file = current_file
        window = sublime.active_window()

        if current_file == None:
            current_file = "None" + self.view.substr(self.view.line(0))
            file_name = self.view.substr(sublime.Region(0, self.view.size()))
            tab_name = "untitled"
        else:
            file_name = current_file.split("/")[-1]
            tab_name = file_name

        self.tab_name = tab_name
        self.file_name = file_name
        self.window = window

        region = self.view.sel()[0]
        if region.empty():
            stmt = self.find_statement(region)
        else:
            stmt = self.view.substr(region).strip()

        self.ensure_output_view()
        self.tweak_view_settings(self.view)

        if len(stmt) == 0:
            self.query_core.output_text(True, stmt + "\nunable to find statement")
            return

        self.query_core.save_stmt(stmt, args["allow_read"], args["allow_write"])
        if self.query_core.has_selected_profile():
            self.query_core.start_query()
        else:
            self.query_core.pick_database()

    def tweak_view_settings(self, target_view):
        target_view.settings().set("rulers", [])

    def has_sqlstmt_start(self, line):
        if len(line) == 0:
            return False
        if line[0].isspace():
            return False
        search_line = line.lower()
        if search_line == 'commit;':
            return True
        first_word = search_line.partition(' ')[0]
        return first_word in self.SQLSTMT_STARTS

    def find_statement(self, cursor):
        cursor_lreg = self.view.line(cursor.a)

        lreg = cursor_lreg
        while True:
            begin_stmt = lreg.begin()
            line = str(self.view.substr(lreg))
            if self.has_sqlstmt_start(line):
                break
            if begin_stmt == 0:
                return ''
            lreg = self.view.line(begin_stmt - 1)

        max_end = self.view.size()
        lreg = cursor_lreg
        while True:
            end_stmt = lreg.end()
            if end_stmt >= max_end:
                break
            line = self.view.substr(lreg)
            if len(line) == 0 or line[-1] == ';' or line.isspace():
                break
            curr_begin = lreg.begin()
            if (curr_begin > begin_stmt) and self.has_sqlstmt_start(line):
                end_stmt = curr_begin - 1
                break
            lreg = self.view.line(end_stmt + 1)

        return self.view.substr(sublime.Region(begin_stmt, end_stmt)).strip()

    def ensure_output_view(self):
        if self.query_core.has_output_view():
            return
        new_view = None
        for window in sublime.windows():
            for check_view in window.views():
                if check_view.settings().get('run_mysql_source_file') == self.current_file:
                    new_view = check_view
        if new_view == None:
            new_view = self.build_output_view()
        self.query_core.save_view(new_view, self.tab_name)

    def build_output_view(self):
        window = sublime.active_window()
        parent_view = window.active_view()
        new_view = window.new_file()
        new_view.settings().set('run_mysql_source_file', self.current_file)
        new_view.settings().set('word_wrap', False)
        new_view.settings().set("RunInScratch", True)
        self.tweak_view_settings(new_view)
        new_view.set_scratch(True)
        return new_view

class ActivateViews(sublime_plugin.EventListener):
    def __init__(self):
        self.activating = False

    def bring_to_front(self, view, orig):
        if view == None:
            return
        view.window().focus_view(view)
        view.window().focus_view(orig)

    def get_mirror_view(self, view):
        look_at_filename = False
        if view.settings().get('run_mysql_source_file') != None:
            look_for_file = view.settings().get('run_mysql_source_file')
            look_at_filename = True
        else:
            look_for_file = view.file_name()
        for window in sublime.windows():
            for check_view in window.views():
                if look_at_filename:
                    if check_view.file_name() == look_for_file:
                        return check_view
                else:
                    if check_view.settings().get('run_mysql_source_file') == look_for_file:
                        return check_view

    def on_activated(self, view):
        if self.activating:
            return
        self.activating = True
        if view.settings().get('run_mysql_source_file') != None:
            self.bring_to_front(self.get_mirror_view(view), view)
        elif view.settings().get('parent_view') == True:
            self.bring_to_front(self.get_mirror_view(view), view)
        self.activating = False
