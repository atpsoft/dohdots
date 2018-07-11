import sublime, sublime_plugin, sys, os
import subprocess
import time
import threading
sys.path.append(os.path.dirname(os.path.realpath(__file__)))
import pymysql
import sqlite3
import traceback

sourceViewToCoreRegistry = {}

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
        if value is None:
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
    RECONNECT_MYSQL_ERRORS = frozenset([2003,2006, 2013])

    def __init__(self, query_core, connection_name, stmt_list, table_builder):
        self.query_core = query_core
        self.connection_name = connection_name
        self.stmt_list = stmt_list
        self.table_builder = table_builder
        threading.Thread.__init__(self)

    def run(self):
        try:
            for stmt in self.stmt_list:
                if not self.run_one_query(stmt):
                    return
        except Exception as excpt:
            self.query_core.output_text(False, "unexpected error: " + str(excpt) + "\n")
            raise

    def log_query(self, stmt, output):
        self.query_core.log_text(self.connection_name, True, stmt)
        self.query_core.log_text(self.connection_name, False, output)

    def run_one_query(self, stmt):
        dbconn = self.query_core.get_connection(self.connection_name, False)
        if dbconn is None:
            self.query_core.output_text(False, "unable to connect to database")
            return False

        mysql_error_code = 0
        error, output = self.try_query_once(dbconn, stmt)
        if error is not None:
            mysql_error_code = error.args[0]
        self.query_core.output_text(False, output)
        if error is None:
            self.log_query(stmt, output)

        if not (mysql_error_code in self.RECONNECT_MYSQL_ERRORS):
            return (error is None)

        dbconn = self.query_core.get_connection(self.connection_name, True)
        if not dbconn:
            self.query_core.output_text(False, "unable to connect to database")
            return False

        error, output = self.try_query_once(dbconn, stmt)
        self.query_core.output_text(False, output)
        if error is None:
            self.log_query(stmt, output)
        return True

    def try_query_once(self, dbconn, stmt):
        msg = "(%s)\n%s" %  (self.connection_name, stmt)
        self.query_core.output_text(True, msg)

        cursor = dbconn.cursor()
        output = ""
        error = None
        try:
            start_time = time.time()
            cursor.execute(stmt)
            if ('sqlite' in self.connection_name):
                dbconn.commit()
            elapsed_amt = round(time.time() - start_time, 2)
            elapsed_str = str(elapsed_amt) + ' sec'
            if cursor.description is None:
                if stmt.lower().find("use") == 0:
                    return (None, 'database changed\n')
                insert_id = cursor.lastrowid
                # when lastrowid does not apply (not an insert statement), None is returned from sqlite, but zero from mysql
                if insert_id and insert_id > 0:
                    return (None, 'last insert id: ' + str(insert_id) + ' (' + elapsed_str + ')\n')
                return (None, str(cursor.rowcount) + ' rows affected (' + elapsed_str + ')\n')
            data = cursor.fetchall()
            headers = []
            for header_detail in cursor.description:
                headers.append(header_detail[0])
            row_count = len(data)
            if row_count == 0:
                return (None, "no rows (" + elapsed_str + ")\n")

            if stmt[-2] == ';':
                output = self.table_builder.build_line_per_field(data, headers)
            else:
                output = self.table_builder.build_line_per_row(data, headers)
            output += str(row_count) + " rows (" + elapsed_str + ")\n"
        except Exception as excpt:
            error = excpt
            output = str(excpt) + "\n"
        return (error, output)


class QueryCore:
    READ_CMDS = frozenset(['select','describe','desc','explain','show'])
    WRITE_CMDS = frozenset(['update','delete','insert','replace','load','create','alter','truncate','commit','drop','rename','flush','analyze','kill','optimize','start','commit','rollback','grant'])
    NEUTRAL_CMDS = frozenset(['use','set'])

    def __init__(self, source_view):
        self.source_view = source_view
        self.output_view = None
        self.source_tab_name = None
        self.table_builder = AsciiTableBuilder()
        self.selected_profile = None
        self.profile_config = None
        # connections and logfiles are both keyed off connection_name
        self.connections = {}
        self.logfiles = {}
        self.stmt_list = []
        self.allow_read_stmts = False
        self.allow_write_stmts = False
        self.specific_connection_name = None
        self.all_settings = sublime.load_settings('doh.sublime-settings')

    def update_output_view_name(self):
        name = '%s: %s' % (self.source_tab_name, self.selected_profile)
        self.output_view.set_name(name)

    def output_text(self, include_timestamp, text):
        self.output_view.run_command("append_text", {'timestamp': include_timestamp, 'text': text})

    def open_logfile(self, conn_config):
        logdir = self.all_settings.get("logfile_dir")
        if logdir is None:
            self.output_text(True, "no logfile_dir setting, logging disabled")
            return

        logname = conn_config.get('logfile')
        if logname is None:
            self.output_text(True, "no logfile set for this connection, logging disabled")
            return

        logpath = os.path.join(logdir, logname)
        self.output_text(True, "logging queries to %s" % (logpath))
        return open(logpath, 'a')

    def log_text(self, connection_name, include_timestamp, text):
        logfile = self.logfiles.get(connection_name)
        if logfile is None:
            return

        if include_timestamp:
            timestr = time.strftime("%Y-%m-%d %H:%M:%S  ", time.localtime())
            logfile.write(timestr)
        logfile.write(text + "\n")
        logfile.flush()

    def pick_profile(self):
        self.ui_profile_list = []
        for profile in self.all_settings.get('profiles'):
            self.ui_profile_list.append([profile.get('name')])
        window = sublime.active_window()
        window.show_quick_panel(self.ui_profile_list, self.profile_was_picked)

    def profile_was_picked(self, picked):
        if picked < 0:
            self.clear_selected_profile()
            return

        profile_name = self.ui_profile_list[picked][0]
        self.set_selected_profile(profile_name)
        if len(self.stmt_list) > 0:
            self.start_queries()

    def lookup_connection_params(self, connection_name):
        connections_list = self.all_settings.get('connections')

        found_connection = None
        for connection_config in connections_list:
            if connection_config.get('name') == connection_name:
                found_connection = connection_config
        return found_connection

    def connect_to_database(self, connection_name):
        self.connections[connection_name] = None
        retval = None

        conn_config = self.lookup_connection_params(connection_name)
        if not conn_config:
            self.output_text(True, "unable to find settings for connection " + connection_name)
            return None

        dbtype = self.profile_config.get('dbtype')
        if dbtype == 'mysql':
            msg = "using '%s' connection settings" % (connection_name)
            self.output_text(True, msg)

        vars_cmd = 'SET autocommit=1'
        extra_vars = conn_config.get('server_variables')
        if extra_vars:
            vars_cmd = vars_cmd + ", " + extra_vars
        vars_msg = "(%s) %s" %  (connection_name, vars_cmd)

        retval = self.try_connect_once(conn_config, vars_msg, vars_cmd)
        self.connections[connection_name] = retval

        if retval:
            self.logfiles[connection_name] = self.open_logfile(conn_config)
            return retval

        if dbtype == 'sqlite':
            return None

        retry_script = self.all_settings.get('connection_retry_script')
        if not retry_script:
            return retval

        self.output_text(True, "connection failed, will execute connection_retry_script once, then try the connection again up to 3 times")
        try:
            subprocess.check_call(["sh", retry_script], env=os.environ.copy())
        except Exception as excpt:
            self.output_text(True, str(excpt))
            return None

        for index in range(0, 3):
            time.sleep(20)
            retval = self.try_connect_once(conn_config, vars_msg, vars_cmd)
            if retval:
                break

        self.connections[connection_name] = retval
        if retval:
            self.logfiles[connection_name] = self.open_logfile(conn_config)
        return retval

    def try_connect_once(self, conn_config, vars_msg, vars_cmd):
        retval = None
        try:
            dbtype = self.profile_config.get('dbtype')
            if dbtype == 'sqlite':
                sqlite_path = self.source_view.settings().get('sqlite_path')
                if sqlite_path is None:
                    sqlite_path = conn_config.get('path')
                if sqlite_path is None:
                    self.output_text(True, "no sqlite path found in source file or settings")
                    return None
                retval = sqlite3.connect(sqlite_path)
            elif dbtype == 'mysql':
                retval = pymysql.connect(conn_config.get('host'), conn_config.get('user'), conn_config.get('pass'), conn_config.get('db'), conn_config.get('port'))
                cursor = retval.cursor()
                self.output_text(True, vars_msg)
                cursor.execute(vars_cmd)
            elif dbtype is None:
                self.output_text(True, "no dbtype set in profile")
                return None
            else:
                self.output_text(True, "unknown dbtype " + dbtype)
                return None
        except Exception as excpt:
            traceback.print_exc()
            retval = None
            self.output_text(True, str(excpt))
        return retval

    def check_statement_type(self, stmt):
        first_word = stmt.partition(' ')[0].lower().strip()
        if first_word in self.NEUTRAL_CMDS:
            return 'neutral'
        elif first_word in self.READ_CMDS:
            return 'read'
        elif first_word in self.WRITE_CMDS:
            return 'write'
        else:
            sublime.error_message("unrecognized statement type: " + first_word)
            raise Exception("unrecognized statement type")

    def is_query_allowed(self, stmt_type):
        if stmt_type == 'neutral':
            return True

        if (stmt_type == 'read') and self.allow_read_stmts:
            return True

        if (stmt_type == 'write') and self.allow_write_stmts:
            return True

        if not self.allow_read_stmts:
            sublime.error_message("unable to execute read statements with that command")
        elif not self.allow_write_stmts:
            sublime.error_message("unable to execute write statements with that command")
        else:
            sublime.error_message("there is a bug in is_query_allowed")
        return False

    def get_connection_name(self, stmt_type):
        if self.specific_connection_name:
            return self.specific_connection_name

        retval = None
        if self.allow_read_stmts and ((stmt_type == 'neutral') or (stmt_type == 'read')):
            retval = self.profile_config.get('read_connection')
            if retval:
                return retval
        elif self.allow_write_stmts and ((stmt_type == 'neutral') or (stmt_type == 'write')):
            retval = self.profile_config.get('write_connection')
            if retval:
                return retval
        retval = self.profile_config.get('default_connection')
        if retval:
            return retval
        return self.profile_config.get('connection')

    def start_queries(self):
        broadest_type_needed = 'neutral'
        for stmt in self.stmt_list:
            stmt_type = self.check_statement_type(stmt)
            if not self.is_query_allowed(stmt_type):
                return
            if stmt_type == 'write':
                broadest_type_needed = 'write'
            elif (stmt_type == 'read') and (broadest_type_needed == 'neutral'):
                broadest_type_needed = 'read'

        connection_name = self.get_connection_name(broadest_type_needed)
        if not connection_name:
            sublime.error_message("There is no " + broadest_type_needed + " connection defined for the " + self.profile_config.get('name') + " profile, please add one or use a different profile.")
            return
        thread = QueryRunnerThread(self, connection_name, self.stmt_list, self.table_builder)
        thread.start()

    def save_output_view(self, view, source_tab_name):
        self.output_view = view
        self.source_tab_name = source_tab_name

    def has_output_view(self):
        return (self.output_view is not None) and (self.output_view.window() is not None)

    def get_connection(self, connection_name, force_new):
        retval = self.connections.get(connection_name)
        if self.profile_config.get('dbtype') == 'sqlite':
            force_new = True
        if force_new or (not retval):
            retval = self.connect_to_database(connection_name)
        return retval

    def set_selected_profile(self, profile_name):
        if profile_name is None:
            return

        found_profile = None
        profiles_list = self.all_settings.get('profiles')
        for profile in profiles_list:
            if profile.get('name') == profile_name:
                found_profile = profile
        if found_profile is None:
            self.output_text(True, "Unable to locate profile named " + profile_name)
            return

        self.source_view.settings().set('selected_profile', profile_name)
        self.selected_profile = profile_name
        self.profile_config = found_profile
        self.connections = {}
        self.update_output_view_name()
        theme = self.profile_config.get('theme')
        if theme:
            self.source_view.settings().set('color_scheme', theme)
            self.output_view.settings().set('color_scheme', theme)
        else:
            self.source_view.settings().erase('color_scheme')
            self.output_view.settings().erase('color_scheme')

    def clear_selected_profile(self):
        self.source_view.settings().erase('selected_profile')
        self.source_view.settings().erase('color_scheme')

        self.selected_profile = None
        self.profile_config = None
        self.connections = {}

        if self.output_view is not None:
            name = '%s: <no profile>' % (self.source_tab_name)
            self.output_view.set_name(name)
            self.output_view.settings().erase('color_scheme')

    def has_active_profile(self):
        return (self.profile_config is not None)

    def save_stmt_list(self, stmt_list, allow_read, allow_write, specific_connection_name):
        self.stmt_list = stmt_list
        self.allow_read_stmts = allow_read
        self.allow_write_stmts = allow_write
        self.specific_connection_name = specific_connection_name

    def reset_stmt_list(self):
        self.stmt_list = []
        self.allow_read_stmts = False
        self.allow_write_stmts = False
        self.specific_connection_name = None


def get_query_core(view):
    idkey = view.id()
    retval = sourceViewToCoreRegistry.get(idkey)
    if not retval:
        retval = QueryCore(view)
        sourceViewToCoreRegistry[idkey] = retval
    return retval

def get_sqlite_path(view):
    cursor = view.sel()[0]
    lreg = view.line(cursor.a)
    line = view.substr(lreg)
    return line[2:].strip()

class DohmysqlChangeProfileCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):
        query_core = get_query_core(self.view)
        query_core.reset_stmt_list()
        query_core.clear_selected_profile()
        query_core.pick_profile()


class DohsqlChangePathCommand(sublime_plugin.TextCommand):
    def run(self, edit, **args):
        sqlite_path = get_sqlite_path(self.view)
        self.view.settings().set('sqlite_path', sqlite_path)


class DohmysqlQueryCommand(sublime_plugin.TextCommand):
    SQLSTMT_STARTS = frozenset(['select','update','delete','insert','replace','use','load','describe','desc','explain','create','alter','truncate','show','commit','set','drop','rename','flush','analyze','kill','optimize','start','commit','rollback','grant'])

    def __init__(self, view):
        super(DohmysqlQueryCommand, self).__init__(view)
        self.query_core = None
        self.view.settings().set('parent_view', True)

    def run(self, edit, **args):
        if not self.query_core:
            self.query_core = get_query_core(self.view)

        if self.view.settings().get('run_mysql_source_file') is not None:
            sublime.error_message("unable to run queries from an output view (for now)")
            return

        self.current_file = self.view.file_name()
        if not self.current_file:
            sublime.error_message("please save this file before trying to run mysql commands from it")
            return

        self.file_name = self.current_file.split("/")[-1]
        self.tab_name = self.file_name
        self.window = sublime.active_window()

        region = self.view.sel()[0]
        if region.empty():
            stmt_list = [self.find_statement(region)]
        else:
            stmt_list = self.split_into_statements(self.view.substr(region))

        self.ensure_output_view()
        self.tweak_view_settings(self.view)

        if len(stmt_list) == 0:
            self.query_core.output_text(True, "unable to find statement")
            return

        self.query_core.save_stmt_list(stmt_list, args["allow_read"], args["allow_write"], args.get("connection"))

        # first try to set the active profile to a saved one in the view;
        # will fail silently if none has been selected or if the one selected no longer exists
        if not self.query_core.has_active_profile():
            self.query_core.set_selected_profile(self.view.settings().get('selected_profile'))

        # now, if there's an active profile, run the queries, otherwise need to pick a profile
        if self.query_core.has_active_profile():
            self.query_core.start_queries()
        else:
            self.query_core.pick_profile()

    def split_into_statements(self, text):
        if len(text) == 0:
            return []
        not_empty = lambda str: len(str.strip()) > 0
        return list(filter(not_empty, (text.strip() + "\n").split(";\n")))

    def tweak_view_settings(self, target_view):
        target_view.settings().set("rulers", [])
        target_view.settings().set("spell_check", False)

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
        if new_view is None:
            new_view = self.build_output_view()
        self.query_core.save_output_view(new_view, self.tab_name)

    def build_output_view(self):
        window = sublime.active_window()
        parent_view = window.active_view()
        new_view = window.new_file()
        new_view.settings().set('run_mysql_source_file', self.current_file)
        new_view.settings().set('word_wrap', False)
        new_view.settings().set('gutter', False)
        new_view.settings().set("RunInScratch", True)
        self.tweak_view_settings(new_view)
        new_view.set_scratch(True)
        return new_view

class ActivateViews(sublime_plugin.EventListener):
    def __init__(self):
        self.activating = False

    def bring_to_front(self, view, orig):
        if view is None:
            return
        view.window().focus_view(view)
        view.window().focus_view(orig)

    def get_mirror_view(self, view):
        look_at_filename = False
        if view.settings().get('run_mysql_source_file') is not None:
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
        if view.settings().get('run_mysql_source_file') is not None:
            self.bring_to_front(self.get_mirror_view(view), view)
        elif view.settings().get('parent_view') == True and view.file_name() is not None:
            self.bring_to_front(self.get_mirror_view(view), view)
        self.activating = False
