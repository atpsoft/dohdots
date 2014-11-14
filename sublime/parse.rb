#!/usr/bin/env ruby
require "yaml"
require 'FileUtils'

# I am lazy and you get what I give you... and you will be happy about it!!

class SublimeFiles
  attr_reader :status
  def initialize
    # DEFAULT = repo, USER = account name, OUTPUT =  sublime user default
    @full_path = File.expand_path("../", __FILE__)
    @sublime_path = '/Users/' + ENV['USER'] + '/Library/Application Support/Sublime Text 2/Packages'
    @onn_link_path = @sublime_path + '/onn'
    @default_path = @full_path + '/root'
    @default_file = @default_path + '/Default (OSX).sublime-keymap'
    @user_path = @full_path + '/' + ENV['USER']
    @user_link_path = '/Users/' + ENV['USER'] + '/Library/Application Support/Sublime Text 2/Packages/User'
    @user_file = @user_path + '/' + ENV['USER'] + '.sublime-keymap'
    @output_file = @user_path + '/Default (OSX).sublime-keymap'
    @auto_msg = "\n\n/*\n  -=- #{@user_file} -=-\n*/\n\n"
    @output_contents = nil
    @user_contents = nil
    @default_contents = nil
  end

  def generated?
    # return true if the sublime files where generated
    if !@output_contents.index(@auto_msg)
      return false
    else
      return true
    end
  end

  def changed?
  	if @default_contents == nil or @default_contents == ""
  		return true
  	end
  	if @user_contents == nil or @user_contents == ""
  		return true
  	end
    if @output_contents != @default_contents + @auto_msg + @user_contents
      return true
    end
    return false
  end

  def create_output_file
    # just for testing strange setups
    if @user_contents == nil
      @user_contents = strip_str(file_to_string(@user_file),false)
    end
    if @default_contents == nil
      @default_contents = strip_str(file_to_string(@default_file),true)
    end

    @output_contents = @default_contents + @auto_msg + @user_contents
    string_to_file(@output_contents,@output_file)
  end

  def split_output_file
    result = @output_contents.split(@auto_msg)
    if result.size == 2
      @default_contents = result[0]
      # @default_contents = wrap_str(result[0],true)
      # @user_contents = wrap_str(result[1],false)
      @user_contents = result[1]
    else
      raise "@output_contents.split result.size = #{result.size}"
    end

    if @user_contents == nil
      raise "No @user_contents"
    end
    if @default_contents == nil
      raise "No @default_contents"
    end

    return true
  end

  def wrap_str(str,first)
    if str != nil && str.length > 1
      if first
        if str[0].chr == '['
          return str[0..str.rindex(",")-1] + "\n]\n"
        else
          raise "Unexpected character str[0]=\"#{str[0]}\" "
        end
      else
        if str[-1].chr != '['
          return "[" + str
        else
          raise "Unexpected character str[0]=\"#{str[0]}\" "
        end
      end
    else
      raise "wrap_str on \"#{str}\" "
    end
  end

  def strip_str(str,first)
    if str != nil && str.length > 1
      str = str.strip
      if first
        return str[0..str.rindex(/\}(?!.)/)] + ",\n"
      else
        if str[0].chr == '['
          return str[1..-1]
        else
          raise "Unexpected character str[0]=\"#{str[0]}\" "
        end
        # Is this causing issues?
        # return str[str.index(/\[/)+1..-1]
      end
    else
      raise "strip_str on \"#{str}\" "
    end
  end

  def file_to_string(file_name)
    if file_name != nil && File.exist?(file_name)
      file = File.open(file_name)
      contents = file.read
      file.close
      return contents.strip
    end
    return nil
  end

  def string_to_file(contents,file_name)
    my_output_file = File.new(file_name, "w")
    my_output_file.write(contents)
    my_output_file.close
    return true
  end


  def do_that_voodoo
    if !File.exist?(@onn_link_path)
      # link basic path
      puts "Creating link #{@onn_link_path}"
      FileUtils.ln_s( @default_path, @onn_link_path, :verbose => true)
    end

    if !File.exist?(@user_path)
      # move default sublime user directory as baseline
      puts "Moving Sublime user directory to repository and linking."
      FileUtils.mv(@user_link_path,@user_path, :verbose => true)
      FileUtils.ln_s(@user_path, @user_link_path, :verbose => true)
    end
    if !File.exist?(@user_file) # check again now that we have moved files
      # copy default file as baseline
      puts "Creating #{@user_file}"
      FileUtils.cp(@output_file,@user_file, :verbose => true)
    end
    @user_contents = strip_str(file_to_string(@user_file),false)

    if File.exist?(@default_file)
      @default_contents = strip_str(file_to_string(@default_file),true)
    else
  	  raise "missing the repo file #{@default_file}"
    end

    if File.exist?(@output_file)
      @output_contents = file_to_string(@output_file)
    else
  	  raise "missing the output file #{@output_file}"
    end

    if !self.generated?
      # create @output_file
      puts "Backing up #{@output_file}"
      # save backup before overwrite
      FileUtils.cp(@output_file,@output_file+'.backup', :verbose => true)
      self.create_output_file
    end


    if self.changed?
      # Need to check the dates on the files (mtime) to determine if there are changes to write to output or the other way
      self.split_output_file
      puts "Updating #{@default_file} and #{@user_file}"
      string_to_file(wrap_str(@default_contents,true),@default_file)
      string_to_file(wrap_str(@user_contents,false),@user_file)
    end
  end
end


case ARGV[0]
when "status"  # information on the current setup
when "clean"
  puts "clean build"
when "links"
  puts "create/recreate links only"
else
  sfiles = SublimeFiles.new()
  sfiles.do_that_voodoo

end

