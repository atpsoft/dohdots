#!/usr/bin/env ruby
require "yaml"
require 'FileUtils'

class SublimeFiles
  attr_reader :status
  def initialize
    @user = ENV['USER']
    # @user = 'kem'
    @full_path = File.expand_path("../", __FILE__)
    @default_path = @full_path + '/root'
    @default_file = @default_path + '/Default (OSX).sublime-keymap'
    @user_path = @full_path + '/' + @user
    @user_file = @user_path + '/' + @user + '.sublime-keymap'
    @output_file = @user_path + '/Default (OSX).sublime-keymap'
    @auto_msg = "\n\n/*\n  -=- #{@user}.sublime-keymap -=-\n*/\n\n"
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
    @output_contents = file_to_string(@output_file)

    result = @output_contents.split(@auto_msg)
    if result.size == 2
      @default_contents = result[0]
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

sfiles = SublimeFiles.new()
sfiles.do_that_voodoo
