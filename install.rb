#!/usr/bin/env ruby
require 'date'

def link_file(src, dest, hard_link = false)
  src = get_path(src)
  dest = get_path(dest)
  if is_non_linked_file(dest)
    raise "File already exists: #{dest} -- aborting installation"
  end
  `rm -f "#{dest}"`
  if hard_link
    opts = ''
  else
    opts = ' -s'
  end
  `ln#{opts} "#{src}" "#{dest}"`
  puts "linked #{src} to #{dest}"
end

def safely_remove_dir(path)
  fullpath = get_path(path)
  if File.exist?(fullpath)
    `rmdir "#{fullpath}"`
  end
  if File.exist?(fullpath)
    raise "failed to remove directory #{fullpath}"
  end
end

def copy_file(src, dest)
  src = get_path(src)
  if !File.exist?(src)
    puts "not copying #{src} - file doesn't exist"
    return
  end
  dest = get_path(dest)
  `cp -n "#{src}" "#{dest}"`
  puts "copied #{src} to #{dest}"
end

def merge_files(user, src, dest)
  src = get_path(src)
  dest = get_path(dest)
  lines = ["#generated by dohdots install -- via ruby < <(curl -B https://raw.githubusercontent.com/atpsoft/dohdots/master/install.rb)\n"]
  if is_non_linked_file(dest)
    destbackup = dest + '.backup-by-dohdots-install-' + DateTime.now.strftime('%F:%T')
    puts "moving #{dest} to #{destbackup}"
    `mv #{dest} #{destbackup}`
    lines += ["#previous file exists at: #{destbackup}\n"]
  end
  userfile = src + '.' + user
  lines += File.readlines(src)
  if File.exist?(userfile)
    lines += File.readlines(userfile)
  end
  file = File.new(dest, 'w')
  file << lines.join("")
end

def get_path(file)
  fromhome = file[0] != '/'
  root = fromhome ? ENV['HOME'] : '/'
  return File.join(root, file)
end

def ensure_exists(file)
  path = get_path(file)
  `mkdir -p "#{path}"`
end

def is_non_linked_file(file)
  File.exist?(file) && !File.symlink?(file)
end

def is_mac
  `uname -a` =~ /Darwin/
end

def is_mac_m1
  `uname -a`.downcase.include?('arm64')
end

def is_mac_atleast_bigsur
  return false if !is_mac
  return true if is_mac_m1
  return true if `sw_vers -productVersion`.split('.')[0].to_i >= 11
  return true if `sw_vers -productVersion`.split('.')[1].to_i >= 16
  return false
end

def getuser
  user = ENV['USER']
  usermap = {'kmason' => 'kem', 'mmason' => 'makani', 'tlarson' => 'trent'}
  user = usermap[user] if usermap[user]
  user
end

def get_dohdots
  if (!File.exist?(get_path('src/dohdots')))
    puts "gitting dohdots from github"
    ensure_exists('src')
    puts `git clone https://github.com/atpsoft/dohdots.git ~/src/dohdots`
  else
    puts "updating dohdots from github"
    puts `cd ~/src/dohdots; git pull`
  end
end

SHELL_NAMES = ['', 'bash', 'zsh']
def link_files(input)
  shell_name = input.to_s.strip.downcase
  if !SHELL_NAMES.include?(shell_name)
    puts "please enter a valid shell name: bash or zsh"
    exit
  end
  if shell_name.empty?
    if is_mac_atleast_bigsur
      shell_name = 'zsh'
    else
      shell_name = 'bash'
    end
  end

  puts "linking #{shell_name}"
  user = getuser
  puts "linking dotfiles for user: #{user}"
  if is_mac
    puts "Since we're on a mac, we'll link DefaultKeyBinding.dict"
    ensure_exists('Library/KeyBindings')
    link_file('src/dohdots/mac/DefaultKeyBinding.dict', 'Library/KeyBindings/DefaultKeyBinding.dict', true)
    # ensure_exists('Library/Application Support/Karabiner')
    # link_file('src/dohdots/mac/karabiner.xml', 'Library/Application Support/Karabiner/private.xml')
    # copy_file("src/dohdots/mac/moom_preferences.#{user}.plist", 'Library/Preferences/com.manytricks.Moom.plist')
    link_file('/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl', '/opt/doh/bin/subl')
    link_file('src/dohdots/sublime/root', 'Library/Application Support/Sublime Text 3/Packages/doh')
    safely_remove_dir('Library/Application Support/Sublime Text 3/Packages/User')
    link_file("src/dohdots/sublime/#{user}", 'Library/Application Support/Sublime Text 3/Packages/User')
  end
  link_file('src/dohdots/git/gitignore', '.gitignore')
  if shell_name == 'bash'
    link_file("src/dohdots/bash/bash_profile", ".bash_profile")
  elsif shell_name == 'zsh'
    link_file("src/dohdots/zsh/zprofile", ".zprofile")
  end
  link_file("src/dohdots/#{shell_name}/screenrc", ".screenrc")
  if (File.exist?(get_path("src/dohdots/#{shell_name}/#{shell_name}rc.#{user}")))
    link_file("src/dohdots/#{shell_name}/#{shell_name}rc.#{user}", ".#{shell_name}rc.user")
  end
  link_file("src/dohdots/#{shell_name}/#{shell_name}rc", ".#{shell_name}rc")
  link_file('src/dohdots/emacs/emacs', '.emacs')
  link_file('src/dohdots/emacs/emacs.d', '.emacs.d')
  if (File.exist?(get_path("src/dohdots/emacs/emacs.#{user}")))
    link_file("src/dohdots/emacs/emacs.#{user}", '.emacs.user')
  end
  link_file('src/dohdots/git/gitconfig', '.gitconfig')
  if (File.exist?(get_path("src/dohdots/git/gitconfig.#{user}")))
    link_file("src/dohdots/git/gitconfig.#{user}", '.gitconfig.user')
  end
  # if (File.exist?(get_path("Library/Application Support/Code/User")))
  #   link_file("src/dohdots/vscode/settings.json", 'Library/Application Support/Code/User/settings.json')
  # end
end

get_dohdots
link_files(ARGV[0])
