[user]
  name = Jake Weeks
  email = gitauthor@jakeland.com
[diff]
  tool = opendiff
[difftool "sourcetree"]
  cmd = /Applications/DiffMerge.app/Contents/MacOS/DiffMerge --nosplash \"$LOCAL\" \"$REMOTE\"
  path =
[mergetool "sourcetree"]
  cmd = /Applications/DiffMerge.app/Contents/MacOS/DiffMerge --merge --result=\"$MERGED\" \"$LOCAL\" \"$BASE\" \"$REMOTE\"
  trustExitCode = true
