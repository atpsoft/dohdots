[user]
  name = Jake Weeks
  email = gitauthor@jakeland.com
[diff]
  tool = Kaleidoscope
[difftool "diffmerge"]
  cmd = diffmerge \"$LOCAL\" \"$REMOTE\"
[difftool "sourcetree"]
  cmd = opendiff \"$LOCAL\" \"$REMOTE\"
  path =
[mergetool "sourcetree"]
  cmd = /Applications/SourceTree.app/Contents/Resources/opendiff-w.sh \"$LOCAL\" \"$REMOTE\" -ancestor \"$BASE\" -merge \"$MERGED\"
  trustExitCode = true
[difftool "Kaleidoscope"]
  cmd = ksdiff --partial-changeset --relative-path \"$MERGED\" -- \"$LOCAL\" \"$REMOTE\"
[mergetool "Kaleidoscope"]
  cmd = ksdiff --merge --output \"$MERGED\" --base \"$BASE\" -- \"$LOCAL\" --snapshot \"$REMOTE\" --snapshot
  trustExitCode = true
[mergetool]
  prompt = false
[merge]
  tool = Kaleidoscope
[branch]
  autosetuprebase = always
