[user]
  name = Jake Weeks
  email = gitauthor@jakeland.com
[diff]
  tool = diffmerge
[difftool "diffmerge"]
  cmd = diffmerge \"$LOCAL\" \"$REMOTE\"
[difftool "sourcetree"]
  cmd = /Applications/DiffMerge.app/Contents/MacOS/DiffMerge --nosplash \"$LOCAL\" \"$REMOTE\"
  path =
[mergetool "sourcetree"]
  cmd = /Applications/DiffMerge.app/Contents/MacOS/DiffMerge --merge --result=\"$MERGED\" \"$LOCAL\" \"$BASE\" \"$REMOTE\"
  trustExitCode = true
