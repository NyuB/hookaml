# Aliases
  $ show_workspace() { gitst "$1" workspace.out; cat workspace.out; }
# Setup
  $ echo "workspace.*" > .gitignore
  $ git init
  Initialized empty Git repository in $TESTCASE_ROOT/.git/
  $ git config user.name "Brice Decaestecker"
  $ touch workspace.out
# Some commits
  $ git commit --allow-empty -m "Root" > /dev/null
  $ git commit --allow-empty -m "#1 Commit" > /dev/null
  $ git commit --allow-empty -m "#2 Commit" > /dev/null
# Branch out
  $ git checkout -b my_branch
  Switched to a new branch 'my_branch'
  $ git commit --allow-empty -m "#3 Commit on my branch" > /dev/null
  $ alias escape_hashes="sed -E 's/(\"[0-9a-f]+)|(:hash [0-9a-f]+)/<hash>/'" # Escape hashes for test stability
  $ show_workspace last_commits.sexp | escape_hashes
  Last 3 commits:
  (((<hash>) (:author "Brice Decaestecker")
    (:message "#3 Commit on my branch"))
   ((<hash>) (:author "Brice Decaestecker") (:message "#2 Commit"))
   ((<hash>) (:author "Brice Decaestecker") (:message "#1 Commit")))
  
  Commits since reference:
  (<hash> | #3 Commit on my branch | Brice Decaestecker")
