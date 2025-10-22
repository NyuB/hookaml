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
  $ alias escape_hashes="sed -E 's/(\"[0-9a-f]+)|(:hash [0-9a-f]+)/<hash>/'" # Escape hashes for test stability
  $ show_workspace def.sexp | escape_hashes
  Commits since HEAD~1:
  ("#2 Commit")
  
  Commits since HEAD~2:
  ("#2 Commit" "#1 Commit")
