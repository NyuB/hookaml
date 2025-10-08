(
    (projection ((describe "Changed '.md' files in worktree") (repo .)
        (show (filter (on (:file (ends_with .md))) worktree))))
)