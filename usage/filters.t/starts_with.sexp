(
    (projection ((describe "Changed files in worktree starting with RE") (repo .)
        (show (filter (on (file (starts_with RE))) worktree))))
)
