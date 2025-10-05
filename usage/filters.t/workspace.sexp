(
    (projection ((describe "Changed '.md' files in worktree") (repo .)
        (show (filter (ends_with .md) worktree))))
    (projection ((describe "Changed files in worktree starting with RE") (repo .)
        (show (filter (starts_with RE) worktree))))
)
