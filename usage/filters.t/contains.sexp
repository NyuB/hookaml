(
    (projection ((describe "Last 3 commits") (repo .)
        (show (filter (contains fixup) (format ((:author (s |) :message) (commit_range HEAD~3 HEAD)))))))
)