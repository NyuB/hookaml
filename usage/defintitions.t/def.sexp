(
    (defn commits (label since) 
        (projection ((describe label) (repo .) (show (format (:message) (commit_range since HEAD)))))
    )

    (apply (commits One HEAD~1))
    (apply (commits Two HEAD~2))
)