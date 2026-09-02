# Bot Review Workflow

Purpose: run a headless review partner on a pull/merge request, post findings, and
iterate until the review is clean or human judgment is needed.

Prerequisites:

1. Work is on a feature branch.
2. A pull/merge request exists for the branch.
3. Changes are committed and pushed.
4. The project quality gate passes.
5. The selected review partner is configured in the shared review bot config.

Loop:

1. Invoke the shared review script with pull request number, reviewer name,
   review round, and optional model.
2. Read the posted review output.
3. For each finding, decide whether it is valid and in scope.
4. Fix valid in-scope findings.
5. Run the quality gate.
6. Commit fixes with an issue reference and push.
7. Post a fix note that summarizes addressed findings.
8. Start the next review round.

Stop after a clean review or when repeated rounds require human guidance.
