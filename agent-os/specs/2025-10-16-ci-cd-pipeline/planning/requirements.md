# Spec Requirements: CI/CD Pipeline

## Initial Description
Initialize a new spec folder for the CI/CD Pipeline feature.

The user has selected option 1 - they want to proceed with the CI/CD Pipeline feature that's next in the roadmap.

This feature involves enhancing the existing GitHub Actions workflow to include comprehensive linting, testing, and coverage reporting while maintaining the current multi-platform test matrix.

## Requirements Discussion

### First Round Questions

**Q1:** For the CI platform, I assume we should continue using GitHub Actions since you already have `.github/workflows/test.yml`. Is that correct, or would you prefer a different platform like GitLab CI or CircleCI?
**Answer:** GitHub Actions (confirmed)

**Q2:** For linting, I'm thinking we should add these jobs: package-lint (validates package metadata), checkdoc (validates documentation strings), and byte-compilation warnings (catches code issues). Should we configure these to fail-fast (stop on first failure) or continue running all checks?
**Answer:** Continue running all checks (fail-fast: false for linting)

**Q3:** I assume you want coverage reporting integrated. Which tool should we use - undercover.el (Emacs-native) or simplecov-style tools? And should we upload to Coveralls or Codecov?
**Answer:** Use undercover.el for coverage

**Q4:** Looking at your current workflow, you test on Ubuntu, macOS, and Windows with Emacs 29.4, 30.1, and snapshot. Should we keep this matrix, or would you prefer to test on fewer platforms/versions to speed up CI?
**Answer:** Keep the current matrix (all three platforms, all three Emacs versions)

**Q5:** For the "required-checks" job that's currently named in your workflow, should we rename it to something more descriptive like "all-checks-pass" to match the roadmap description?
**Answer:** Yes, rename to `all-checks-pass`

**Q6:** I'm assuming we should keep the existing triggers (push to master, PRs, workflow_dispatch) and concurrency settings. Is that correct?
**Answer:** Yes, keep existing triggers

**Q7:** Are there any features you explicitly DON'T want in this CI/CD pipeline that we should document as out of scope? For example: deployment automation, performance benchmarking, security scanning, automated releases, etc.?
**Answer:** Out of scope items documented in follow-up

### Follow-up Questions

**Follow-up 1:** For coverage reporting with undercover.el, where should we send the results? Should we upload to an external service like Coveralls/Codecov, or display them directly in GitHub somehow?
**Answer:** Don't upload to external service, but show the coverage somehow in GitHub (research options for displaying coverage in GitHub without external services)

**Follow-up 2:** Which coverage tool specifically - undercover.el has been mentioned. Is that the one you want to use?
**Answer:** Yes, undercover.el

**Follow-up 3:** For the linting jobs (package-lint, checkdoc, byte-compilation), what severity level should cause the build to fail? Should we fail on warnings, or only on errors?
**Answer:** Fail on all warnings for all linting jobs

**Follow-up 4:** Do you want any special configuration for the linters (like excluding certain warnings, custom rules), or should we use their default configurations?
**Answer:** Use default configurations

**Follow-up 5:** Should the test matrix remain exactly as is (29.4, 30.1, snapshot on all three platforms)?
**Answer:** Yes, keep current test matrix

### Existing Code to Reference

**Similar Features Identified:**
- Current workflow file: `/home/mathematician314/data/personal/jj.el/.github/workflows/test.yml`
- Existing test setup uses eask for package management and buttercup for testing
- Current aggregation job pattern in `required-checks` can be extended for new linting jobs

No other similar CI/CD features identified - this is the main workflow.

### Visual Assets

**Files Provided:**
No visual assets provided.

**Visual Insights:**
Not applicable - no visual files found.

## GitHub-Native Coverage Display Options

Based on research into GitHub Actions capabilities, there are several options for displaying coverage without external services:

### Option 1: GitHub Actions Job Summary (Recommended)
- Uses `GITHUB_STEP_SUMMARY` environment variable
- Supports GitHub Flavored Markdown
- Displays on the workflow run summary page
- Persistent and easily accessible
- Can include tables, badges, and formatted reports
- Example: Append coverage report to `$GITHUB_STEP_SUMMARY` as markdown table

### Option 2: PR Comments via GitHub API
- Post coverage report as a comment on pull requests
- Requires `GITHUB_TOKEN` permissions
- Uses GitHub API or actions like `peter-evans/create-or-update-comment`
- More visible for PR reviewers
- Can update existing comment to avoid spam

### Option 3: Artifacts with HTML Reports
- Generate HTML coverage report
- Upload as workflow artifact
- Users can download and view locally
- Less convenient but provides detailed reports

### Option 4: Annotations
- Use `::notice` workflow commands for summary metrics
- Example: `echo "::notice::Coverage: 85.3%"`
- Shows in workflow logs and annotations panel
- Less detailed but quick to scan

### Recommendation
Combine Option 1 (Job Summary) with Option 2 (PR Comments) for best visibility:
- Job Summary: Always show coverage on workflow runs
- PR Comments: Post coverage summary on PRs for easy review

## Requirements Summary

### Functional Requirements

**Core Functionality:**
- Enhance existing GitHub Actions workflow with comprehensive CI/CD pipeline
- Add linting jobs: package-lint, checkdoc, byte-compilation warnings
- Integrate coverage reporting using undercover.el
- Display coverage in GitHub without external services
- Rename aggregation job from `required-checks` to `all-checks-pass`
- Maintain existing multi-platform test matrix

**Linting Jobs (all fail on warnings):**
- **package-lint**: Validate Emacs package metadata and structure
  - Use default configuration
  - Fail build on any warnings

- **checkdoc**: Validate documentation strings
  - Use default configuration
  - Fail build on any warnings

- **byte-compilation**: Check for compilation warnings
  - Use default configuration
  - Fail build on any warnings

- Run linting jobs with `fail-fast: false` to see all issues

**Testing:**
- Keep existing multi-platform testing: Ubuntu, macOS, Windows
- Keep existing Emacs version matrix: 29.4, 30.1, snapshot
- Keep `fail-fast: false` in test matrix
- Keep `continue-on-error: true` for snapshot builds
- Continue using eask for package management
- Continue using buttercup for test execution

**Coverage Reporting:**
- Tool: undercover.el
- Display methods:
  - Primary: GitHub Actions Job Summary (markdown formatted)
  - Secondary: PR comments for pull requests
- No external service uploads (no Coveralls, Codecov, etc.)
- Generate coverage metrics after test execution
- Format coverage data as readable markdown tables
- Include overall coverage percentage and per-file breakdown

**Workflow Triggers:**
- Keep existing: push to master, pull_request, workflow_dispatch
- Keep existing concurrency settings (cancel-in-progress: true)

**Aggregation Job:**
- Rename from `required-checks` to `all-checks-pass`
- Should depend on: test job + all linting jobs
- Use jq to verify all dependent jobs succeeded
- Run with `if: ${{ always() }}` to ensure it runs even if some jobs fail

### Reusability Opportunities
- Existing workflow structure in `.github/workflows/test.yml` provides foundation
- Current `required-checks` job pattern can be extended to include linting jobs
- Existing eask setup steps can be reused for linting jobs
- Current test matrix structure should be preserved

### Scope Boundaries

**In Scope:**
- Adding linting jobs (package-lint, checkdoc, byte-compilation)
- Integrating undercover.el for coverage
- Displaying coverage in GitHub Actions summaries and PR comments
- Renaming aggregation job to `all-checks-pass`
- Updating aggregation job to depend on all new linting jobs
- Ensuring all linting jobs fail on warnings
- Maintaining existing test matrix and platform coverage

**Out of Scope:**
- Caching (not requested for this iteration)
- Deployment automation
- Quality gates beyond pass/fail
- Documentation building/publishing
- Automated releases or changelog generation
- Performance benchmarking
- Security scanning tools
- External coverage service integration (Coveralls, Codecov)
- Changes to existing test matrix or platform coverage
- Custom linter configurations or rule adjustments

**Future Enhancements (mentioned but deferred):**
- These items were explicitly excluded from current scope

### Technical Considerations

**Coverage Implementation:**
- undercover.el will need to be added as a development dependency
- Coverage data collection should run during test execution
- Coverage report generation should happen after tests complete
- Markdown formatting required for GitHub Actions summary
- PR comment action will need `GITHUB_TOKEN` with write permissions
- Consider coverage threshold (e.g., warn if below 80%) in future iterations

**Linting Job Implementation:**
- Each linting job should run on ubuntu-latest (no need for multi-platform)
- All linters should use recent/stable Emacs version (30.1 recommended)
- Jobs should run in parallel with tests for faster feedback
- Use eask commands for each linter where available
- Configure to exit with non-zero code on warnings

**Workflow Structure:**
- Linting jobs can run in parallel with test job
- `all-checks-pass` job runs last, depends on all jobs
- Total job count: 1 test job (matrix of 9) + 3 linting jobs + 1 aggregation = 13 total jobs
- Consider job ordering for optimal feedback time

**Emacs Version Consistency:**
- Current workflow tests on 29.4, 30.1, snapshot
- Tech-stack documentation should be updated to reflect minimum version 29.1
- Linting can run on single stable version (30.1) since Emacs Lisp is generally backward compatible

**GitHub Actions Features to Use:**
- `GITHUB_STEP_SUMMARY` for job summaries
- `GITHUB_TOKEN` for PR comments
- Workflow artifacts for detailed HTML reports (optional)
- Job annotations for quick metrics display

**Dependencies:**
- undercover.el (coverage tool)
- package-lint (linting tool)
- checkdoc (built into Emacs)
- Existing: eask, buttercup, jcs090218/setup-emacs, emacs-eask/setup-eask

**Error Handling:**
- Coverage job should not fail if coverage collection fails (post coverage as 0% or N/A)
- PR comment should handle cases where PR context is not available
- Aggregation job should correctly handle experimental job failures

**Documentation Updates Needed:**
- Update tech-stack.md to document CI/CD pipeline tools
- Document coverage integration approach
- Note minimum Emacs version (29.1) in tech-stack

### Recommendations

1. **Phased Rollout**: Consider implementing in phases:
   - Phase 1: Add linting jobs
   - Phase 2: Add coverage reporting
   - Phase 3: Optimize and refine

2. **Coverage Threshold**: While not required now, consider adding coverage threshold checks in future (e.g., fail if coverage drops below X%)

3. **Badge**: Consider adding coverage badge to README once coverage is stable

4. **Notification**: Keep GitHub's default notification behavior - no custom notifications needed

5. **Performance**: All linting jobs on single platform should complete quickly; test matrix remains the bottleneck (expected)

6. **Maintainability**: Use GitHub Actions marketplace actions where possible to reduce custom scripting

## Notes

- User prefers single-line commit messages without Claude mentions
- Commit extended messages should include "related to: [gitlab issue URL]"
- All script shebangs should use `/usr/bin/env` not absolute paths
- Current branch: basic-tests
- Main branch for PRs: master
