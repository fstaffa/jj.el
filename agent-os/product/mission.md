# Product Mission

## Pitch
jj.el is a Magit-like Emacs interface for Jujutsu version control system that helps Emacs users leverage Jujutsu's powerful stacked changesets workflow by providing a familiar, production-quality interface that makes complex version control operations intuitive and efficient.

## Users

### Primary Customers
- **Emacs Power Users**: Developers who live in Emacs and want seamless VCS integration without leaving their editor
- **Jujutsu Adopters**: Teams and individuals adopting Jujutsu who need a mature interface comparable to Magit for Git
- **Version Control Enthusiasts**: Developers interested in modern VCS workflows who value both learning and production tooling

### User Personas

**Senior Developer** (30-50 years)
- **Role:** Staff/Principal Engineer at tech companies
- **Context:** Heavy Emacs user with years of Magit muscle memory, exploring Jujutsu for better change management
- **Pain Points:** Existing Jujutsu Emacs packages lack feature completeness; CLI context-switching breaks flow; want to experiment with stacked changesets but need familiar UX
- **Goals:** Maintain productivity during Jujutsu adoption; leverage stacked changesets without sacrificing the polished Magit experience

**Open Source Maintainer** (25-40 years)
- **Role:** Individual contributor or small team lead
- **Context:** Managing multiple feature branches and patches across projects
- **Pain Points:** Git's branch management is cumbersome for stacked changes; need better tools for managing dependent commits
- **Goals:** Streamline contribution workflow; easily rebase and reorganize changesets; maintain clean commit history

**Learning Developer** (20-35 years)
- **Role:** Mid-level developer improving workflows
- **Context:** Comfortable with Emacs, curious about Jujutsu's innovative approach to version control
- **Pain Points:** Jujutsu CLI has learning curve; want guided discovery through a UI; need production-ready tools, not just toys
- **Goals:** Learn Jujutsu concepts through familiar Emacs interface; build confidence before committing to full adoption

## The Problem

### Jujutsu Lacks a Mature Emacs Interface
While Jujutsu offers powerful version control primitives (especially stacked changesets and conflict-free merging), Emacs users lack a polished interface comparable to Magit. Existing implementations are either incomplete, experimental, or focused on basic vc.el integration rather than exposing Jujutsu's unique strengths. This creates a barrier for Emacs users who want to adopt Jujutsu but aren't willing to sacrifice the productivity they've gained from Magit.

**Our Solution:** Build a production-quality, Magit-inspired interface that not only matches basic Git workflow operations but specifically showcases Jujutsu's innovations - stacked changesets, powerful revision history manipulation, and conflict resolution - in a way that feels natural to Emacs power users.

### Stacked Changesets Need Visual, Interactive Tools
Jujutsu's stacked changesets model enables workflows that are painful in Git (managing dependent commits, reorganizing history, splitting/squashing changes), but these operations are complex to visualize and execute via CLI alone. The cognitive overhead limits adoption even when the underlying VCS is superior.

**Our Solution:** Provide visual, interactive interfaces for viewing and manipulating changeset stacks, making Jujutsu's most powerful features discoverable and accessible through Transient menus and dedicated buffer modes.

## Differentiators

### Dual Learning and Production Focus
Unlike experimental interfaces or pure learning projects, jj.el is designed with production quality from the start while embracing learning as a first-class goal. Testing infrastructure, CI/CD, and robust error handling are foundational priorities, not afterthoughts. This makes it both a safe tool for teams to adopt and an educational resource for developers exploring modern Emacs package development.

### Magit UX Familiarity with Jujutsu Innovation
Rather than reinventing interaction patterns, jj.el leverages Magit's proven UX paradigms (status buffers, transient popups, keybindings) while adapting them to expose Jujutsu's unique capabilities. This results in near-zero learning curve for the thousands of Emacs developers already fluent in Magit, while unlocking workflows impossible in Git.

### Stacked Changesets as a First-Class Workflow
While other implementations treat Jujutsu as "Git with different commands," jj.el is architected around stacked changesets as a primary workflow. Visual representation of changeset stacks, intuitive operations for reordering/rebasing dependent changes, and clear status indicators for stack health make advanced version control accessible to all skill levels.

### Built for the Emacs Ecosystem
Deep integration with Emacs conventions (Evil mode support, buffer management, standard keybindings) and leveraging modern tooling (Transient.el for menus, proper major modes, Eask for package management). The result is a package that feels like a native part of Emacs, not a bolt-on shell wrapper.

## Key Features

### Core Workflow Features
- **Status View:** Real-time overview of working directory changes with intuitive navigation and quick actions
- **Change Management:** Create, describe, abandon, and manipulate changes with Transient-powered interfaces
- **Commit Message Editing:** Integrated describe command with message editing workflow
- **Log and History:** Powerful log filtering with customizable revsets, patch viewing, and graph visualization

### Stacked Changesets Features
- **Visual Stack Representation:** Clear display of changeset dependencies and stack structure
- **Stack Navigation:** Move between related changes in a stack with keyboard shortcuts
- **Rebase and Reorder:** Interactive tools for reorganizing changeset stacks
- **Stack Health Indicators:** Visual feedback on conflicts, empty changes, or stack inconsistencies

### Integration Features
- **Git Remote Operations:** Fetch and push to Git remotes with branch/bookmark management
- **Evil Mode Support:** Full keybinding support for Evil users
- **Buffer Management:** Standard Emacs buffer conventions for all jj modes
- **CLI Integration:** Robust command execution with proper error handling and feedback

### Advanced Features
- **Diff Viewing:** Integrated patch and diff displays with whitespace handling options
- **Revset Support:** Full access to Jujutsu's powerful revset query language
- **Multi-Parent Changes:** Create changes with multiple parent revisions
- **Custom Templates:** Leverage jj's template system for log and status displays

## Success Metrics

### Adoption and Usage
- **Active Users:** Track GitHub stars, clone counts, and issue engagement
- **Community Growth:** Measure contributor count, PR submissions, and community discussions
- **Feature Completeness:** Progress toward parity with Magit's most-used features
- **Real-World Usage:** Anecdotal evidence of teams/individuals using jj.el for daily work

### Quality Indicators
- **Test Coverage:** Maintain high test coverage across all features
- **Bug Resolution Time:** Measure responsiveness to reported issues
- **Documentation Quality:** User feedback on learning curve and documentation completeness
- **Performance:** Ensure responsive UI even with large repositories

### Learning Outcomes
- **Educational Value:** Community feedback on jj.el as a learning resource for package development
- **Code Quality:** Adherence to Emacs Lisp best practices and standards
- **Architectural Decisions:** Document and share design patterns useful for similar projects

## Project Philosophy

jj.el embraces a dual nature as both a serious production tool and an educational journey:

- **Production Quality**: Testing, CI/CD, error handling, and stability are not optional - they're foundational
- **Learning in Public**: Development decisions, refactoring, and evolution are documented and shared
- **User-Centered Design**: Features are prioritized based on real workflows, not technical novelty
- **Community-Driven**: Open to contributions, feedback, and collaborative improvement
- **Long-Term Sustainability**: Architected for maintainability and extensibility as Jujutsu evolves
