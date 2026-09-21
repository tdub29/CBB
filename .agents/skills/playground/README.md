# Playground Skill

Creates interactive HTML playgrounds — self-contained single-file explorers that let users configure something visually through controls, see a live preview, and copy out a prompt.

**Source:** [anthropics/claude-plugins-official — plugins/playground](https://github.com/anthropics/claude-plugins-official/tree/main/plugins/playground)

## What is a Playground?

A playground is a self-contained HTML file with:

- Interactive controls on one side
- A live preview on the other
- A prompt output at the bottom with a copy button

The user adjusts controls, explores visually, then copies the generated prompt back into the chat.

## When to Use

Use this skill when the user asks for an interactive playground, explorer, or visual tool for a topic — especially when the input space is large, visual, or structural and hard to express as plain text.

## Templates

The skill includes templates for common playground types:

| Template | Use for |
|----------|---------|
| **design-playground** | Visual design (components, layouts, spacing, color, typography) |
| **data-explorer** | Data and query building (SQL, APIs, pipelines, regex) |
| **concept-map** | Learning and exploration (concept maps, knowledge gaps, scope mapping) |
| **document-critique** | Document review (suggestions with approve/reject/comment workflow) |
| **diff-review** | Code review (git diffs, commits, PRs with line-by-line commenting) |
| **code-map** | Codebase architecture (component relationships, data flow, layer diagrams) |
| **image-annotation** | Image review and revision (regions + feedback → copyable revision prompt; for generate→annotate→refine) |

## Usage

1. User asks for a playground (e.g. “Make a button style explorer”).
2. Load the matching template from `templates/`.
3. Build the playground following the template; output a single HTML file.
4. Open the file in the browser (e.g. `open path/to/playground.html`).

See `SKILL.md` for full instructions and patterns.
