# Image Annotation Template

Use this template when the playground lets users review and annotate an image: mark regions or sections, add labels and feedback per area, and produce a single revision prompt to feed back into an image generator or editor. Fits the **generate → annotate → refine** agentic loop ([DAIR.AI: Agentic Image Generation](https://academy.dair.ai/blog/agentic-image-generation)).

## Layout

```
+---------------------------+--------------------+
|                           |                    |
| Image preview             | Annotations panel  |
| with numbered markers     | (filterable list)  |
| or click-to-place pins    | • Region label     |
|                           | • Feedback text    |
|                           | • Add / Delete     |
|                           |                    |
+---------------------------+--------------------+
| Prompt output (all annotations as revision instructions) |
| [ Copy Prompt ]                                           |
+----------------------------------------------------------+
```

## Key components

### Image panel (left)
- Display the image (user-provided URL, file input, or embedded base64).
- Numbered markers or pins for each annotation (click to focus).
- Optional: click on image to add a new annotation at a position (show pin + open form in sidebar).
- Optional: drag pins to reposition.

### Annotations panel (right)
- List of annotations: each has a label (e.g. region name), optional position hint, and feedback text.
- Add annotation button; delete per annotation.
- Reorder or filter (e.g. by "needs revision" vs "approved") if needed.
- Each card: label input, textarea for feedback, delete button.

### Prompt output (bottom)
- Single natural-language revision prompt combining all annotations.
- Format: "Please revise [IMAGE DESCRIPTION] with the following changes: 1. [Region A] – [feedback]. 2. [Region B] – [feedback]."
- Copy button with "Copied!" feedback.
- Updates live as user edits annotations.

## State structure

```javascript
const state = {
  imageSource: null,        // data URL or object URL
  imageTitle: 'Untitled',   // e.g. "Building Effective Agents infographic"
  annotations: [
    {
      id: 1,
      label: 'Workflows vs Agents',
      x: 0.2, y: 0.3,      // optional normalized 0–1
      feedback: 'Can we make these little bubbles a nice light green'
    },
    {
      id: 2,
      label: 'Key Principles',
      feedback: 'Can we make the icons nicer'
    }
  ]
};
```

## Prompt output generation

Build a single revision prompt from all annotations:

```javascript
function updatePrompt() {
  if (state.annotations.length === 0) {
    promptEl.textContent = 'Add annotations above. Prompt will appear here.';
    return;
  }

  const title = state.imageTitle || 'this image';
  let text = `Please revise the "${title}" with the following changes:\n\n`;

  state.annotations.forEach((a, i) => {
    const region = a.label ? `[${a.label}]` : `[Region ${i + 1}]`;
    text += `${i + 1}. ${region} – ${a.feedback.trim() || '(no feedback)'}\n`;
  });

  promptEl.textContent = text;
}
```

## Image input

Support at least one of:

- **File input:** `<input type="file" accept="image/*">` → `URL.createObjectURL(file)`.
- **URL input:** User pastes image URL → use as `img.src` (ensure CORS or proxy if needed).
- **Embedded:** Page is built for a specific image (e.g. infographic) with image as base64 or path; no picker.

When building for a specific asset (e.g. "review this infographic"), embed the image and pre-fill 2–5 example annotations so the prompt output is immediately useful.

## Styling

- Dark theme: dark background, light text, subtle borders.
- Markers: numbered badges (1, 2, 3) overlaid on image; highlight active.
- Annotations list: cards with label, feedback textarea, delete; minimal chrome.
- Prompt area: monospace or readable font, copy button prominent.

## Presets

- **Empty:** One placeholder annotation to show structure.
- **Example set:** 2–3 sample annotations (e.g. "Header – make bolder", "Chart – use blue tones") so the user sees the format.

## Common mistakes

- Prompt is raw key/value dump → use natural revision instructions.
- No image input or only URL (no file) → support file upload for local use.
- Annotations not tied to regions → use labels or positions so the prompt is clear ("[Section name] – feedback").
- Copy doesn’t include image context → include image title or short description in the prompt.

## Example use cases

- Infographic revision (sections like "Workflows vs Agents", "Key Principles").
- Logo or brand asset feedback (areas: mark, icon, wordmark).
- Screenshot or UI mockup review (components: header, sidebar, card).
- Marketing image (regions: headline, CTA, product).
- Any image where you want to paste a single revision prompt back into Claude/Cursor or an image generator.

## Integration with agentic loop

After the user copies the prompt:

1. User pastes into Claude Code / Cursor with the original image.
2. Agent (or image generator plugin) produces a revised image.
3. User can open the same playground again with the new image and iterate: **generate → annotate → refine**.
