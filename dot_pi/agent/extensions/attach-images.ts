// Attach image files named in the prompt as real image parts. Pasting or
// dragging into the TUI, and the Emacs frontend, only insert the path as
// text, so the model never sees the pixels and has to be told to `read` it.
import { readFileSync, existsSync } from "node:fs";
import { extname } from "node:path";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const MIME: Record<string, string> = {
  ".png": "image/png", ".jpg": "image/jpeg", ".jpeg": "image/jpeg",
  ".webp": "image/webp", ".gif": "image/gif",
};
// absolute or ~/ paths; spaces allowed (macOS screenshots have them)
const PATH_RE = /@?((?:~|\/)[^\n"']*?\.(?:png|jpe?g|webp|gif))(?=\s|$)/gi;

export default function (pi: ExtensionAPI) {
  pi.on("input", async (event) => {
    if (event.source === "extension") return { action: "continue" };
    const images = [...(event.images ?? [])];
    let text = event.text;
    for (const m of event.text.matchAll(PATH_RE)) {
      const file = m[1].replace(/^~/, process.env.HOME ?? "~");
      if (!existsSync(file)) continue;
      images.push({
        type: "image",
        data: readFileSync(file).toString("base64"),
        mimeType: MIME[extname(file).toLowerCase()] ?? "image/png",
      });
      text = text.replace(m[0], `[image: ${file.split("/").pop()}]`);
    }
    if (images.length === (event.images?.length ?? 0)) return { action: "continue" };
    return { action: "transform", text: text.trim() || "What do you see?", images };
  });
}
