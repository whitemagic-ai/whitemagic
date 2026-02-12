/**
 * WhiteMagic SDK — Context Sanitizer.
 *
 * Redacts likely secrets and tokens before sending context to LLM providers.
 * Mirrors the Python _sanitize_context() in whitemagic/tools/handlers/ollama_agent.py.
 */

const SANITIZE_PATTERNS: [RegExp, string][] = [
  [/sk-[A-Za-z0-9]{20,}/g, "[REDACTED_API_KEY]"],
  [/sk-proj-[A-Za-z0-9_-]{40,}/g, "[REDACTED_API_KEY]"],
  [/ghp_[A-Za-z0-9]{36,}/g, "[REDACTED_GITHUB_TOKEN]"],
  [/gho_[A-Za-z0-9]{36,}/g, "[REDACTED_GITHUB_TOKEN]"],
  [/xox[bpsar]-[A-Za-z0-9-]{10,}/g, "[REDACTED_SLACK_TOKEN]"],
  [
    /-----BEGIN [A-Z ]*PRIVATE KEY-----[\s\S]*?-----END [A-Z ]*PRIVATE KEY-----/g,
    "[REDACTED_PRIVATE_KEY]",
  ],
  [
    /eyJ[A-Za-z0-9_-]{20,}\.eyJ[A-Za-z0-9_-]{20,}\.[A-Za-z0-9_-]{20,}/g,
    "[REDACTED_JWT]",
  ],
  [/AKIA[0-9A-Z]{16}/g, "[REDACTED_AWS_KEY]"],
];

/**
 * Redact likely secrets and tokens from text.
 *
 * @param text - The text to sanitize
 * @returns The sanitized text with secrets replaced by [REDACTED_*] placeholders
 */
export function sanitizeContext(text: string): string {
  for (const [pattern, replacement] of SANITIZE_PATTERNS) {
    text = text.replace(pattern, replacement);
  }
  return text;
}
