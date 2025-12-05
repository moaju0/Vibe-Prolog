import { copyFileSync, mkdirSync, existsSync, cpSync } from 'fs';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const rootDir = join(__dirname, '..', '..');
const contentDir = join(__dirname, '..', 'src', 'content');
const publicDir = join(__dirname, '..', 'public');

// Create content directory if it doesn't exist
if (!existsSync(contentDir)) {
  mkdirSync(contentDir, { recursive: true });
}

// List of markdown files to copy
const files = [
  { src: 'README.md', dest: 'README.md' },
  { src: 'docs/FEATURES.md', dest: 'FEATURES.md' },
  { src: 'docs/ARCHITECTURE.md', dest: 'ARCHITECTURE.md' },
  { src: 'docs/GRAMMAR_COMPARISON.md', dest: 'GRAMMAR_COMPARISON.md' },
  { src: 'docs/SYNTAX_NOTES.md', dest: 'SYNTAX_NOTES.md' },
  { src: 'AGENTS.md', dest: 'AGENTS.md' },
];

console.log('Copying markdown files...');
files.forEach(file => {
  const src = join(rootDir, file.src);
  const dest = join(contentDir, file.dest);

  if (existsSync(src)) {
    copyFileSync(src, dest);
    console.log(`  ✓ Copied ${file.src} → ${file.dest}`);
  } else {
    console.warn(`  ⚠ File not found: ${file.src}`);
  }
});

// Copy images directory
const imagesDir = join(rootDir, 'images');
const publicImagesDir = join(publicDir, 'images');

if (existsSync(imagesDir)) {
  console.log('Copying images...');
  cpSync(imagesDir, publicImagesDir, { recursive: true });
  console.log('  ✓ Copied images directory');
}

console.log('Done!');
