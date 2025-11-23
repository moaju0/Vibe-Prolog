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
  'README.md',
  'FEATURES.md',
  'ARCHITECTURE.md',
  'AGENTS.md',
];

console.log('Copying markdown files...');
files.forEach(file => {
  const src = join(rootDir, file);
  const dest = join(contentDir, file);

  if (existsSync(src)) {
    copyFileSync(src, dest);
    console.log(`  ✓ Copied ${file}`);
  } else {
    console.warn(`  ⚠ File not found: ${file}`);
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
