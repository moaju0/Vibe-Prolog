# Vibe Prolog Websitex

## Overview

The website automatically pulls content from the project's markdown files:
- `README.md` → Home page
- `FEATURES.md` → Features page
- `ARCHITECTURE.md` → Documentation page
- `AGENTS.md` → Development page

Images from the parent `images/` directory are also copied to the website during the build process.

## Development

### Prerequisites

- Node.js 20 or later
- npm

### Local Development

```bash
# Install dependencies
npm install

# Copy markdown files and start dev server
npm run dev
```

The dev server will start at `http://localhost:4321`.

### Build

```bash
# Build the website
npm run build
```

The built files will be in the `dist/` directory.

### Preview

```bash
# Preview the built site
npm run preview
```

## Deployment

The website is automatically deployed to GitHub Pages when changes are pushed to the `main` branch. The deployment is handled by the GitHub Actions workflow at `.github/workflows/deploy-website.yml`.

### Manual Deployment

You can also trigger a manual deployment from the GitHub Actions tab by running the "Deploy Website to GitHub Pages" workflow.

### GitHub Pages Configuration

The repository needs to be configured for GitHub Pages:

1. Go to repository Settings → Pages
2. Under "Source", select "GitHub Actions"
3. The workflow will automatically build and deploy on push

## Project Structure

```
website/
├── src/
│   ├── layouts/
│   │   └── Layout.astro       # Base layout with navigation
│   ├── pages/
│   │   ├── index.astro        # Home page
│   │   ├── features.astro     # Features page
│   │   ├── docs.astro         # Documentation page
│   │   └── development.astro  # Development guide
│   └── content/               # (Generated) Markdown files copied from parent
├── public/
│   └── images/                # (Generated) Images copied from parent
├── scripts/
│   └── copy-markdown.mjs      # Script to copy markdown and images
├── astro.config.mjs           # Astro configuration
└── package.json               # Dependencies and scripts
```

## Customization

### Styling

The website uses CSS variables defined in `src/layouts/Layout.astro`. You can customize the colors and fonts by editing the `:root` section.

### Navigation

Update the navigation links in `src/layouts/Layout.astro` in the `<nav>` section.

### Content

The content is automatically pulled from the parent directory's markdown files. To update the website content, edit the markdown files in the parent directory and rebuild.

## Troubleshooting

### Build Errors

If you encounter build errors:

1. Make sure all markdown files exist in the parent directory
2. Run `npm run copy-content` to manually copy the markdown files
3. Check for syntax errors in the markdown files

### Broken Images

If images aren't showing:

1. Verify the images exist in the parent `images/` directory
2. Check that the paths in the markdown files are correct (e.g., `./images/TheCastle.jpg`)
3. Run a fresh build with `npm run build`

## Scripts

- `npm run copy-content` - Copy markdown files and images from parent directory
- `npm run dev` - Start development server
- `npm run build` - Build for production
- `npm run preview` - Preview production build
- `npm run astro` - Run Astro CLI commands
