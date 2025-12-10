const fs = require('fs').promises;
const path = require('path');
const ejs = require('ejs');
const matter = require('gray-matter');

let markedParser = null;

async function getPosts() {
  const postsDir = path.join(__dirname, 'posts');
  const files = await fs.readdir(postsDir);
  const posts = await Promise.all(
    files
      .filter(f => f.endsWith('.md') && !f.startsWith('_'))
      .map(async filename => {
        const full = path.join(postsDir, filename);
        const raw = await fs.readFile(full, 'utf8');
        const { data, content } = matter(raw);
        return {
          id: filename.replace(/\.md$/, ''),
          title: data.title || 'Untitled',
          date: data.date || '',
          description: data.description || '',
          content // raw markdown; will convert to html when rendering single post
        };
      })
  );

  posts.sort((a, b) => new Date(b.date) - new Date(a.date));
  return posts;
}

async function renderIndex(posts) {
  const template = await fs.readFile(path.join(__dirname, 'views', 'index.ejs'), 'utf8');
  return ejs.render(template, { posts });
}

async function renderPost(post) {
  if (!markedParser) {
    markedParser = (await import('marked')).marked;
  }
  
  const htmlContent = markedParser.parse(post.content);
  const template = await fs.readFile(path.join(__dirname, 'views', 'posts.ejs'), 'utf8');
  const postWithHtml = {
    ...post,
    content: htmlContent
  };
  
  return ejs.render(template, { post: postWithHtml });
}

async function build() {
  console.log('🔨 Building static site...');
  
  const outDir = path.join(__dirname, 'out');
  
  // Clean and create output directory
  try {
    await fs.rm(outDir, { recursive: true });
  } catch (e) {
    // dir doesn't exist yet
  }
  await fs.mkdir(outDir, { recursive: true });
  
  // Get posts
  const posts = await getPosts();
  console.log(`📝 Found ${posts.length} posts`);
  
  // Copy public folder
  const publicSrc = path.join(__dirname, 'public');
  const publicDest = path.join(outDir, 'public');
  await fs.mkdir(publicDest, { recursive: true });
  
  const publicFiles = await fs.readdir(publicSrc);
  for (const file of publicFiles) {
    const src = path.join(publicSrc, file);
    const dest = path.join(publicDest, file);
    const stat = await fs.stat(src);
    
    if (stat.isDirectory()) {
      // Recursively copy directories
      await copyDir(src, dest);
    } else {
      await fs.copyFile(src, dest);
    }
  }
  console.log('📂 Copied public assets');
  
  // Render index page
  const indexHtml = await renderIndex(posts);
  await fs.writeFile(path.join(outDir, 'index.html'), indexHtml);
  console.log('✅ Generated index.html');
  
  // Render each post
  for (const post of posts) {
    const postDir = path.join(outDir, 'post', post.id);
    await fs.mkdir(postDir, { recursive: true });
    
    const postHtml = await renderPost(post);
    await fs.writeFile(path.join(postDir, 'index.html'), postHtml);
    console.log(`✅ Generated /post/${post.id}/index.html`);
  }
  
  console.log('\n🎉 Build complete! Output in ./out/');
  console.log('   Deploy the "out" folder to your static host (Vercel, Netlify, GitHub Pages, etc)');
}

async function copyDir(src, dest) {
  await fs.mkdir(dest, { recursive: true });
  const files = await fs.readdir(src);
  
  for (const file of files) {
    const srcPath = path.join(src, file);
    const destPath = path.join(dest, file);
    const stat = await fs.stat(srcPath);
    
    if (stat.isDirectory()) {
      await copyDir(srcPath, destPath);
    } else {
      await fs.copyFile(srcPath, destPath);
    }
  }
}

build().catch(err => {
  console.error('❌ Build failed:', err);
  process.exit(1);
});
