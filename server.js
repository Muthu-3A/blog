const express = require('express');
const matter = require('gray-matter');
const fs = require('fs').promises;
const path = require('path');
let markedParser = null;

const app = express();
app.set('view engine', 'ejs');

app.use(express.static('public'));

const POSTS_DIR = path.join(__dirname, 'posts');

async function getPosts() {
	const files = await fs.readdir(POSTS_DIR);
	const posts = await Promise.all(
		files
			.filter(f => f.endsWith('.md'))
			.map(async filename => {
				const full = path.join(POSTS_DIR, filename);
				const raw = await fs.readFile(full, 'utf8');
				const { data, content } = matter(raw);
				return {
					id: filename.replace(/\.md$/, ''),
					title: data.title || 'Untitled',
					date: data.date || '',
					description: data.description || '',
					content // raw markdown; convert to html when rendering single post
				};
			})
	);

	posts.sort((a, b) => new Date(b.date) - new Date(a.date));
	return posts;
}

app.get('/', async (req, res, next) => {
	try {
		const posts = await getPosts();
		res.render('index', { posts });
	} catch (err) {
		next(err);
	}
});

app.get('/post/:id', async (req, res, next) => {
	try {
		const filename = path.join(POSTS_DIR, req.params.id + '.md');
		try {
			await fs.access(filename);
		} catch (e) {
			return res.status(404).send('Post not found');
		}

		const raw = await fs.readFile(filename, 'utf8');
		const { data, content } = matter(raw);
		if (!markedParser) {
			markedParser = (await import('marked')).marked;
		}
		const html = markedParser.parse(content);
		const post = {
			id: req.params.id,
			title: data.title || 'Untitled',
			date: data.date || '',
			description: data.description || '',
			content: html
		};

		res.render('posts', { post });
	} catch (err) {
		next(err);
	}
});

app.listen(3000, () => console.log('Server running on http://localhost:3000'));
