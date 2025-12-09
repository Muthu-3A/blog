const express = require('express');
const app = express();
const posts = require("./posts");

app.set("view engine", "ejs");

app.use(express.static("public"));

app.get('/', (req, res) => {
	res.render('index', { posts });
});

app.get('/post/:id', (req, res) => {
	const post = posts.find(p => p.id == req.params.id);

	if (!post) return res.status(404).send("Post not found");

	res.render("posts", { post });
});

app.listen(3000, () => console.log('Server running on https://localhost:3000'));
