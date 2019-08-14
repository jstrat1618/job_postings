DROP table job_posts.main_posts;

CREATE TABLE job_posts.main_posts(
id INT PRIMARY KEY,
title TEXT,
link TEXT,
location TEXT,
summary TEXT,
pulled DATETIME
);