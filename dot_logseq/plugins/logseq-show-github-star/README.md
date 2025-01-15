# logseq-show-github-star

A simple logseq plugin that will display the star number for GitHub repo links.

Demo1:
![demo1](./demo1.gif)

This plugin will call the GitHub API.
If the Access Token is not set, the number of calls per hour will be limited, so it is recommended to set the GitHub Personal Access Token.

Create a Personal Access Token:
- Log in to your GitHub account.
- Click on your profile picture in the top right corner > Settings.
- On the left sidebar, click on "Developer settings".
- On the left sidebar, click on "Personal access tokens (classic)".
- Click "Generate new token".
- Choose appropriate permissions for the token. Because we are just making an API request, there is no need to select any permissions, just click the generate button
- Click "Generate token".
- Copy your new token and make sure to save it, as you won't be able to see it again.
