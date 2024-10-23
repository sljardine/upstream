## Background

Upstream is a Golem/ RShiny app. You can learn more about Golem [here](https://engineering-shiny.org/golem.html).

We use Git for version control. You can learn more about the basics of using Git [here](https://www.freecodecamp.org/news/learn-the-basics-of-git-in-under-10-minutes-da548267cc91/).

## Setting up your development environment - Mac instructions

If this is your first time using your Mac for development, start by installing the following software:
1) XCode Command Line Tools: Install using the command `xcode-select --install` in a Terminal window.
2) Homebrew is an incredibly useful package manager tool for developing on a Mac. Find installation instructions [here](https://brew.sh/), and make sure to follow the steps about adding Homebrew to your PATH.
3) Install Git using Homebrew with the command `brew install git`.
4) Download and install [RStudio](https://posit.co/download/rstudio-desktop/). 
5) Clone the repository: `git clone https://github.com/sljardine/upstream.git`. You may need to set up SSH keys for GitHub - there's a good help article explaining how to do that [here](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account).

## Hot-reloading

By default, the development port is set to 8000 in run_dev.R. Hot reloading is a standard web development feature where when changes are made to the source code, the code automatically recompiles and your localhost updates with your changes. Typically, this happens in milliseconds and doesn't require any page refreshing or work on your part as a developer. Unfortunately, there is a [known issue](https://github.com/ThinkR-open/golem/issues/263) for Golem/RShiny apps where hot re-loading does not work. We've implemented one of the workarounds suggested in the issue article linked above. 

Install the entr package using the command `brew install entr` on Terminal. Then, within Terminal, navigate to the utils folder within upstream. Run `./run_app.sh`. This will start a watch script that watches for changes to files in the /R directory. When changes are found, it kills any running processes on port 8000 and then re-runs the run_dev.R script to restart the app.

After run_app has started, navigate in your browser to localhost:8000. You should see the Upstream app has rendered. Then, make a small change to a file in the /R directory. Refresh the webpage, observe your change appearing.
