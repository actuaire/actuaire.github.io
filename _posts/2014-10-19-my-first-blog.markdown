---
layout: post
title:  "My first blog"
date:   2014-10-19 18:20:11
categories: blog
published : false
---

Finally, I have a blog run on Jekyll and Github. In my case, the simplest way is using pure Jekyll. Others solutions such as Octopress or Jekyllbootstrap have cost me a lot energy and time. Moreover, I felt that these latters have taken away from the UNIX's KISS principle. What please me the most with pure Jekyll is the Spartan configurations. Everythins is about the contents, the appearance is reduced to the strict necessary.

Here are steps (in my case) to lauch the [datasciencer] blog:

## Set-up stuffs:

* Install Git : 
{% highlight sh %}
sudo pacman -S git
{% endhighlight %}

* Ruby using **rbenv** [How](http://octopress.org/docs/setup/rbenv/). Add this to your *.bashrc* file

{% highlight sh %}
cd
git clone git://github.com/sstephenson/rbenv.git .rbenv
echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bashrc
echo 'eval "$(rbenv init -)"' >> ~/.bashrc
git clone git://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
{% endhighlight %}

* Pre-install nodejs to have a javascript instant :
{% highlight sh %}
 sudo pacman -S nodejs
{% endhighlight %}
* Create within my Github account the repository *datasciencer.github.io*
* Install openssh

* Generate and add the public ssh key to this repository inside Settings/Deploy key. The key is show by enter this command line inside the terminal : cat .ssh/id_rsa.pub
* Install **jekyll** through Ruby's **gem**
{% highlight sh %}
 gem install jekyll
{% endhighlight %}

## Write the first blog

 Let's start now by creating the jekyll blog : 

* jekyll new datasciencer.github.io
* cd datasciencer.github.io
* git remote set-url origin git@github.com:datasciencer/datasciencer.githu.io.git
* git add .
* git commit -m "first commit"
* git pull
* git push origin master


Let's check if jekyll could highlight R code

{% highlight R %}

packages_vec <\- c('reshape2', 'ggplot2', 'XLConnect', 'dplyr')
lapply(packages_vec, require, character.only=T)

{% endhighlight %}


In case that you have multi-github-accounts and want to set ssh certificates,  follow [this link](http://code.tutsplus.com/tutorials/quick-tip-how-to-work-with-github-and-multiple-accounts--net-22574) : 

* Generate a key for each repository of different github accounts
* Create a config file inside .ssh/ for ghaccount-1 and ghaccount-2
* Inside each origin folder, for example github account-1, enter this command into the terminal : git remote set-url origin git@ghaccount-1:nameaccount1/nameaccount1.github.io.git

## Ajustement for Pixyll theme

To change headers' font size, go to _sass/_main.scss

Modify font size for headers h1, h2, h3 ... inside three blocks *@media screen* : min-width : 48em, max-width : 48em, min-width : 64e


Check out the [Jekyll docs][jekyll] for more info on how to get the most out of Jekyll. File all bugs/feature requests at [Jekyll’s GitHub repo][jekyll-gh]. If you have questions, you can ask them on [Jekyll’s dedicated Help repository][jekyll-help].

[jekyll]:      http://jekyllrb.com
[jekyll-gh]:   https://github.com/jekyll/jekyll
[jekyll-help]: https://github.com/jekyll/jekyll-help
[datasciencer] : http://datasciencer.github.io/
