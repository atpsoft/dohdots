If you want to use and make changes to dohdots, you need to generate an ssh key (if you haven't already), and add it to your github account.  Remember to pick a passphrase for your key, and store it safely.
``` bash
ssh-keygen
ssh-add
```
then clone the repository:
``` bash
mkdir -p ~/src
cd ~/src
git clone git@github.com:atpsoft/dohdots.git
```

But if you want to simply install dohdots, and not make any changes, you can just do this:
``` bash
curl -B https://raw.githubusercontent.com/atpsoft/dohdots/master/install.rb | ruby
```
