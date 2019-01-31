Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/xenial64"
  config.vm.network "private_network", ip: "192.168.55.5"
  config.vm.network "forwarded_port", guest: 4545, host: 4545
  config.vm.synced_folder "./", "/home/vagrant/Flori", owner: "vagrant", group: "vagrant"
  config.vm.provision "shell", :privileged => true, inline: <<-SHELL
    apt update
    apt install -y build-essential
  SHELL
  config.vm.provision "shell", :privileged => false, inline: <<-SHELL
    cd ~/
    git clone https://github.com/snowlt23/adhocc
    cd adhocc
    make
    echo PATH='$PATH:$HOME/adhocc' >> ~/.bash_profile
  SHELL
end
