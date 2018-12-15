Vagrant.configure("2") do |config|
  config.vm.box = "ubuntu/xenial64"
  config.vm.synced_folder "./", "/home/vagrant/Flori", owner: "vagrant", group: "vagrant"
  config.vm.provision "shell", :privileged => true, inline: <<-SHELL
    sed -i.bak -e "s%http://us.archive.ubuntu.com/ubuntu/%http://ftp.jaist.ac.jp/pub/Linux/ubuntu/%g" /etc/apt/sources.list
    apt update
    apt install -y build-essential
  SHELL
  config.vm.provision "shell", :privileged => false, inline: <<-SHELL
    cd ~/
    git clone https://github.com/snowlt23/adhocc
    cd adhocc
    make
    echo PATH='$PATH:$HOME/adhocc' >> ~/.bashrc
  SHELL
end