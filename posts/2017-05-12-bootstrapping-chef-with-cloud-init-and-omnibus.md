---
title: Bootstrapping chef with cloud-init and omnibus
---
Dear reader,

Please don't use cloud-init's chef bootstrapper, because it's not being maintained and doesn't offer you locking the version either.

    #cloud-config
    # ^ This line's actually required! Like a shebang!
    # https://i.ytimg.com/vi/GD6qtc2_AQA/maxresdefault.jpg
    # http://stackoverflow.com/questions/36921195/user-data-not-being-execute-by-cloud-init
    
    write_files:
      - path: /etc/chef/acme-organization-validation.pem
        permissions: '0600'
        encoding: gz
        content:
          YOUR PEM GOES HERE
    
      - path: /etc/chef/client.rb
        content: |
          chef_server_url "https://chef.acme.com/organizations/acme"
          validation_client_name   "acme-validator"
          validation_key           "/etc/chef/acme-organization-validation.pem"
    
      - path: /etc/chef/boot.json
        content: |
          {"run_list": ["role[cookbook]"]}
    
    runcmd:
      - curl -LO "https://omnitruck.chef.io/install.sh"
      - bash ./install.sh -v 12.19
      - rm install.sh
      - [
        chef-client,
        "--environment", _default,
        "-j", "/etc/chef/boot.json",
        "-N", "${domain}"
      ]
      - rm "/etc/chef/acme-organization-validation.pem"
