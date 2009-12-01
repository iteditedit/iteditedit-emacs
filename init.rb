require 'yaml'

submodules = YAML.load_file('submodules.yml')

installed_submodules = `git submodule`.split("\n").map {|line| line.match(/\ (.+)?$/)[1]}
puts "Installed submodules: #{installed_submodules*' '}"

submodules.each do |key, submodule|
  if installed_submodules.include?(submodule["path"])
    puts "#{key} already installed as submodule"
  else
    puts "installing '#{key}'... "
    puts `git submodule add #{submodule["url"]} #{submodule["path"]}`
  end
end

`git submodule init`
`git submodule update`