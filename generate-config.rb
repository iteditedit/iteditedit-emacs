require 'yaml'
require 'helpers'

puts `ruby generate-load-path.rb`

print 'Generate main config...'

File.open('init.el', 'w') do |f|
  ['path', 'emacs', 'keys', *Vendor_to_load[]].each {|name| f.write(P[name])} 
end

puts 'OK'