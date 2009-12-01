require 'yaml'
require 'helpers'

print 'Generate load path...'

File.open(PJoin['path'],'w') do |f|
  #(*('personalization','private'].map {|name| "~/.emacs.d/#{name}"}),
  ['.', *Vendor_to_load[]].each do |path|      
    f.write(AddLoadPath['~/.emacs.d/vendor/' + path])
  end
end

puts 'OK'
