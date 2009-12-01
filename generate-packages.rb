require 'yaml'

vendor_hash = Dir.glob('vendor/*').map do |x| 
    File.split(x)[1]
  end.inject({'vendor' => {}}) do |res, vendor| 
    res['vendor'][vendor] = 1
    res 
  end

File.open('panel.yml','w+') do |f|
  f.write vendor_hash.to_yaml
end