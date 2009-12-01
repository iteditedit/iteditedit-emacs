Vendor_to_load = lambda { YAML.load_file('panel.yml')['vendor'].
  select { |(name, val)| val == 1 }.
    map {|hash_pair| hash_pair.first}}
    
PJoin = lambda {|name| File.join('personalization', name + '.el') }
P = lambda {|name| File.exists?(PJoin[name]) ? File.read(PJoin[name]) : ''}

AddLoadPath = lambda {|path| %Q{(add-to-list 'load-path "#{path}")\n}}    
    