# pry everywhere
# http://lucapette.com/pry/pry-everywhere/
# alternatively, pry-rails can be used within a project (https://github.com/rweng/pry-rails)
# Stolen from Aiden Feldman (https://github.com/afeld/dotfiles/blob/master/irbrc#L57-72)
begin
  require 'pry'
rescue LoadError
  puts "WARN: couldn't load pry. `gem install pry`"
else
  begin
    require 'pry-debugger'
  rescue LoadError
    puts "WARN: couldn't load pry-debugger. `gem install pry-debugger`"
  end
  Pry.start
  exit
end
