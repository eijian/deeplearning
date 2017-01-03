#!/usr/bin/ruby

STDIN.each do |l|
  /^iter =\s+(\d+)\/\d+ accuracy = (\S+) time = (\S+)s\s*$/ =~ l
  #/^iter =\s+(\d+)\/\d+ time = (\S+)s ratio = (\S+)$/ =~ l
  next if $1 == nil || $2 == nil || $3 == nil
  puts "#{$1},#{$2},#{$3}"
end
