#!/usr/bin/ruby
#
# drv_pnm2dat.rb
# Usage: drv_pnm2dat.rb (target dir) (#class)
#

USAGE = "Usage: drv_pnm2dat.rb (target dir) (#class)"
PNM2DAT = __dir__ + '/pnm2dat.rb'

def init
  if ARGV.size != 2
    STDERR.puts USAGE
    exit 1
  end

  $DIR = ARGV[0]
  if File.exist?($DIR) == false
    STDERR.puts "No valid target dir: #{$DIR}"
    STDERR.exit 1
  end
  $NCLASS = ARGV[1].to_i
end

def pnm2dat(c)
  Dir.glob("#{$DIR}/#{c}/*.p?m") do |f|
    dat = "#{$DIR}/#{c}/#{File.basename(f, ".*")}.dat"
    next if File.exist?(dat) && File.ctime(dat) > File.ctime(f)
    #puts "F:#{f}, #{dat}"
    system("ruby #{PNM2DAT} < #{f} > #{dat}")
  end
end

def main
  init
  $NCLASS.times do |i|
    pnm2dat i
  end
end

main
