#!/usr/bin/ruby
#
#

XSIZE = 12
YSIZE = 12
CLASS = 3

def init
  if ARGV.size != 2
    STDERR.puts "Usage: mksimpledat.rb <probably> <nsample>"
    exit 1
  end

  @prob = ARGV[0].to_f
  @nsample = ARGV[1].to_i
  @random = Random.new(Time.now.to_i)
end

def mkdat(i, cl)
  d = Array.new
  st = YSIZE / CLASS
  YSIZE.times do |y|
    p2 = if y / st == cl then @prob else 1.0 - @prob end
    XSIZE.times do |x|
      v = @random.rand
      d << (if v < p2 then 127 else 0 end)
    end
  end
  d
end

def main
  init

  @nsample.times do |i|
    cl = i % CLASS
    fname = sprintf("#{cl}/%04d.pgm", i)
    File.open(fname, 'w') do |fp|
      body = <<"EOF"
P5
# mksimpledat.rb
#{XSIZE} #{YSIZE}
255
EOF
      fp.print body
      d = mkdat(i, cl)
      fp.write d.pack('C*')
    end
  end
end

main
