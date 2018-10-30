#!/usr/bin/ruby
#
# MNIST data labeler
#

require 'fileutils'

USAGE = "Usage:"
SKIP = 8
LEN = 1

def init
  if ARGV.size != 1
    STDERR.puts USAGE
    exit 1
  end

  $FILE = ARGV[0]
end

def main
  init
  dat = File.binread($FILE)
  dat = dat.unpack("C*").drop(SKIP)
  dat.length.times do |i|
    fn = sprintf("%08d.pgm", i)
    fn2 = "#{dat[i]}/#{fn}"
    puts fn2
    FileUtils.move(fn, fn2)
  end

end

main


