

require 'digest/sha1'

commit = "commit 212\0"

File.open(ARGV[0], 'r') do |fp|
  commit = fp.read
end

puts Digest::SHA1.hexdigest(commit)

