guard :shell do
  watch /^selfupdate\/.+\.go$/ do |m|
    puts "#{Time.now}: #{m[0]}"
    case m[0]
    when /_test\.go$/
      parent = File.dirname m[0]
      sources = Dir["#{parent}/*.go"].reject{|p| p.end_with? '_test.go'}.join(' ')
      system "go test -v -short #{m[0]} #{sources}"
    else
      system 'go build ./selfupdate/'
    end
  end

  watch /^cmd\/selfupdate-example\/.+\.go$/ do |m|
    puts "#{Time.now}: #{m[0]}"
    system 'go build ./cmd/selfupdate-example/'
  end
end
