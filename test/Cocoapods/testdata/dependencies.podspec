Pod::Spec.new do |s|
  s.name             = 'baz'
  s.version          = '0.0.0'
  s.summary          = 'fake'
  s.homepage         = 'https://foo.bar'
  s.license          = {:type => "GPL",
                        :file => "LICENSE.txt",
                        :text => "<license text>"}
  s.author           = { 'foo' => 'bar' }
  s.source           = { :git => 'https://github.com', :tag => s.version.to_s }
  s.platform     = :ios, '0'

  s.source_files = 'src/'
  s.public_header_files = 'include/'
  s.resource_bundles = {
    'Resource1' => ['assets/*']
  }
  s.dependency 'dep-one', '1.0.0'
end
