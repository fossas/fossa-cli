Pod::Spec.new do |s|
  s.name             = 'foo'
  s.version          = '0.0.0'
  s.summary          = 'fake'
  s.homepage         = 'https://foo.bar'
  # This next license line should be skipped
  # s.license          = 'LGPL'
  s.license          = 'MIT'
  s.author           = { 'foo' => 'bar' }
  s.source           = { :git => 'https://github.com', :tag => s.version.to_s }
  s.platform     = :ios, '0'

  s.source_files = 'src/'
  s.public_header_files = 'include/'
  s.resource_bundles = {
    'Resource1' => ['assets/*']
  }
end
