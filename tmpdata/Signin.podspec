Pod::Spec.new do |s|
  s.name                  = 'Signin'
  s.version               = '0.5.5'
  s.summary               = 'Signin SDK for iOS'
      
  s.homepage              = 'https://bitbucket.org/careemteam/signin-ios/'
  s.author                = { 'Careem' => 'careemdev@careem.com' }
  s.source                = { :git => 'git@bitbucket.org:careemteam/signin-ios.git', :tag => s.version.to_s }
  s.source_files          = 'Source/**/*.{swift}'
  s.platform              = :ios, '8.0'
  s.swift_version         = '5.1'
  s.frameworks            = 'Foundation'
  s.dependency 'AlamofireObjectMapper', '~> 5.2'
  s.dependency 'AppAuth'
end
