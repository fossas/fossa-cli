Gem::Specification.new do |s|
  s.specification_version = 2 if s.respond_to? :specification_version=
  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=

  s.name = 'foo'
  s.version = '0.0.0'
  s.date = '2021-01-01'

  s.description = "test gemspec"
  s.summary     = s.description
  s.license     = "Ruby"

  s.authors = ["foo"]
  s.email = "foo@bar.com"

  # = MANIFEST =
  s.files = %w[
    COPYING
    lib/foo.rb
    lib/bar.rb
  ]
  # = MANIFEST =

  s.executables = ['foo']

  s.homepage = "https://foo.com"
  s.rdoc_options = ["--line-numbers", "--inline-source", "--title", "Foo", "--main", "None"]
  s.require_paths = %w[lib]
  s.rubygems_version = '1.1.1'
end
