# -*- encoding: utf-8 -*-
# frozen_string_literal: true

Gem::Specification.new do |s|
  s.name        = "vendoredlib2"
  s.version     = "1.0"
  s.platform    = Gem::Platform::RUBY
  s.licenses    = ["MPL"]
  s.summary     = "A fixture for FOSSA CLI testing"
  s.authors     = ['FOSSA']

  s.add_dependency("warden", "~> 1.2.3")
  s.add_dependency("orm_adapter", "~> 0.1")
  s.add_dependency("bcrypt", "~> 3.0")
  s.add_dependency("railties", ">= 4.1.0", "< 6.0")
  s.add_dependency("responders")
end