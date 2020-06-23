# A haiku showing how permissive our current parser is.
To prove our parser
This is not in a comment
No hashtag needed

# Basic parsing
BuildRequires: boost

# Simple version text
BuildRequires: cmake >= 2.8
BuildRequires: jsoncpp <= 0.10.5
BuildRequires: gtest > 6.4
BuildRequires: libevent < 9.8.1
BuildRequires: libvirt == 3.9.0
BuildRequires: openssl = 1.23.45

# Bad operator, will ignore version
BuildRequires: xz >> 1.2.3

# Lots of spaces, should be fine
BuildRequires:   libxml2     >=   5.4.98

# Complex version text (shouldn't change behavior)
BuildRequires:  gmock = 1.6.0-1.aapps.el7

# We also handle "Requires" headers
Requires:   tacp

# Ignored due to macro use
BuildRequires:  cloudistics-core%{?_isa} = %{version}-%{release}

