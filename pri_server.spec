%global upstream madrat
%global realname pri_server
%global git_tag head 

# Later, when patch files will be added
%global patchnumber 0

%global debug_package %{nil}

Name:		erlang-%{realname}
Version:	1.0.7
Release:	1.%{?patchnumber}%{?dist}
Summary:	Erlang OTP GenServer Extension
Group:		Development/Libraries
License:	EPL
URL:		http://github.com/madrat-/pri_server.git
Source0:	%{upstream}-%{realname}-%{version}-g%{git_tag}.tar.bz2

BuildRequires:	erlang-erts
BuildRequires:	erlang-rebar

Requires:	erlang-erts%{?_isa}
Requires:	erlang-kernel%{?_isa}
Requires:	erlang-stdlib%{?_isa} >= 17
Provides:	%{realname} = %{version}-%{release}

%description
Erlang OTP GenServer Extension, which allows add priorities to call/cast events

%prep
%setup -q -n %{realname}

%build
export VSN=%{version}
make compile

%install
install -m 755 -p -d %{buildroot}%{_libdir}/erlang/lib/%{realname}-%{version}/ebin
install -m 755 -p -d %{buildroot}%{_libdir}/erlang/lib/%{realname}-%{version}/include
install -m 644 ebin/%{realname}.app %{buildroot}%{_libdir}/erlang/lib/%{realname}-%{version}/ebin
install -m 644 ebin/*.beam %{buildroot}%{_libdir}/erlang/lib/%{realname}-%{version}/ebin
install -m 644 include/*.hrl %{buildroot}%{_libdir}/erlang/lib/%{realname}-%{version}/include
#cp -a priv %{buildroot}%{_libdir}/erlang/lib/%{realname}-%{version}/

%check
make test

%files
%doc LICENSE README.md
%{_libdir}/erlang/lib/%{realname}-%{version}

%changelog
* Wed Dec 23 2015 Anatoly madRat L. Berenblit - 1.0.7
- erlang 17 full support 

* Wed Jul 15 2015 Anatoly madRat L. Berenblit - 1.0.5
- erlang R17 support 

* Mon Nov 3 2014 Anatoly madRat L. Berenblit - 1.0.1
- module for EUNIT testing

* Mon Nov 3 2014 Anatoly madRat L. Berenblit - 1.0.0
- Initial build
