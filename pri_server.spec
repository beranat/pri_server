%global realname pri_server
%global upstream madrat
%global debug_package %{nil}
%global git_tag 00000000
%global patchnumber 0

Name:		erlang-%{realname}
Version:	0.0.1
Release:	0.1%{?dist}
Summary:	Erlang OTP GenServer Extension
Group:		Development/Libraries
License:	EPL
URL:		https://github.com/xxx

Source0:	%{upstream}-%{realname}-%{version}-%{patchnumber}-g%{git_tag}.tar.bz2

BuildRequires:	erlang-erts
BuildRequires:	erlang-rebar

Requires:	erlang-erts%{?_isa}
Requires:	erlang-kernel%{?_isa}
Requires:	erlang-stdlib%{?_isa}
Provides:	%{realname} = %{version}-%{release}

%description
Erlang OTP GenServer Extension, which allows add priorities to call/cast events

%prep
%setup -q -n %{realname}

%build
rebar compile -v

%install
mkdir -p %{buildroot}%{_libdir}/erlang/lib/%{realname}-%{version}/ebin
#mkdir -p %{buildroot}%{_libdir}/erlang/lib/%{realname}-%{version}/include
install -m 644 ebin/%{realname}.app %{buildroot}%{_libdir}/erlang/lib/%{realname}-%{version}/ebin
install -m 644 ebin/*.beam %{buildroot}%{_libdir}/erlang/lib/%{realname}-%{version}/ebin
#install -m 644 include/*.hrl %{buildroot}%{_libdir}/erlang/lib/%{realname}-%{version}/include
#cp -a priv %{buildroot}%{_libdir}/erlang/lib/%{realname}-%{version}/

%check
rebar eunit -v

%files
%doc LICENSE README.md
#%{_bindir}/rebar
%{_libdir}/erlang/lib/%{realname}-%{version}

%changelog
* Tue Nov 3 2015 Anatoly madRat L. Berenblit - 1.0.0
- Initial build
