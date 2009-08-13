#!/usr/bin/env cfkit8.6

# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

set dir	[pwd]

package require cflib
package require platform

namespace eval cflib {
	namespace export *
}
namespace import cflib::*

proc fail {msg} { #<<<
	puts stderr $msg
	exit 1
}

#>>>
proc getkey {dict key args} { #<<<
	if {[llength $args] > 1} {
		throw {syntax} "Too many arguments, expecting dict key ?default?"
	}
	if {[dict exists $dict $key]} {
		return [dict get $dict $key]
	}
	if {[llength $args] == 1} {
		return [lindex $args 0]
	}
	set choices	[join [dict keys $dict] "\", \""]
	throw [list invalid_key $key] "Invalid key: \"$key\", choose from \"$choices\""
}

#>>>
proc decomment {in} { #<<<
	set out	""

	foreach line [split $in \n] {
		if {[string index [string trim $line] 0] eq "#"} continue
		append out	$line "\n"
	}

	return $out
}

#>>>
proc dsl_eval {interp dsl_commands dsl_script args} { #<<<
	set aliases_old	{}
	foreach {cmdname cmdargs cmdbody} [decomment $dsl_commands] {
		dict set aliases_old $cmdname [$interp alias $cmdname]

		$interp alias $cmdname apply [list $cmdargs $cmdbody] {*}$args
	}

	try {
		$interp eval $dsl_script
	} finally {
		dict for {cmdname oldalias} $aliases_old {
			$interp alias $cmdname $oldalias
		}
	}
}

#>>>

# Build tbuildconf <<<
set tbuildconf [dict create \
	projfile			tbuild.proj \
	tm_build			tm \
	repo_base			[file join $::env(HOME) .tbuild repo] \
	default_runtime		cfkit* \
	default_platform	linux-glibc2.3-ix86 \
]

foreach file [list \
		[file join / etc tbuild.conf] \
		[file join $env(HOME) .tbuild config] \
		[file join $dir .tbuild config] \
] {
	if {[file exists $file] && [file isreadable $file]} {
		set tbuildconf	[dict merge $tbuildconf [readfile $file]]
	}
}
# Build tbuildconf >>>

oo::object create actions
oo::objdefine actions {
	method build {name args} { #<<<
		global projinfo
		my _load_projfile

		if {$name eq "all"} {
			set targets {}

			foreach {category target} [my _targets] {
				my build $target {*}$args
			}
			return
		}

		set istm	[dict exists $projinfo tms $name]
		set ispkg	[dict exists $projinfo pkgs $name]
		set isapp	[dict exists $projinfo applications $name]

		if {!($istm || $ispkg || $isapp)} {
			fail "Target \"$name\" is not a tm, package or application"
		}

		if {$istm} {
			my _build_tm $name {*}$args
		}
		
		if {$ispkg} {
			puts "would build pkg \"$name\""
		}

		if {$isapp} {
			try {
				my _build_application $name {*}$args
			} on error {errmsg options} {
				puts stderr "Uncaught error building application \"$name\": [dict get $errorinfo]"
				exit 3
			}
		}
	}

	#>>>
	method _build_tm {name args} { #<<<
		global projinfo
		my _load_projfile

		dict with projinfo tms $name {}

		set tm_data	""

		append tm_data	[list package provide $name $version] "\n"

		if {[info exists requires]} {
			foreach req $requires {
				append tm_data [list package require {*}$req] "\n"
			}
		}

		if {[info exists init]} {
			append tm_data $init "\n"
		}

		foreach file $files {
			set file_data	[readfile $file]
			append tm_data $file_data
			if {[string index $file_data end] ne "\n"} {
				append tm_data "\n"
			}
		}

		set out_fn_base	[dict get $::tbuildconf tm_build]

		set target_platforms	[dict keys $platforms]
		if {[llength $target_platforms] == 0} {
			fail "No platforms specified"
		}

		foreach platform $target_platforms {
			set tm_name		${name}-$version.tm
			set out_fn_dir	[file dirname [file join $out_fn_base $platform $tm_name]]
			set out_fn	[file join $out_fn_dir [file tail ${name}-$version.tm]]

			if {![file exists $out_fn_dir]} {
				file mkdir $out_fn_dir
			}
			if {"-compress" in $args} {
				set oldsize	[string length $tm_data]
				set compressed [string map [list "\u001a" "\\u001a"] [zlib deflate $tm_data 3]]
				set tm_data	[list eval zlib inflate $compressed]
				set newsize	[string length $tm_data]
				puts "Compressed $tm_name $oldsize -> $newsize"
			}
			writefile $out_fn $tm_data
			puts "Wrote \"$out_fn\""
		}
	}

	#>>>
	method _build_application {name args} { #<<<
		global projinfo
		my _load_projfile

		set appsettings	[dict get $projinfo applications $name]

		puts stderr "building for \"$name\" platforms [dict keys [dict get $appsettings platforms]]"
		dict for {platform platformsettings} [dict get $appsettings platforms] {
			set before	[pwd]
			my _in_tmp_dir {
				try {
					file mkdir $name.vfs

					# main.tcl <<<
					set tokens	[dict create]
					if {[dict exists $appsettings choose_package]} {
						dict set tokens %choose_package% \
								[list [dict get $appsettings choose_package]]
					} else {
						dict set tokens %choose_package% [list [list return $name]]
					}

					set main.tcl_data [string map $tokens {
if {[catch {
	package require starkit
}]} {
	set have_starkit	0
} else {
	set have_starkit	1
}
package require platform

apply {
	{} {
		set here	[file dirname [info script]]
		foreach platform [platform::patterns [platform::identify]] {
			set pkgpath	[file join $here pkg $platform]
			set tmpath	[file join $here tm $platform]
			if {[file exists $pkgpath] && [file isdirectory $pkgpath]} {
				lappend ::auto_path $pkgpath
			}
			if {[file exists $tmpath] && [file isdirectory $tmpath]} {
				::tcl::tm::path add $tmpath
			}
		}
	}
}

if {$have_starkit && [starkit::startup] eq "sourced"} return

try {
	package require app-[apply {{argv} %choose_package%} $argv]
} on error {errmsg options} {
	puts stderr "$errmsg\n[dict get $options -errorinfo]"
}
}]

					writefile [file join $name.vfs main.tcl] [set main.tcl_data]
					# main.tcl >>>

					set app_base	[file join $name.vfs pkg tcl app-$name]
					file mkdir $app_base

					# _init.tcl <<<
					if {[dict exists $appsettings entrypoint]} {
						set entrypoint	[dict get $appsettings entrypoint]
					} else {
						set entrypoint	[lindex [dict get $appsettings files] 0]
					}
					if {$entrypoint ni [dict get $appsettings files]} {
						error "Invalid file specified as entrypoint: \"$entrypoint\""
					}

					set _init.tcl_data		{}
					set include_packages	{}
					foreach req [dict get $appsettings requires] {
						lappend _init.tcl_data	[list package require {*}$req]
						lappend include_packages $req
					}
					if {[dict exists $platformsettings requires]} {
						foreach req [dict get $platformsettings requires] {
							puts stderr "Adding specific platform require: ($req) for $platform"
							lappend _init.tcl_data	[list package require {*}$req]
							lappend include_packages $req
						}
					}
					lappend _init.tcl_data [list package provide app-$name [dict get $appsettings version]]
					lappend _init.tcl_data "source \[file join \[file dirname \[info script\]\] [list $entrypoint]]"

					writefile [file join $app_base _init.tcl] \
							[join [set _init.tcl_data] "\n"]
					# _init.tcl >>>

					# app pkgIndex.tcl <<<
					set pkgIndex.tcl_data	[string map [dict create \
							%name%		[list app-$name] \
							%version%	[list [dict get $appsettings version]] \
					] {package ifneeded %name% %version% [list source [file join $dir _init.tcl]]
					}]

					writefile [file join $app_base pkgIndex.tcl] [set pkgIndex.tcl_data]
					# app pkgIndex.tcl >>>

					# Copy specified application files <<<
					foreach file [dict get $appsettings files] {
						set src_fn	[file join $before $file]
						if {![file exists $src_fn]} {
							error "Specified file doesn't exist: \"$file\""
						}
						if {![file readable $src_fn]} {
							error "Specified file isn't readable: \"$file\""
						}
						set parts	[file split $file]
						if {[lindex $parts 0] eq "/"} {
							error "Absolute paths not allowed for files: \"$file\""
						}
						if {".." in $parts} {
							error "Cannot have \"..\" in file specification: \"$file\""
						}
						set file_dest	[file join $app_base $file]
						set file_dir	[file dirname $file_dest]
						if {![file exists $file_dir]} {
							file mkdir $file_dir
						}
						set file_contents	[readfile $src_fn binary]
						writefile $file_dest $file_contents binary
					}
					# Copy specified application files >>>

					if {[dict exists $appsettings runtime]} {
						set runtime		[dict get $appsettings runtime]
					} else {
						set runtime		[dict get $::tbuildconf default_runtime]
					}

					if {$platform eq "tcl"} {
						set runtime_platform	[dict get $::tbuildconf default_platform]
					} else {
						set runtime_platform	$platform
					}

					lassign [my _select_runtime $runtime $runtime_platform] \
							runtime_name \
							runtime_version \
							runtime_path \
							runtime_info
					set ::package_manifest [dict get $runtime_info builtin_packages]

					set compatible_platforms	[platform::patterns $platform]
					dict for {fn data} [my _resolve_packages $include_packages $compatible_platforms] {
						puts stderr "transcribing file ($fn)"
						set fqfn	[file join $name.vfs $fn]
						set dir		[file dirname $fqfn]
						if {![file exists $dir]} {
							file mkdir $dir
						}
						writefile $fqfn $data binary
					}

					unset ::package_manifest

					set out_app_base	[file join [dict get $::tbuildconf repo_base] apps $platform]
					if {![file exists $out_app_base]} {
						file mkdir $out_app_base
					}
					set app_name		[file join $out_app_base $name]
					set app_name_static	[file join $out_app_base ${name}-static]
					if {"starkit" in [dict get $runtime_info builtin_packages]} {
						puts "Writing application \"$app_name\""
						exec -- sdx wrap $app_name -vfs $name.vfs -interp [file tail $runtime_path]
						if {$platform ne "tcl"} {
							puts "Writing application \"$app_name_static\""
							exec -- sdx wrap $app_name_static -vfs $name.vfs -runtime $runtime_path
						}
					} elseif {"trofs" in [dict get $runtime_info builtin_packages]} {
						package require trofs

						set header	""
						append header	"#!/bin/sh\n"
						append header	"# \\\n"
						append header	[string map [list %r $runtime_name] {exec "%r" "$0" ${1+"$@"}}] "\n"
						append header \
{
package require trofs
set top		[trofs::mount [info script]]
set main	[file join $top main.tcl]
if {![file exists $main]} {
	puts stderr "No main.tcl"
	exit 1
}
source $main
}
						puts "Writing application \"$app_name\""
						cflib::writefile $app_name $header
						trofs::archive $name.vfs $app_name

						if {$::tcl_platform(platform) eq "unix"} {
							file attributes $app_name -permissions rwxr-xr-x
						}

						if {$platform ne "tcl"} {
							# TODO: merge $name.vfs with boot trofs
						}
					}
				} on error {errmsg options} {
					puts stderr "Uncaught error building \"$name\" for platform \"$platform\":\n[dict get $options -errorinfo]"
				}
			}
		}
	}

	#>>>
	method _select_runtime {runtime platform} { #<<<
		set candidates	{}
		foreach compatplat [platform::patterns $platform] {
			set runtime_base	[file join [dict get $::tbuildconf repo_base] runtimes $compatplat]
			foreach candidate [glob -nocomplain -type f [file join $runtime_base info *]] {
				set info	[readfile $candidate]
				set ext		[file extension $candidate]
				switch -- $ext {
					".exe"	{set candidate [file rootname $candidate]}
					default	{set ext	""}
				}
				set simplename	[file tail $candidate]
				if {[string match $runtime $simplename]} {
					lappend candidates [list \
							[file tail $candidate] \
							[dict get $info builtin_packages Tcl] \
							[file join $runtime_base bin $simplename]$ext \
							$info \
					]
				}
			}
		}

		if {[llength $candidates] == 0} {
			error "No suitable runtime found matching \"$runtime\" for platform \"$platform\""
		}

		set ordered	[lsort \
				-index 1 \
				-decreasing \
				-command [list package vcompare] \
				$candidates]

		return [lindex $ordered 0]
	}

	#>>>
	method _resolve_packages {required compatible_platforms} { #<<<
		set file_list	[dict create]
		foreach req $required {
			puts stderr "Resolving requirement: ($req)"
			set rest	[lassign $req pkgname]
			if {
				[dict exists $::package_manifest $pkgname] &&
				([llength $rest] == 0 ||
				[package vsatisfies [dict get $::package_manifest $pkgname] {*}$rest])
			} continue

			set available	[my _find_compatible $req $compatible_platforms]
			try {
				if {[llength $available] == 0} {
					throw {not_found} ""
				}
				foreach candidate $available {
					lassign $candidate ver type path

					if {$type eq "tm"} { #<<<
						set sub_requires	[my _extract_tm_requires $path]
						set sub_file_list	[my _resolve_packages $sub_requires $compatible_platforms]
						set file_list	[dict merge \
								$file_list \
								$sub_file_list \
						]
						set path_rel	[my _strip_base [dict get $::tbuildconf repo_base] $path]

						dict set file_list $path_rel [readfile $path binary]
						dict set ::package_manifest $pkgname $ver
						throw {found} ""
						#>>>
					} elseif {$type eq "pkg"} { #<<<
						foreach file [my _list_files_recursive $path] {
							puts "Addling file ($file)"
							set file_rel	[my _strip_base [dict get $::tbuildconf repo_base] $file]
							dict set file_list $file_rel [readfile $file binary]
						}
						dict set ::package_manifest $pkgname $ver
						throw {found} ""
						#>>>
					} else { #<<<
						error "Cannot deal with package type: \"$type\""
						#>>>
					}
				}
				throw {not_found} ""
			} trap {found} {} {
				continue
			} trap {not_found} {} {
				puts stderr "No suitable candidates found for requirement \"$req\""
				error "No suitable candidates found for requirement \"$req\""
			} on error {errmsg options} {
				puts stderr "Unexpected error resolving dep ($req):\n[dict get $options -errorinfo]"
				return -options $options $errmsg
			}
		}

		return $file_list
	}

	#>>>
	method _strip_base {base path} { #<<<
		set fqfn_base	[fullynormalize $base]
		set fqfn_path	[fullynormalize $path]
		set baselen	[string length $fqfn_base]
		set prefix	[string range $fqfn_path 0 $baselen-1]
		if {$prefix ne $fqfn_base} {
			puts "fqfn_base: ($fqfn_base)"
			puts "fqfn_path: ($fqfn_path)"
			error "\"$path\" isn't contained in \"$base\""
		}
		string range $fqfn_path $baselen+1 end
	}

	#>>>
	method _extract_tm_requires {path} { #<<<
		set requires	{}
		package require sugar
		foreach cmdraw [sugar::scriptToList [readfile $path]] {
			set cmd	{}
			foreach token $cmdraw {
				lassign $token type val

				if {$type eq "TOK"} {
					lappend cmd $val
				}
			}

			if {[lrange $cmd 0 1] eq [list package require]} {
				set rest	[lassign [lrange $cmd 2 end] name]
				puts stderr "detected requirement for \"$name\" \"$rest\" from \"$path\""
				lappend requires	[lrange $cmd 2 end]
			}
		}

		return $requires
	}

	#>>>
	method _list_files_recursive {base} { #<<<
		# TODO: defend against circular links
		set entries	{}

		foreach ent [glob -type f -nocomplain [file join $base *]] {
			lappend entries $ent
		}

		foreach ent [glob -type d -nocomplain [file join $base *]] {
			lappend entries {*}[my _list_files_recursive $ent]
		}

		return $entries
	}

	#>>>
	method _find_compatible {req compatible_platforms} { #<<<
		set available	{}

		set rest	[lassign $req pkgname]
		set pkgpath	[string map [list :: /] $pkgname]
		set tm_pkg		[file tail $pkgpath]
		set tm_prefix	[file dirname $pkgpath]

		set repo_base	[dict get $::tbuildconf repo_base]

		# Look for tm
		foreach plat $compatible_platforms {
			if {$tm_prefix ne "."} {
				set tm_path		[file join $repo_base tm $plat $tm_prefix]
			} else {
				set tm_path		[file join $repo_base tm $plat]
			}
			foreach match [glob -nocomplain -type f [file join $tm_path $tm_pkg-*.tm]] {
				if {![regexp {^([[:alpha:]][:[:alnum:]]*)-([[:digit:]].*)\.tm$} [file tail $match] -> name version]} continue
				if {
					[llength $rest] == 0 ||
					[package vsatisfies $version {*}$rest]
				} {
					lappend available [list $version tm $match]
				}
			}
		}

		# Look for package
		foreach plat $compatible_platforms {
			set pkg_path		[file join $repo_base pkg $plat]
			foreach pkgIndex [glob -nocomplain -type f [file join $pkg_path * pkgIndex.tcl]] {
				package require sugar
				foreach cmdraw [sugar::scriptToList [readfile $pkgIndex]] {
					set cmd	{}
					foreach token $cmdraw {
						lassign $token type val
						if {$type eq "TOK"} {
							lappend cmd $val
						}
					}

					if {[lrange $cmd 0 2] eq [list package ifneeded $pkgname]} {
						set offered_version	[lindex $cmd 3]
						if {
							[llength $rest] == 0 ||
							[package vsatisfies $offered_version {*}$rest]
						} {
							lappend available	[list $offered_version pkg [file dirname $pkgIndex]]
						}
					}
				}
			}
		}

		return [lsort -unique -decreasing -command [list apply {
			{a b} {
				set aver	[lindex $a 0]
				set bver	[lindex $b 0]
				set sort	[package vcompare $aver $bver]
				if {$sort == 0} {
					# As a version tie-breaker, prefer tm over pkg
					set atype	[lindex $a 1]
					set btype	[lindex $b 1]
					if {$atype eq $btype} {
						return 0
					} elseif {$atype eq "tm" && $btype ne "tm"} {
						return 1
					} elseif {$atype ne "tm" && $btype eq "tm"} {
						return -1
					}
				}
				return $sort
			}
		}] $available]
	}

	#>>>
	method _refresh_build {name} { #<<<
		# TODO: determine if target $name needs to be rebuilt
		my build $name
	}

	#>>>
	method clean {} { #<<<
	}

	#>>>
	method install {args} { #<<<
		global projinfo
		my _load_projfile

		if {[llength $args] == 0 || "all" in $args} {
			foreach {category target} [my _targets] {
				my _install_$category $target
			}
			return
		}

		foreach name $args {
			set istm	[dict exists $projinfo tms $name]
			set ispkg	[dict exists $projinfo pkgs $name]
			set isapp	[dict exists $projinfo applications $name]

			if {!($istm || $ispkg || $isapp)} {
				fail "No target called \"$name\""
			}

			if {$istm} {my _install_tm $name}
			if {$ispkg} {my _install_pkg $name}
			if {$isapp} {my _install_app $name}
		}
	}

	#>>>
	method _install_tm {name} { #<<<
		global projinfo

		my _refresh_build $name
		dict with projinfo tms $name {
			set src_fn_base	[dict get $::tbuildconf tm_build]
			set dst_fn_base	[file join [dict get $::tbuildconf repo_base] tm]

			set target_platforms	[dict keys $platforms]
			if {[llength $target_platforms] == 0} {
				fail "No platforms specified"
			}

			foreach platform $target_platforms {
				set tm_name	${name}-$version.tm
				set src_fn	[file join \
						$src_fn_base \
						$platform \
						$tm_name \
				]
				if {![file exists $src_fn]} {
					puts stderr "WARNING: Expecting \"$src_fn\" but it wasn't built"
					continue
				}

				set dst_fn_dir	[file dirname [file join $dst_fn_base $platform $tm_name]]
				if {![file exists $dst_fn_dir]} {
					file mkdir $dst_fn_dir
				}

				try {
					file copy -force $src_fn $dst_fn_dir
					puts "Installed tm $name in $dst_fn_dir"
				} on error {errmsg options} {
					fail "Error installing tm $name: $errmsg"
				}
			}
		}
	}

	#>>>
	method _install_pkg {name} { #<<<
		global projinfo

		dict with $projinfo 
	}

	#>>>
	method _install_app {name} { #<<<
	}

	#>>>
	method rpm {args} { #<<<
		package require rpm

		global projinfo
		my _load_projfile

		if {![dict exists $projinfo rpms]} {
			dict set projinfo rpms {}
		}

		if {[llength $args] == 0 || "all" in $args} {
			set args	[dict keys [dict get $projinfo rpms]]
		}

		set built_apps	[dict create]
		foreach name $args {
			set rpminfo	[dict get $projinfo rpms $name]
			dict set rpminfo name $name
			set runtimes	{}

			dict for {appname installpath} [dict get $rpminfo appfiles] {
				if {![dict exists $built_apps $appname]} {
					puts "----- building app ($appname) -----"
					my _build_application $appname
					dict set built_apps $appname 1
				}
				lappend runtimes	[dict get $projinfo applications $appname runtime]
			}
			set runtimes	[lsort -unique $runtimes]

			if {![dict exists $rpminfo target] || [dict size [dict get $rpminfo target]] == 0} {
				puts stderr "No targets defined for rpm \"$name\""
				exit 1
			}
			dict for {rpmtarget platform} [dict get $rpminfo target] {
				puts "building rpm \"$name\", rpmtarget: ($rpmtarget) platform: ($platform)"

				if {![dict exists $rpminfo version]} {
					error "Must define rpm version"
				}
				if {![dict exists $rpminfo summary]} {
					error "Require summary to be defined to build an rpm"
				}
				if {![dict exists $rpminfo description]} {
					error "Require description to be defined to build an rpm"
				}
				set reqs	[dict get $rpminfo requires]
				foreach runtime $runtimes {
					switch -glob -- $runtime {
						"*cfkit*" {
							lappend reqs	"cfkit"
						}

						"*kbskit*" {
							lappend reqs	"kbskit"
						}
					}
				}
				dict set rpminfo requires [lsort -unique $reqs]

				#if {[info exists rpm]} {unset rpm}
				#array set rpm $rpminfo
				#puts "rpm settings:"
				#parray rpm
				#unset rpm

				set files	{}
				set out_app_base	[file join [dict get $::tbuildconf repo_base] apps $platform]
				dict for {appname installpath} [dict get $rpminfo appfiles] {
					set app_src		[file join $out_app_base $appname]
					lappend files	$app_src $installpath
				}
				if {[dict exists $rpminfo files]} {
					foreach {pattern dst} [dict get $rpminfo files] {
						foreach match [glob -nocomplain $pattern] {
							if {[string index $dst end] eq "/"} {
								lappend files	$match [file join $dst [file tail $match]]
							} else {
								lappend files	$match $dst
							}
						}
					}
					#lappend files {*}[dict get $rpminfo files]
				}
				puts "files:\n[join $files \n]"
				try {
					rpm::make_rpm $rpminfo $files $rpmtarget
				} on error {errmsg options} {
					puts stderr "Error building rpm: $errmsg"
					cflib::writefile /tmp/failed.spec [rpm::build_spec $rpminfo $files]
					return -options $options $errmsg
				}
			}
		}
	}

	#>>>
	method remove {} { #<<<
	}

	#>>>
	method path {args} { #<<<
		set paths		{}

		# Apps
		set basepath	[file join [dict get $::tbuildconf repo_base] apps]
		foreach platform [platform::patterns [platform::identify]] {
			# TODO: figure out how to quote spaces and other nasties
			set path		[file join $basepath $platform]
			if {"-all" in $args || [file isdirectory $path]} {
				lappend paths	[file nativename $path]
			}
		}

		# Runtimes
		set basepath	[file join [dict get $::tbuildconf repo_base] runtimes]
		foreach platform [platform::patterns [platform::identify]] {
			# TODO: figure out how to quote spaces and other nasties
			set path		[file join $basepath $platform bin]
			if {"-all" in $args || [file isdirectory $path]} {
				lappend paths	[file nativename $path]
			}
		}

		puts [join $paths :]
	}

	#>>>
	method _load_projfile {} { #<<<
		global projinfo projinfo_loaded

		if {[info exists projinfo_loaded]} return

		set projfile	[getkey $::tbuildconf projfile]
		set conffile	[file join $::dir $projfile]

		if {![file exists $conffile]} {
			fail "Cannot find config file $projfile"
		}

		if {![file readable $conffile]} {
			fail "Cannot read config file $projfile"
		}

		set projinfo {
			tms		{}
			pkgs	{}
		}

		interp create -safe proj

		# TODO: Not safe - provide filtered versions of these
		interp expose proj file
		interp expose proj glob

		dsl_eval proj {
			tm {name script} { #<<<
				global projinfo

				dict set projinfo tms $name {
					platforms	{}
				}

				dsl_eval proj {
					version {cx ver} { #<<<
						dict set ::projinfo {*}$cx version $ver
					}

					#>>>
					summary {cx summary} { #<<<
						dict set ::projinfo {*}$cx summary $summary
					}

					#>>>
					description {cx desc} { #<<<
						dict set ::projinfo {*}$cx description $desc
					}

					#>>>
					requires {cx packagelist} { #<<<
						dict set ::projinfo {*}$cx requires $packagelist
					}

					#>>>
					init {cx script} { #<<<
						dict set ::projinfo {*}$cx init $script
					}

					#>>>
					files {cx filelist} { #<<<
						set resolvedfiles	{}
						foreach pattern $filelist {
							foreach file [glob -nocomplain -type file $pattern] {
								set fqfn	[fullynormalize $file]
								if {$fqfn ni $resolvedfiles} {
									lappend resolvedfiles $fqfn
								}
							}
						}
						dict set ::projinfo {*}$cx files $resolvedfiles
					}

					#>>>
					cfiles {cx filelist} { #<<<
						dict set ::projinfo {*}$cx cfiles $filelist
					}

					#>>>
					platform {cx platname {platform_specific_config {}}} { #<<<
						dict set ::projinfo {*}$cx platforms $platname [dict create]
						dsl_eval proj {
							requires {cx packagelist} { #<<<
								dict set ::projinfo {*}$cx requires $packagelist
							}

							#>>>
						} $platform_specific_config [concat $cx [list platforms $platname]]
					}

					#>>>
				} $script [list tms $name]
			}

			#>>>
			application {name script} { #<<<
				global projinfo

				dict set projinfo applications $name {
					platforms	{}
				}

				dsl_eval proj {
					runtime {cx runtime} { #<<<
						dict set ::projinfo {*}$cx runtime $runtime
					}

					#>>>
					version {cx ver} { #<<<
						dict set ::projinfo {*}$cx version $ver
					}

					#>>>
					version_of {pkg} { #<<<
						# TODO: find version of package $pkg
					}

					#>>>
					summary {cx summary} { #<<<
						dict set ::projinfo {*}$cx summary $summary
					}

					#>>>
					description {cx desc} { #<<<
						set trimmed	{}
						foreach line [string trim [split $desc \n]] {
							lappend trimmed	[string trim $line]
						}
						dict set ::projinfo {*}$cx description [join $trimmed \n]
					}

					#>>>
					requires {cx packagelist} { #<<<
						dict set ::projinfo {*}$cx requires $packagelist
					}

					#>>>
					init {cx script} { #<<<
						dict set ::projinfo {*}$cx init $script
					}

					#>>>
					files {cx filelist} { #<<<
						set resolvedfiles	{}
						foreach pattern $filelist {
							foreach file [glob -nocomplain -type file $pattern] {
								set fqfn	[fullynormalize $file]
								if {$fqfn ni $resolvedfiles} {
									#lappend resolvedfiles $fqfn
									lappend resolvedfiles $file
								}
							}
						}
						dict set ::projinfo {*}$cx files $resolvedfiles
					}

					#>>>
					cfiles {cx filelist} { #<<<
						dict set ::projinfo {*}$cx cfiles $filelist
					}

					#>>>
					platform {cx platname {platform_specific_config {}}} { #<<<
						dict set ::projinfo {*}$cx platforms $platname [dict create]
						dsl_eval proj {
							requires {cx packagelist} { #<<<
								dict set ::projinfo {*}$cx requires $packagelist
							}

							#>>>
						} $platform_specific_config [concat $cx [list platforms $platname]]
					}

					#>>>
					choose_package {cx script} { #<<<
						dict set ::projinfo {*}$cx choose_package $script
					}

					#>>>
				} $script [list applications $name]
			}

			#>>>
			pkg {name script} { #<<<
				global projinfo

				dict set projinfo pkgs $name {
					platforms	{}
				}

				dsl_eval proj {
					version {cx ver} { #<<<
						dict set ::projinfo {*}$cx version $ver
					}

					#>>>
					summary {cx summary} { #<<<
						dict set ::projinfo {*}$cx summary $summary
					}

					#>>>
					description {cx desc} { #<<<
						dict set ::projinfo {*}$cx description $desc
					}

					#>>>
					requires {cx packagelist} { #<<<
						dict set ::projinfo {*}$cx requires $packagelist
					}

					#>>>
					init {cx script} { #<<<
						dict set ::projinfo {*}$cx init $script
					}

					#>>>
					files {cx filelist} { #<<<
						dict set ::projinfo {*}$cx files $filelist
					}

					#>>>
					cfiles {cx filelist} { #<<<
						dict set ::projinfo {*}$cx cfiles $filelist
					}

					#>>>
					platform {cx platname {platform_specific_config {}}} { #<<<
						dict set ::projinfo {*}$cx platforms $platname [dict create]
						dsl_eval proj {
							requires {cx packagelist} { #<<<
								dict set ::projinfo {*}$cx requires $packagelist
							}

							#>>>
						} $platform_specific_config [concat $cx [list platforms $platname]]
					}

					#>>>
				} $script [list pkgs $name]
			}

			#>>>
			this_platform {} { #<<<
				return [platform::identify]
			}

			#>>>
			rpm {name settings} { #<<<
				dict set ::projinfo rpms $name {
					target			{}
					release			1
					license			"commercial"
					vendor			"Codeforge"
					group			"Applications/System"
					sourcetar		"tbuild_tmp_source.tar.gz"
					requires		{}
					post_scriptlet	""
					preun_scriptlet	""
					appfiles		{}
				}
				dsl_eval proj {
					version {cx version} { #<<<
						dict set ::projinfo {*}$cx version $version
					}

					#>>>
					summary {cx summary} { #<<<
						dict set ::projinfo {*}$cx summary $summary
					}

					#>>>
					description {cx desc} { #<<<
						set trimmed	{}
						foreach line [split [string trim $desc] \n] {
							lappend trimmed	[string trim $line]
						}
						dict set ::projinfo {*}$cx description [join $trimmed \n]
					}

					#>>>
					target {cx target platform} { #<<<
						dict set ::projinfo {*}$cx target $target $platform
					}

					#>>>
					release {cx rel} { #<<<
						dict set ::projinfo {*}$cx release $rel
					}

					#>>>
					license {cx lic} { #<<<
						dict set ::projinfo {*}$cx license $lic
					}

					#>>>
					vendor {cx vendor} { #<<<
						dict set ::projinfo {*}$cx vendor $vendor
					}

					#>>>
					group {cx group} { #<<<
						dict set ::projinfo {*}$cx group $group
					}

					#>>>
					rpmrequires {cx req} { #<<<
						dict set ::projinfo {*}$cx requires $req
					}

					#>>>
					applications {cx applist} { #<<<
						package require sugar
						set appfiles	{}
						foreach cmdraw [sugar::scriptToList $applist] {
							set cmd	{}
							foreach token $cmdraw {
								lassign $token type val

								if {$type eq "TOK"} {
									lappend cmd $val
								}
							}

							switch -- [llength $cmd] {
								0 {continue}

								1 {
									lappend cmd	[file join / usr bin [lindex $cmd 0]]
								}

								2 {}

								default {
									error "Invalid syntax for rpm application list: ($cmd)"
								}
							}

							lappend appfiles	{*}$cmd
						}
						dict set ::projinfo {*}$cx appfiles $appfiles
					}

					#>>>
					post_scriptlet {cx scr} { #<<<
						dict set ::projinfo {*}$cx post_scriptlet $scr
					}

					#>>>
					preun_scriptlet {cx scr} { #<<<
						dict set ::projinfo {*}$cx preun_scriptlet $scr
					}

					#>>>
					files {cx files} { #<<<
						dict set ::projinfo {*}$cx files $files
					}

					#>>>
					include {cx fn} { #<<<
						return [cflib::readfile $fn]
					}

					#>>>
				} $settings [list rpms $name]
			}

			#>>>
		} [readfile $conffile]

		interp delete proj

		set projinfo_loaded	1
	}

	#>>>
	method _targets {} { #<<<
		global projinfo
		my _load_projfile

		set targets	{}

		set catmap	{
			tms				tm
			pkgs			pkg
			applications	apps
		}

		foreach category {tms pkgs applications} {
			if {[dict exists $projinfo $category]} {
				foreach target [dict keys [dict get $projinfo $category]] {
					set cat	[dict get $catmap $category]
					lappend targets [list $cat $target]
				}
			}
		}

		return [concat {*}[lsort -unique $targets]]
	}

	#>>>
	method import_runtime {path platform} { #<<<
		package require platform

		if {![file exists $path]} {
			puts stderr "Invalid path: \"$path\""
			exit 1
		}

		# Special case: win32 runtime on a linux host that can run it with
		# wine (works - how to accomodate it?)
		if {$platform ni [platform::patterns [platform::identify]]} {
			puts stderr "Runtime is incompatible with this platform, please import it on a compatible platform and copy the imported version here"
			exit 1
		}

		# runtime is compatible with the platform we're running on, interrogate it

		lassign [chan pipe] readpipe writepipe

		set handle	[open [list |$path >@ $writepipe] w]
		close $writepipe
		chan configure $handle -blocking 0
		puts -nonewline $handle {
			catch {package require __no_such_package}
			set runtime_info	{}
			set builtin	{}
			foreach package [package names] {
				if {[catch {
					package require $package
				} res]} {
					set errmsg	$res
					puts stderr "Couldn't load package: \"$package\": $errmsg"
				} else {
					set ver	$res
					lappend builtin $package $ver
				}
			}
			lappend runtime_info builtin_packages $builtin
			puts -nonewline $runtime_info
			exit 0
		}
		chan configure $handle -blocking 1
		try {
			chan close $handle
		} trap {CHILDSTATUS} {errmsg options} {
			set exitstatus	[lindex [dict get $options -errorcode] 2]
			puts stderr "Interrogation failed ($exitstatus): $errmsg"
			exit 1
		} on ok {} {
			set output	[read $readpipe]
		} on error {errmsg options} {
			puts "WARNING: interrogation wrote to stderr:\n$errmsg"
			set output	[read $readpipe]
		} finally {
			chan close $readpipe
		}

		try {
			if {![dict exists $output builtin_packages]} {
				error ""
			}
		} on error {} {
			puts stderr "Could not parse interrogation result:\n$output"
			exit 1
		}

		set runtime_base	[file join \
				[dict get $::tbuildconf repo_base] \
				runtimes \
				$platform]
		set runtime_data	[readfile $path binary]
		set runtime_outfn	[file join $runtime_base bin [file tail $path]]
		set info_outfn		[file join $runtime_base info [file tail $path]]

		if {![file exists [file join $runtime_base bin]]} {
			file mkdir [file join $runtime_base bin]
		}
		if {![file exists [file join $runtime_base info]]} {
			file mkdir [file join $runtime_base info]
		}

		set attribs	[file attributes $path]
		writefile $runtime_outfn $runtime_data binary
		if {[dict exists $attribs -longname]} {
			dict unset attribs -longname
		}
		if {[dict exists $attribs -shortname]} {
			dict unset attribs -shortname
		}
		file attributes $runtime_outfn {*}$attribs
		writefile $info_outfn $output binary
		puts "Imported runtime \"$path\": \"[file normalize $info_outfn]\""
		exit 0
	}

	#>>>
	method _in_tmp_dir {script} { #<<<
		set oldpwd		[pwd]
		set tempfp	[file tempfile tmpdir]
		close $tempfp
		file delete $tmpdir
		file mkdir $tmpdir
		cd $tmpdir
		try {
			uplevel $script
		} on error {errmsg options} {
			dict incr options -level
			return -options $options $errmsg
		} finally {
			cd $oldpwd
			if {[info exists tmpdir] && [file exists $tmpdir]} {
				# TODO: paranoid checks
				file delete -force -- $tmpdir
			}
		}
	}

	#>>>
	method run {args} { #<<<
		package require platform
		set cfg	[cflib::config new $args [subst {
			variable platform	[list [dict get $::tbuildconf default_platform]]
			variable runtime	[list [dict get $::tbuildconf default_runtime]]
		}]]
		set args	[$cfg rest]
		set platform	[$cfg get platform]
		lassign [my _select_runtime [$cfg get runtime] $platform] \
				runtime_name \
				runtime_version \
				runtime_path \
				runtime_info
		set fp	[file tempfile launcherfn]
		chan puts $fp [string map [list %p% [list $platform] %b% [file normalize ~]] {
package require platform

foreach pattern [platform::patterns %p%] {
	set tmpath	[file normalize [file join %b% .tbuild repo tm $pattern]]
	if {[file isdirectory $tmpath]} {
		::tcl::tm::path add $tmpath
	}
	set pkgpath	[file normalize [file join %b% .tbuild repo pkg $pattern]]
	if {[file isdirectory $pkgpath]} {
		lappend ::auto_path	$pkgpath
	}
}

set argv	[lassign $argv app]
set argv0	$app
if {$app ne ""} {
	source $app
} else {
	set ::tcl_interactive	1
	while {1} {
		apply {
			{} {
				puts -nonewline "> "; flush stdout
				set cmd	[chan gets stdin]
				try {
					uplevel #0 $cmd
				} on error {errmsg options} {
					puts stderr $errmsg
				} on ok {res} {
					if {$res ne ""} {
						puts $res
					}
				}
			}
		}
	}
}
		}]
		chan close $fp
		set exitstatus	0
		try {
			exec $runtime_path $launcherfn {*}$args >@ stdout 2>@ stderr <@ stdin
		} trap {CHILDSTATUS} {errmsg options} {
			lassign [dict get $options -errorcode] code pid status
			set exitstatus	$status
		} finally {
			if {[file exists $launcherfn]} {
				file delete $launcherfn
			}
		}

		exit $exitstatus
	}

	#>>>
}

if {[llength $argv] == 0} {
	fail "No action specified"
}

set actionargs	[lassign $argv action]
actions $action {*}$actionargs

