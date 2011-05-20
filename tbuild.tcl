#!/usr/bin/env cfkit8.6
# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

if {[file system [info script]] eq "native"} {
	package require platform

	foreach platform [platform::patterns [platform::identify]] {
		set tm_path		[file join $env(HOME) .tbuild repo tm $platform]
		set pkg_path	[file join $env(HOME) .tbuild repo pkg $platform]
		if {[file exists $tm_path]} {
			tcl::tm::path add $tm_path
		}
		if {[file exists $pkg_path]} {
			lappend auto_path $pkg_path
		}
	}
}

# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

set dir	[pwd]

package require cflib
package require platform

namespace path [concat [namespace path] {
	::cflib
}]
interp alias {} readfile {} ::cflib::readfile

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
	app_build			app \
	repo_base			[file join $::env(HOME) .tbuild repo] \
	default_runtime		cfkit8.6 \
	default_platform	[platform::identify] \
	debug				0 \
	debupload {
		{debfile} {
			puts stderr "No upload script specified"
		}
	} \
	rpmupload {
		{rpmfile} {
			puts stderr "No upload script specified"
		}
	} \
]
foreach file [list \
		[file join / etc tbuild.conf] \
		[file join $env(HOME) .tbuild config] \
		[file join $dir .tbuild config] \
] {
	if {[file exists $file] && [file readable $file]} {
		set tbuildconf	[dict merge $tbuildconf [decomment [readfile $file]]]
	}
}
# Build tbuildconf >>>

if {[dict get $tbuildconf debug]} {
	proc ?? {script} {uplevel 1 $script}
} else {
	proc ?? {args} {}
}

oo::object create actions
oo::objdefine actions {
	method build {{name "all"} args} { #<<<
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
			my build_tm $name {*}$args
		}
		
		if {$ispkg} {
			puts "would build pkg \"$name\""
		}

		if {$isapp} {
			try {
				my build_application $name {*}$args
			} on error {errmsg options} {
				puts stderr "Uncaught error building application \"$name\": [dict get $errorinfo]"
				exit 3
			}
		}
	}

	#>>>
	method build_tm {name args} { #<<<
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
				set compressed [zlib deflate [encoding convertto utf-8 [string map [list "\u001a" "\\u001a"] $tm_data]] 3]
				set tm_data	"eval \[encoding convertfrom utf-8 \[zlib inflate [list $compressed]\]\]"
				set newsize	[string length $tm_data]
				puts "Compressed $tm_name $oldsize -> $newsize"
			}
			writefile $out_fn $tm_data
			puts "Wrote \"$out_fn\""
		}
	}

	#>>>
	method build_application {name args} { #<<<
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
#if {[catch {
#	package require starkit
#}]} {
#	set have_starkit	0
#} else {
#	set have_starkit	1
#}
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

#puts stderr "have_starkit: ($have_starkit)"
#if {$have_starkit} {
#	puts stderr "starkit::startup: [starkit::startup]"
#}
#if {$have_starkit && [starkit::startup] eq "sourced"} return

try {
	package require app-[apply {{argv} %choose_package%} $argv]
} on error {errmsg options} {
	puts stderr "$errmsg\n[dict get $options -errorinfo]"
	#puts stderr "auto_path:\n\t[join $::auto_path \n\t]"
	#puts stderr "tm path:\n\t[join [tcl::tm::path list] \n\t]"
} on ok {} {
	if {[info exists ::tbuild::app_res]} {
		return -options $::tbuild::app_options $::tbuild::app_res
	}
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
						#lappend _init.tcl_data	[list package require {*}$req]
						lappend include_packages $req
					}
					if {[dict exists $platformsettings requires]} {
						foreach req [dict get $platformsettings requires] {
							puts stderr "Adding specific platform require: ($req) for $platform"
							#lappend _init.tcl_data	[list package require {*}$req]
							lappend include_packages $req
						}
					}
					lappend _init.tcl_data [list package provide app-$name [dict get $appsettings version]]
					lappend _init.tcl_data [format {
						try {
							source [file join [file dirname [info script]] %s]
						} on ok {res options} {
							namespace eval ::tbuild [list variable app_res $res]
							namespace eval ::tbuild [list variable app_options $options]
						}
					} [list $entrypoint]]

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
						?? {puts "reading \"$src_fn\""}
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
					dict for {fn data} [my _resolve_packages $include_packages $compatible_platforms $name] {
						puts stderr "transcribing file ($fn) [string length $data] bytes"
						set fqfn	[file join $name.vfs $fn]
						set dir		[file dirname $fqfn]
						if {![file exists $dir]} {
							file mkdir $dir
						}
						writefile $fqfn $data binary
					}

					unset ::package_manifest

					set out_app_base	[file join $before [dict get $::tbuildconf app_build] $platform]
					if {![file exists $out_app_base]} {
						file mkdir $out_app_base
					}
					set app_name		[file join $out_app_base $name]
					set app_name_static	[file join $out_app_base ${name}-static]
					set fs		""
					switch -- [dict get $appsettings fs] {
						auto {
							if {"trofs" in [dict get $runtime_info builtin_packages]} {
								set fs	"trofs"
							} elseif {"starkit" in [dict get $runtime_info builtin_packages]} {
								set fs	"starkit"
							} elseif {"rozfs" in [dict get $runtime_info builtin_packages]} {
								set fs	"rozfs"
							} else {
								puts stderr "Need at least one of starkit, rozfs or trofs support in the runtime \"$runtime\""
								exit 2
							}
						}

						starkit {
							if {"starkit" ni [dict get $runtime_info builtin_packages]} {
								puts stderr "starkit requested, but selected runtime \"$runtime\" doesn't support it"
								exit 2
							}
							set fs	"starkit"
						}

						rozfs {
							if {"rozfs" ni [dict get $runtime_info builtin_packages]} {
								puts stderr "rozfs requested, but selected runtime \"$runtime\" doesn't support it"
								exit 2
							}
							set fs	"rozfs"
						}

						trofs {
							if {"trofs" ni [dict get $runtime_info builtin_packages]} {
								puts stderr "trofs requested, but selected runtime \"$runtime\" doesn't support it"
								exit 2
							}
							set fs	"trofs"
						}

						zipfs_inline {
							set fs	"zipfs_inline"
						}

						default {
							puts stderr "Invalid fs requested: \"[dict get $appsettings fs]\""
							exit 2
						}
					}
					puts "Building app using filesystem: \"$fs\" ([dict get $appsettings fs])"
					if {$fs eq "starkit"} {
						puts "Writing application \"$app_name\""
						exec -- sdx wrap $app_name -vfs $name.vfs -interp [file tail $runtime_path]
						if {$platform ne "tcl"} {
							puts "Writing application \"$app_name_static\""
							exec -- sdx wrap $app_name_static -vfs $name.vfs -runtime $runtime_path
						}
					} elseif {$fs eq "rozfs"} {
						package require rozfsutils

						set header	""
						append header	"#!/bin/sh\n"
						append header	"# \\\n"
						append header	[string map [list %r $runtime_name] {exec "%r" "$0" ${1+"$@"}}] "\n"
						append header \
{
package require rozfs
set top		[rozfs::mount [info script]]
set main	[file join $top main.tcl]
if {![file exists $main]} {
	puts stderr "No main.tcl"
	exit 1
}
source $main
}
						append header "\u001a"	;# ^Z

						puts "Writing application \"$app_name\""
						cflib::writefile $app_name $header
						set h	[open $app_name a]
						try {
							chan configure $h -translation binary -encoding binary
							chan seek $h 0 end
							chan puts -nonewline $h [rozfsutils::serialize $name.vfs]
						} finally {
							chan close $h
						}

						if {$::tcl_platform(platform) eq "unix"} {
							file attributes $app_name -permissions rwxr-xr-x
						}

						if {$platform ne "tcl"} {
							# TODO: merge $name.vfs with boot trofs
						}
					} elseif {$fs eq "trofs"} {
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
					} elseif {$fs eq "zipfs_inline"} {
						package require zip
						set zipdata	[apply {
							{basedir} {
								set old	[pwd]
								try {
									cd $basedir
									zip::mkzipdata -directory .
								} finally {
									cd $old
								}
							}
						} $name.vfs]
						set header	""
						append header	"#!/bin/sh\n"
						append header	"# \\\n"
						append header	[string map [list %r $runtime_name] {exec "%r" "$0" ${1+"$@"}}] "\n"
						append header	[format {
package require stringchan
package require vfs::zip 1.0.3.1
source [file join [apply {
	{} {
		set mountpoint	zip://[incr ::_zipfs_inline_seq]
		vfs::zip::MountChan [chan create read [stringchan new [binary decode base64 %s]]] $mountpoint -volume
		set mountpoint
	}
}] main.tcl]
} [binary encode base64 $zipdata]]
						cflib::writefile $app_name $header binary
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
					if {[dict get $info builtin_packages Tcl] eq ""} {
						puts stderr "Corrupt version number for $runtime Tcl"
						continue
					}
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

		lindex [lsort \
				-index 1 \
				-decreasing \
				-command {package vcompare} \
				$candidates] 0
	}

	#>>>
	method _resolve_packages {required compatible_platforms name} { #<<<
		set file_list	[dict create]
		foreach req $required {
			puts stderr "Resolving requirement: ($req)"
			set rest	[lassign $req pkgname]
			if {$pkgname eq "tbuild"} {
				global projinfo
				my _load_projfile

				dict set file_list tm/tcl/tbuild-1.0.tm [string map [dict create \
						%name%	[list $name] \
						%version%	[list [dict get $projinfo applications $name version]] \
				] {
					namespace eval tbuild {
						variable name		%name%
						variable version	%version%
					}
				}]
				continue
			}
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
						set sub_file_list	[my _resolve_packages $sub_requires $compatible_platforms $name]
						set file_list	[dict merge \
								$file_list \
								$sub_file_list \
						]
						set path_rel	[my _strip_base $path]

						?? {puts "slurping \"$pkgname\"\[$ver\] ($path) as ($path_rel)"}
						dict set file_list $path_rel [readfile $path binary]
						dict set ::package_manifest $pkgname $ver
						throw {found} ""
						#>>>
					} elseif {$type eq "pkg"} { #<<<
						foreach file [my _list_files_recursive $path] {
							puts "Addling file ($file)"
							set file_rel	[my _strip_base $file]
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
	method _strip_base {path} { #<<<
		set bases	[list [dict get $::tbuildconf repo_base] $::dir]
		foreach base $bases {
			set fqfn_base	[fullynormalize $base]
			set fqfn_path	[fullynormalize $path]
			set baselen	[string length $fqfn_base]
			set prefix	[string range $fqfn_path 0 $baselen-1]
			if {$prefix ne $fqfn_base} continue
			return [string range $fqfn_path $baselen+1 end]
		}
		error "\"$path\" isn't contained in any of $bases"
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
		#puts "Looking for \"$req\" for platforms: [join $compatible_platforms ", "]"
		foreach plat $compatible_platforms {
			foreach loc [list $::dir $repo_base] {
				if {$tm_prefix ne "."} {
					set tm_path		[file join $loc tm $plat $tm_prefix]
				} else {
					set tm_path		[file join $loc tm $plat]
				}
				#puts "Looking for \"[file join $tm_path $tm_pkg-*.tm]\""
				foreach match [glob -nocomplain -type f [file join $tm_path $tm_pkg-*.tm]] {
					#puts "\tFound \"$match\""
					if {![regexp {^([[:alpha:]][[:alnum:]_]*)-([[:digit:]].*)\.tm$} [file tail $match] -> name version]} continue
					#puts "\t-> name: ($name) version: ($version)"
					if {
						[llength $rest] == 0 ||
						[package vsatisfies $version {*}$rest]
					} {
						lappend available [list $version tm $match $loc]
					}
				}
			}
		}

		# Look for package
		foreach plat $compatible_platforms {
			foreach loc [list $::dir $repo_base] {
				set pkg_path		[file join $loc pkg $plat]
				foreach pkgIndex [glob -nocomplain -type f [file join $pkg_path * pkgIndex.tcl]] {
					?? {puts "Checking for $pkgname in $pkgIndex"}
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
							?? {puts "found candidate: $offered_version"}
							if {
								[llength $rest] == 0 ||
								[package vsatisfies $offered_version {*}$rest]
							} {
								lappend available	[list $offered_version pkg [file dirname $pkgIndex] $loc]
							} else {
								?? {puts "... doesn't satisfy requirement: $rest"}
							}
						}
					}
				}
			}
		}

		try {
		lsort -unique -decreasing -command [list apply {
			{a b} {
				set aver	[lindex $a 0]
				set bver	[lindex $b 0]
				set sort	[package vcompare $aver $bver]
				if {$sort == 0} {
					# As a version tie-breaker, prefer tm over pkg
					set atype	[lindex $a 1]
					set btype	[lindex $b 1]
					if {$atype eq $btype} {
						# Prefer local to packages installed in the repo
						set aloc	[expr {[lindex $a 3] eq $::dir}]
						set bloc	[expr {[lindex $b 3] eq $::dir}]
						if {$aloc == $bloc} {
							return [string compare [lindex $a 2] [lindex $b 2]]
						} elseif {$aloc} {
							return 1
						} else {
							return -1
						}
					} elseif {$atype eq "tm" && $btype ne "tm"} {
						return 1
					} elseif {$atype ne "tm" && $btype eq "tm"} {
						return -1
					}
				}
				return $sort
			}
		}] $available
		} on ok {res} {
			?? {puts "returning:\n\t[join $res \n\t]"}
			set res
		}
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

		set allow_tm	1
		set allow_pkg	1
		set allow_app	1

		foreach name $args {
			if {[string index $name 0] eq "-"} {
				switch -- $name {
					-no_app	{set allow_app 0}
					-no_tm	{set allow_tm 0}
					-no_pkg	{set allow_pkg 0}
					default {
						puts stderr "Invalid switch \"$name\""
					}
				}
				continue
			}
			if {$allow_tm} {
				set istm	[dict exists $projinfo tms $name]
			} else {
				set istm	0
			}
			if {$allow_pkg} {
				set ispkg	[dict exists $projinfo pkgs $name]
			} else {
				set ispkg	0
			}
			if {$allow_app} {
				set isapp	[dict exists $projinfo applications $name]
			} else {
				set isapp	0
			}

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
		global projinfo

		my _refresh_build $name
		dict with projinfo applications $name {
			set src_fn_base	[dict get $::tbuildconf app_build]
			set dst_fn_base	[file join [dict get $::tbuildconf repo_base] apps]

			set target_platforms	[dict keys $platforms]
			if {[llength $target_platforms] == 0} {
				fail "No platforms specified"
			}

			foreach platform $target_platforms {
				set src_fn	[file join \
						$src_fn_base \
						$platform \
						$name \
				]
				if {![file exists $src_fn]} {
					puts stderr "WARNING: Expecting \"$src_fn\" but it wasn't built"
					continue
				}

				if {[dict exists $platforms $platform install]} {
					set dst_fn	[dict get $platforms $platform install]
					if {[string index $dst_fn end] eq "/"} {
						set dst_fn	[file join $dst_fn $name]
					}
				} else {
					set dst_fn	[file join $dst_fn_base $platform $name]
				}
				set dst_fn_dir	[file dirname $dst_fn]

				if {![file exists $dst_fn_dir]} {
					file mkdir $dst_fn_dir
				}

				try {
					file copy -force $src_fn $dst_fn
					puts "Installed app $name as $dst_fn"
				} on error {errmsg options} {
					fail "Error installing tm $name: $errmsg"
				}
			}
		}
	}

	#>>>
	method rpm {args} { #<<<
		package require rpm 0.2

		global projinfo
		my _load_projfile

		if {![dict exists $projinfo rpms]} {
			dict set projinfo rpms {}
		}

		set upload	0
		if {$args eq {-upload}} {
			set upload	1
			set args	{}
		}
		if {[llength $args] == 0 || "all" in $args} {
			set args	[dict keys [dict get $projinfo rpms]]
		}

		set built_apps	[dict create]
		foreach name $args {
			if {$name eq "-upload"} {
				set upload	1
				continue
			}
			set rpminfo	[dict get $projinfo rpms $name]
			dict set rpminfo name $name
			set runtimes	{}

			dict for {appname installpath} [dict get $rpminfo appfiles] {
				if {![dict exists $built_apps $appname]} {
					puts "----- building app ($appname) -----"
					my build_application $appname
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
				#set out_app_base	[file join [dict get $::tbuildconf repo_base] apps $platform]
				set out_app_base	[file join [dict get $::tbuildconf app_build] $platform]
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
				puts "files:"
				foreach {s d} $files {
					puts "$s -> $d"
				}
				try {
					set outfiles	[rpm::make_rpm $rpminfo $files $rpmtarget]
				} on error {errmsg options} {
					puts stderr "Error building rpm: $errmsg"
					cflib::writefile /tmp/failed.spec [rpm::build_spec $rpminfo $files]
					return -options $options $errmsg
				} on ok {} {
					if {$upload} {
						my _rpmupload $outfiles
					}
				}
			}
		}
	}

	#>>>
	method deb {args} { #<<<
		package require deb 0.3

		global projinfo
		my _load_projfile

		if {![dict exists $projinfo debs]} {
			dict set projinfo debs {}
		}

		set upload	0
		if {$args eq {-upload}} {
			set upload	1
			set args	{}
		}
		if {[llength $args] == 0 || "all" in $args} {
			set args	[dict keys [dict get $projinfo debs]]
		}

		set ::deb::debug	1
		set built_apps	[dict create]
		foreach name $args {
			if {$name eq "-upload"} {
				set upload	1
				continue
			}
			set debinfo	[dict get $projinfo debs $name]
			set debsettings	[dict create]
			dict set debinfo name $name
			dict set debsettings %PACKAGE_NAME% $name
			set runtimes	{}

			if {[dict exists $debinfo postinst]} {
				dict set debsettings %POSTINST% [dict get $debinfo postinst]
			}

			if {[dict exists $debinfo prerm]} {
				dict set debsettings %PRERM% [dict get $debinfo prerm]
			}

			dict for {appname installpath} [dict get $debinfo appfiles] {
				if {![dict exists $built_apps $appname]} {
					puts "----- building app ($appname) -----"
					my build_application $appname
					dict set built_apps $appname 1
				}
				lappend runtimes	[dict get $projinfo applications $appname runtime]
			}
			set runtimes	[lsort -unique $runtimes]

			if {![dict exists $debinfo target] || [dict size [dict get $debinfo target]] == 0} {
				puts stderr "No targets defined for deb \"$name\""
				exit 1
			}
			try {
				string trim [exec dpkg --print-architecture]
			} on ok {myarch} {}
			dict for {debtarget platform} [dict get $debinfo target] {
				if {$debtarget ne $myarch && $debtarget ne "all"} continue
				puts "building deb \"$name\", debtarget: ($debtarget) platform: ($platform)"

				if {![dict exists $debinfo version]} {
					error "Must define deb version"
				}
				dict set debsettings %VERSION% [dict get $debinfo version]
				if {[dict exists $debinfo revision]} {
					dict set debsettings %REVISION% [dict get $debinfo revision]
				}
				if {![dict exists $debinfo summary]} {
					error "Require summary to be defined to build an deb"
				}
				dict set debsettings %SUMMARY% [dict get $debinfo summary]
				if {![dict exists $debinfo description]} {
					error "Require description to be defined to build an deb"
				}
				dict set debsettings %DESCRIPTION% [dict get $debinfo description]
				set reqs	[dict get $debinfo requires]
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
				dict set debinfo requires [lsort -unique $reqs]
				dict set debsettings %DEPENDS% [dict get $debinfo requires]

				#if {[info exists deb]} {unset deb}
				#array set deb $debinfo
				#puts "deb settings:"
				#parray deb
				#unset deb

				set files	{}
				#set out_app_base	[file join [dict get $::tbuildconf repo_base] apps $platform]
				set out_app_base	[file join [dict get $::tbuildconf app_build] $platform]
				dict for {appname installpath} [dict get $debinfo appfiles] {
					set app_src		[file join $out_app_base $appname]
					lappend files	$app_src $installpath
				}
				if {[dict exists $debinfo files]} {
					foreach {pattern dst} [dict get $debinfo files] {
						foreach match [glob -nocomplain $pattern] {
							if {[string index $dst end] eq "/"} {
								lappend files	$match [file join $dst [file tail $match]]
							} else {
								lappend files	$match $dst
							}
						}
					}
					#lappend files {*}[dict get $debinfo files]
				}
				puts "files:"
				set file_data	[dict create]
				foreach {s d} [my _expand_files $files] {
					puts "$s -> $d"
					dict set file_data	$d perms [file attributes $s -permissions]
					dict set file_data	$d data [cflib::readfile $s binary]
				}
				try {
					deb::make_deb $debsettings $file_data
				} on error {errmsg options} {
					puts stderr "Error building deb: $errmsg"
					return -options $options $errmsg
				} on ok {deboutput} {
					lassign $deboutput deb_fn deb_data
					if {![file exists debout]} {
						file mkdir debout
					}
					cflib::writefile [file join debout $deb_fn] $deb_data binary
					puts "Wrote [file join debout $deb_fn]"
					if {$upload} {
						puts "Attempting to upload [file join debout $deb_fn]"
						my _debupload [file join debout $deb_fn]
					} else {
						puts "Not uploading"
					}
				}
			}
		}
	}

	#>>>
	method _expand_file {s d} { #<<<
		set results	{}
		if {[file type $s] eq "directory"} {
			foreach child [glob -nocomplain [file join $s *]] {
				set tail	[file tail $child]
				lappend results {*}[my _expand_file $child [file join $d $tail]]
			}
		} else {
			lappend results	$s $d
		}
		set results
	}

	#>>>
	method _expand_files {files} { #<<<
		set expanded_files	{}
		foreach {s d} $files {
			lappend expanded_files	{*}[my _expand_file $s $d]
		}
		set expanded_files
	}

	#>>>
	method _rpmupload {fn} { #<<<
		set script	[getkey $::tbuildconf rpmupload]
		apply $script $fn
	}

	#>>>
	method _debupload {fn} { #<<<
		set script	[getkey $::tbuildconf debupload]
		apply $script $fn
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
		#interp expose proj file
		interp expose proj glob

		dsl_eval proj {
			tm {name script} { #<<<
				global projinfo

				dict set projinfo tms $name {
					platforms	{}
					build_commands	{}
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
					build {cx shellscript} { #<<<
						set cmds	[dict get $::projinfo {*}$cx build_commands]
						lappend cmds $shellscript
						dict set ::projinfo {*}$cx build_commands $cmds
						try {
							exec /bin/sh -c $shellscript
						} on error {errmsg} {
							puts stderr "Error running buildscript: $errmsg\n$cmd"
							exit 1
						}
					}
					#>>>
				} $script [list tms $name]
			}

			#>>>
			application {name script} { #<<<
				global projinfo

				dict set projinfo applications $name {
					platforms	{}
					fs			{auto}
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
							install {cx install_dest} { #<<<
								dict set ::projinfo {*}$cx install $install_dest
							}

							#>>>
						} $platform_specific_config [concat $cx [list platforms $platname]]
					}

					#>>>
					choose_package {cx script} { #<<<
						dict set ::projinfo {*}$cx choose_package $script
					}

					#>>>
					fs {cx fs} { #<<<
						dict set ::projinfo {*}$cx fs $fs
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
			deb {name settings} { #<<<
				dict set ::projinfo debs $name {
					target			{}
					release			1
					license			"commercial"
					vendor			"Codeforge"
					section			"utils"
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
					debrequires {cx req} { #<<<
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
									error "Invalid syntax for deb application list: ($cmd)"
								}
							}

							lappend appfiles	{*}$cmd
						}
						dict set ::projinfo {*}$cx appfiles $appfiles
					}

					#>>>
					postinst {cx scr} { #<<<
						dict set ::projinfo {*}$cx postinst $scr
					}

					#>>>
					prerm {cx scr} { #<<<
						dict set ::projinfo {*}$cx prerm $scr
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
				} $settings [list debs $name]
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
			applications	app
		}

		foreach category {tms pkgs applications} {
			if {[dict exists $projinfo $category]} {
				foreach target [dict keys [dict get $projinfo $category]] {
					set cat	[dict get $catmap $category]
					lappend targets [list $cat $target]
				}
			}
		}

		set tms		{}
		set pkgs	{}
		set apps	{}
		foreach {cat target} [concat {*}[lsort -unique $targets]] {
			switch -- $cat {
				tm	{lappend tms	$cat $target}
				pkg	{lappend pkgs	$cat $target}
				app	{lappend apps	$cat $target}
			}
		}
		return [concat $tms $pkgs $apps]
	}

	#>>>
	method import_runtime {path platform} { #<<<
		package require platform

		if {![file exists $path]} {
			puts stderr "Invalid path: \"$path\" pwd: ([pwd])"
			exit 1
		}
		set fqpath	[file normalize $path]

		# Special case: win32 runtime on a linux host that can run it with
		# wine (works - how to accomodate it?)
		if {$platform ni [platform::patterns [platform::identify]]} {
			puts stderr "Runtime is incompatible with this platform, please import it on a compatible platform and copy the imported version here"
			exit 1
		}

		# runtime is compatible with the platform we're running on, interrogate it

		lassign [chan pipe] readpipe writepipe

		set handle	[open [list |$fqpath >@ $writepipe] w]
		close $writepipe
		chan configure $handle -blocking 0
		puts -nonewline $handle {
			catch {package require __no_such_package}
			set runtime_info	{}
			set builtin	{}
			foreach package [package names] {
				try {
					lindex [lsort -command {package vcompare} [package versions $package]] end
				} on error {errmsg options} {
					puts stderr "Couldn't load package: \"$package\": $errmsg"
				} on ok {ver} {
					if {$ver eq ""} {
						set ver	[package require $package]
					}
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
			variable runprefix	{}
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
		set runprefix	[$cfg get runprefix]
		try {
			exec {*}$runprefix $runtime_path $launcherfn {*}$args >@ stdout 2>@ stderr <@ stdin
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
	method _os {} { #<<<
		set build	[string tolower $::tcl_platform(os)]
		switch -- $::tcl_platform(platform) {
			unix {
				try {
					exec lsb_release --id
				} on ok {output} {
					set idx	[string first ":" $output]
					if {$idx != -1} {
						set distribution	[string range $output $idx+1 end]
						set distribution	[string tolower [string trim $distribution]]
					}
				} trap {POSIX ENOENT} {} {}
				if {![info exists distribution]} {
					if {[file exists /etc/SuSE-release]} {
						set distribution	"suse"
					} elseif {[file exists /etc/redhat-release]} {
						set distribution	"redhat"
					} elseif {[file exists /etc/fedora-release]} {
						set distribution	"fedora"
					} elseif {[file exists /etc/debian-release]} {
						set distribution	"debian"
					} elseif {[file exists /etc/gentoo-release]} {
						set distribution	"gentoo"
					} elseif {[file exists /etc/slackware-release]} {
						set distribution	"slackware"
					} else {
						set distribution	"unknown"
					}
				}

				append build "_" $distribution
				return $build
			}

			windows {
				return $build
			}
		}
	}

	#>>>
	method os {} { #<<<
		puts [my _os]
	}

	#>>>
	method setup_rpm_build_environment {} { #<<<
		package require rpm

		try {
			exec rpm --version
		} trap {POSIX ENOENT} {errmsg options} {
			puts "RPM isn't installed, installing"
			switch -- [my _os] {
				linux_ubuntu - linux_debian {
					try {
						exec -ignorestderr sudo apt-get install rpm >@ stdout
					} on error {errmsg options} {
						puts stderr "Couldn't install RPM: $errmsg"
						return
					}
				}

				windows {
					puts stderr "Cannot build RPMS on windows"
					return
				}

				linux_suse - linux_redhat {
					puts stderr "RPM not installed on an RPM based distro?  Weird man"
					return
				}

				default {
					puts stderr "Don't know how to install RPM for platform \"[my _os]\""
					return
				}
			}
		}

		set dirs		{}
		foreach dir [list \
				[rpm::rpm --eval %_sourcedir] \
				[rpm::rpm --eval %_specdir] \
				[rpm::rpm --eval %_srcrpmdir] \
				[rpm::rpm --eval %_rpmdir] \
				[rpm::rpm --eval %_builddir] \
		] {
			if {![file writable $dir]} {
				lappend dirs $dir
			}
		}

		if {[llength $dirs] == 0} {
			puts "Permissions look ok already"
			return
		}

		puts "We need to change the ownership of the following system directories"
		puts "and their contents so that they are writable by the current"
		puts "user ($::env(USER)):"
		puts "\t[join $dirs \n\t]"
		try {
			exec sudo chown -R $::env(USER) {*}$dirs 2>@1
		} trap {CHILDSTATUS} {errmsg options} {
			lassign [dict get $options -errorcode] ecode pid code
			puts stderr "Could not set permissions: $errmsg"
		} on ok {output} {
			puts "Ownership set"
		}
	}

	#>>>
}

if {[llength $argv] == 0} {
	set argv	[list "build"]
}

set actionargs	[lassign $argv action]
actions $action {*}$actionargs

