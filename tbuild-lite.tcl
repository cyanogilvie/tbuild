#!/usr/bin/env tclsh
# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

set dir	[pwd]

package require platform

proc readfile fn { #<<<
	set h	[open $fn r]
	try {read $h} finally {close $h}
}

#>>>
proc writefile {fn chars} { #<<<
	set h	[open $fn w]
	try {puts -nonewline $h $chars} finally {close $h}
}

#>>>
proc fullynormalize {fn} { #<<<
	set fqfn	[file normalize $fn]

	set patience	20
	set seen		{}
	while {[file type $fqfn] eq "link"} {
		set fqfn	[file normalize [file join [file dirname $fqfn] [file readlink $fqfn]]]
		if {[incr patience -1] <= 0} {
			error "Too many symlinks: $fn"
		}
		if {$fqfn in $seen} {
			error "Circular symlinks: $fn"
		}
	}

	return $fqfn
}

#>>>

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
	method init {} {
	}

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

		if {$ispkg || $isapp} {
			fail "Only building tms is supported in tbuild-lite"
		}
	}

	#>>>
	method build_tm {name args} { #<<<
		global projinfo
		my _load_projfile

		dict with projinfo tms $name {}

		set tm_data	""

		#append tm_data	[list package provide $name $version] "\n"

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
							foreach file [glob -nocomplain -type f $pattern] {
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
							foreach file [glob -nocomplain -type f $pattern] {
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
						return [readfile $fn]
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
						return [readfile $fn]
					}

					#>>>
				} $settings [list debs $name]
			}

			#>>>
			file {op args} { #<<<
				switch -- $op {
					tail -
					rootname -
					dirname -
					extension -
					join {file $op {*}$args}
					default {error "Unsupported file subcommand: $op"}
				}
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
}

actions init

if {[llength $argv] == 0} {
	set argv	[list "build"]
}

set actionargs	[lassign $argv action]
actions $action {*}$actionargs

