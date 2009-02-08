#!/usr/bin/env kbskit8.6

# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

set dir	[pwd]

package require cflib
package require platform

namespace path ::cflib

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
	projfile	tbuild.proj \
	tm_build	tm \
	repo_base	[file join $::env(HOME) .tbuild repo] \
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

		namespace path ::cflib

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
			dict with projinfo tms $name {
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
		}
		
		if {$ispkg} {
			puts "would build pkg \"$name\""
		}

		if {$isapp} {
			puts "would build application \"$name\""
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
	method remove {} { #<<<
	}

	#>>>
	method path {args} { #<<<
		set paths		{}
		set basepath	[file join [dict get $::tbuildconf repo_base] apps]
		foreach platform [platform::patterns [platform::identify]] {
			# TODO: figure out how to quote spaces and other nasties
			set path		[file join $basepath $platform]
			if {"-all" in $args || [file isdirectory $path]} {
				lappend paths	[file nativename $path]
			}
		}

		puts [join $paths :]
	}

	#>>>
	method _load_projfile {} { #<<<
		global projinfo projinfo_loaded
		namespace path ::cflib

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
}

if {[llength $argv] == 0} {
	fail "No action specified"
}

set actionargs	[lassign $argv action]
actions $action {*}$actionargs

