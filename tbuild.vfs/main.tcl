package require starkit
package require platform

apply {
	{} {
		set here	[file dirname [info script]]
		foreach platform [platform::patterns [platform::identify]] {
			set pkgpath	[file join $here lib $platform]
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

if {[starkit::startup] eq "sourced"} return

try {
	package require [apply {
		{argv} {
			return "tbuild"
		}
	} $argv]
} on error {errmsg options} {
	puts stderr "$errmsg\n[dict get $options -errorinfo]"
}

