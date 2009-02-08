# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

oo::class create cflib::pclass {
	superclass oo::class

	constructor {def} { #<<<
		my variable superclass_seen
		my pclass_config {}		;# ensure defaults are set

		set superclass_seen	0

		foreach m {
			superclass
			property
			protected_property
			constructor
			method
			destructor
			pclass_config
		} {
			interp alias {} [self namespace]::$m {} [self] $m
		}
		foreach m {
			mixin
			filter
			unexport
			export
			variable
		} {
			interp alias {} [self namespace]::$m {} oo::define [self] $m
		}

		my eval $def

		if {!($superclass_seen)} {
			if {[self] ne "::cflib::pclassbase"} {
				#puts stderr "[self] forcing superclass cflib::pclassbase"
				oo::define [self] superclass cflib::pclassbase
			}
		}
	}

	#>>>
	method _get_prop_var {} { #<<<
		set othervar	[self]::_props
		if {![namespace exists [self]]} {
			namespace eval [self] {}
		}
		return $othervar
	}

	#>>>
	method _provides_baseclass {baseclass class} { #<<<
		if {$class eq $baseclass} {return 1}
		foreach superclass [info class superclasses $class] {
			if {[my _provides_baseclass $baseclass $superclass]} {
				return 1
			}
		}
		return 0
	}

	#>>>
	method superclass {args} { #<<<
		my variable superclass_seen
		set superclass_seen	1
		set seenhere	0
		set baseclass	"cflib::pclassbase"
		foreach superclass $args {
			if {[my _provides_baseclass $baseclass $superclass]} {
				set seenhere	1
				break
			}
		}
		if {!($seenhere)} {
			set args	[concat [list $baseclass] $args]
			#lappend args $baseclass
		}
		#puts stderr "[self] spliced in superclass cflib::pclassbase: ($args)"
		oo::define [self] superclass {*}$args
	}

	#>>>
	method property {name args} { #<<<
		lassign $args initval change_handler
		set othervar	[my _get_prop_var]
		#puts "setting property $name on [self] ($othervar)"
		dict set $othervar $name	[dict create protection public]
		if {[llength $args] >= 1} {
			dict set $othervar $name initval $initval
		}
		if {[llength $args] >= 2} {
			dict set $othervar $name change_handler $change_handler
		}
	}

	#>>>
	method protected_property {name args} { #<<<
		lassign $args initval change_handler
		set othervar	[my _get_prop_var]
		dict set $othervar $name	[dict create protection protected]
		if {[llength $args] >= 1} {
			dict set $othervar $name initval $initval
		}
		if {[llength $args] >= 2} {
			dict set $othervar $name change_handler $change_handler
		}
	}

	#>>>
	method constructor {args body} { #<<<
		my variable cfg
		set othervar	[my _get_prop_var]
		upvar $othervar props

		set newbody {}
		if {[dict get $cfg constructor_auto_next]} {
			append newbody {
				if {[self next] ne {}} {next}
			}
		}
		append newbody {
			if {[info exists [my varname _props]]} {
				dict for {k inf} [set [my varname _props]] {
					my variable $k
				}
				if {[info exists k]} {unset k}
				if {[info exists inf]} {unset inf}
			}
		}
		append newbody $body

		oo::define [self] constructor $args $newbody
	}

	#>>>
	method method {name args body} { #<<<
		set newbody	{
			if {[info exists [my varname _props]]} {
				dict for {k inf} [set [my varname _props]] {
					my variable $k
				}
				if {[info exists k]} {unset k}
				if {[info exists inf]} {unset inf}
			}
		}
		append newbody $body
		oo::define [self] method $name $args $newbody
	}

	#>>>
	method destructor {body} { #<<<
		set newbody	{
			if {[info exists [my varname _props]]} {
				dict for {k inf} [set [my varname _props]] {
					my variable $k
				}
				if {[info exists k]} {unset k}
				if {[info exists inf]} {unset inf}
			}
		}
		append newbody $body {
			if {[self next] ne {}} {next}
		}
		oo::define [self] destructor $newbody
	}

	#>>>
	method pclass_config {config} { #<<<
		my variable cfg
		if {![info exists cfg]} {
			set cfg	{}
		}
		set cfg [dict merge {
			constructor_auto_next	1
		} $cfg $config]
	}

	#>>>
}


cflib::pclass create cflib::pclassbase {
	constructor {} { #<<<
		#puts "in cflib::pclassbase::constructor for [self]"
		my variable _props
		if {![info exists _props]} {
			#puts "initalizing _props"
			set _props	[dict create]
		}
		my _mixin_props	[info object class [self]]
		if {[info exists [my varname _props]]} {
			dict for {k inf} [set [my varname _props]] {
				my variable $k
				if {[dict exists $inf initval]} {
					set $k [dict get $inf initval]
				}
			}
			if {[info exists k]} {unset k}
			if {[info exists inf]} {unset inf}
		}
	}

	#>>>
	method cget {name args} { #<<<
		if {[llength $args] > 1} {
			error "Too many arguments, expecting name ?default_value?"
		}
		my variable _props
		if {[dict exists $_props $name] && [dict get $_props $name protection] eq "public"} {
			my variable $name
			if {[info exists $name]} {
				return [set $name]
			} elseif {[llength $args] > 0} {
				return [lindex $args 0
			}
		} else {
			error "Invalid property \"$name\""
		}
	}

	#>>>
	method configure {args} { #<<<
		if {[llength $args] == 0} return
		my variable _props
		if {![info exists _props]} {
			error "Can't run configure on [self]: _props ([my varname _props]) is missing"
		}
		dict for {k v} $args {
			if {[string index $k 0] ne "-"} {
				throw {SYNTAX GENERAL} "Invalid property name \"$k\""
			}
			set k	[string range $k 1 end]
			if {![dict exists $_props $k]} {
				throw [list SYNTAX PROPERTY_NOTDEFINED -$k] \
						"Invalid property: \"$k\", expecting one of \"[join [dict keys $_props] \",\ \"]\""
			}
			if {[dict get $_props $k protection] ne "public"} {
				throw [list PROTECTION $k] "Property \"$k\" is not public"
			}
			set fqvar	[self namespace]::$k
			if {[info exists $fqvar]} {
				set oldval	[set $fqvar]
			}
			set $fqvar	$v
			if {[dict exists $_props $k change_handler]} {
				try {
					namespace inscope [self namespace] [list my [dict get $_props $k change_handler]]
				} trap {PROPERTY ABORT_CHANGE} {} {
					if {[info exists oldval]} {
						set $fqvar	$oldval
					}
				} on error {errmsg options} {
					if {[info exists oldval]} {
						set $fqvar	$oldval
					}
					dict incr options -level
					return -options $options $errmsg
				}
			}
		}
	}

	#>>>
	method _mixin_props {fromclass} { #<<<
		#puts "_mixin_props on [self], merging ($fromclass)"
		my variable _props
		if {![info exists _props]} {
			set _props	[dict create]
		}
		if {[info exists ${fromclass}::_props]} {
			dict for {k v} [set ${fromclass}::_props] {
				if {![dict exists $_props $k]} {
					dict set _props $k $v
				}
			}
		}
		set superclasses	[info class superclasses $fromclass]
		foreach superclass $superclasses {
			my _mixin_props $superclass
		}
	}

	#>>>

	# convenience methods
	method code {args} { #<<<
		return [namespace code [list my {*}$args]]
	}
	unexport code

	#>>>
}

# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> foldmarker=<<<,>>>

oo::class create cflib::baselog {
	method log {lvl {msg ""} args} {
		puts "$msg"
		#uplevel [string map [list %lvl% $lvl %msg% $msg %args% $args] {
		#	puts "[self] [self class]::[self method] %lvl% %msg%"
		#}]
	}
}


# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

oo::class create cflib::refcounted {
	constructor {} {
		if {[self next] ne {}} {next}
		set refcount	1
	}

	destructor {
		my _clear_registry
		if {[self next] ne {}} {next}
	}

	variable {*}{
		refcount
		registry_var
	}

	method object_registry {varname} { #<<<
		my _clear_registry

		set registry_var	$varname
		upvar $registry_var var_ref

		set var_ref		[self]
	}

	#>>>
	method incref {args} { #<<<
		set old		$refcount
		incr refcount
		my log_cmd debug "[self]: refcount $old -> $refcount ($args)"
	}

	#>>>
	method decref {args} { #<<<
		set old		$refcount
		incr refcount -1
		my log_cmd debug "[self]: refcount $old -> $refcount ($args)"
		if {$refcount <= 0} {
			my log_cmd debug "[self]: our time has come"
			my destroy
			return
		}
	}

	#>>>
	method refcount {} { #<<<
		return $refcount
	}

	#>>>

	method log_cmd {lvl msg args} {}
	method autoscoperef {} { #<<<
		my log_cmd debug "[self class]::[self method] callstack: (callstack dump broken)"
		upvar 2 _cflib_refcounted_scoperef_[string map {:: //} [self]] scopevar
		set scopevar	[self]
		trace variable scopevar u [namespace code {my decref "scopevar unset"}]
	}

	#>>>
	method _clear_registry {} { #<<<
		if {[info exists registry_var]} {
			upvar $registry_var old_registry
			if {[info exists old_registry]} {
				unset old_registry
			}
		}
	}

	#>>>
}


# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

oo::class create cflib::handlers {
	constructor {} {
		set allow_unregistered	1
		set handlers			[dict create]
		set afterids			[dict create]
		set processing_handlers	0
		set processing_stack	{}
	}

	destructor {
		if {[info exists afterids]} {
			dict for {key val} $afterids {
				after cancel $val
				dict unset afterids $key
			}
		}
		if {[self next] ne {}} {next}
	}

	variable {*}{
		allow_unregistered
		handlers
		afterids
		processing_handlers
		processing_stack
	}

	method register_handler {type handler} { #<<<
		if {
			![dict exists $handlers $type]
			|| $handler ni [dict get $handlers $type]
		} {
			my _handlers_debug trivia "Registering handler ($type) ($handler)"
			dict lappend handlers $type	$handler
		}
	}

	#>>>
	method deregister_handler {type handler} { #<<<
		if {![dict exists $handlers $type]} return
		set idx	[lsearch [dict get $handlers $type] $handler]
		#my log trivia "[self] Deregistering handler ($type) ($handler)"
		dict set handlers $type	[lreplace [dict get $handlers $type] $idx $idx]
	}

	#>>>
	method handlers_available {type} { #<<<
		return [expr {
			[dict exists $handlers $type] &&
			[llength [dict get $handlers $type]] >= 1}]
	}

	#>>>
	method dump_handlers {} { #<<<
		return $handlers
	}

	#>>>

	method invoke_handlers {type args} { #<<<
		if {![dict exists $handlers $type]} {
			if {$allow_unregistered} {
				return
			} else {
				error "[self]: No handlers found for type: ($type)"
			}
		}

		set results	{}
		if {$processing_handlers} {
			my _handlers_debug debug "detected reentrant handling for ($type) stack: ($processing_stack)"
		}
		incr processing_handlers	1
		lappend processing_stack	$type
		set last_handler	""
		try {
			my _handlers_debug debug "entering processing of $type"
			foreach handler [dict get $handlers $type] {
				# Check if a previous handler removed this one <<<
				if {
					![dict exists $handlers $type] ||
					$handler ni [dict get $handlers $type]
				} {
					my _handlers_debug debug "Skipping handler ($handler) which has just been removed (presumably by a previous handler in the list"
					continue
				}
				# Check if a previous handler removed this one >>>
				set pending_afterid	\
						[after 3000 [namespace code [list my _throw_hissy_handler $handler $args]]]
				set last_handler	$handler
				dict set afterids invoke_handler_$handler)	$pending_afterid
				my _handlers_debug debug "Invoking callback for ($type): ($handler)"
				lappend results	[uplevel #0 $handler $args]
				after cancel $pending_afterid
				dict unset afterids	invoke_handler_$handler
			}
		} on ok {} {
			incr processing_handlers	-1
			set processing_stack		[lrange $processing_stack 0 end-1]
			my _handlers_debug debug "leaving processing of $type"
			return $results
		} on error {errmsg options} {
			incr processing_handlers	-1
			set processing_stack		[lrange $processing_stack 0 end-1]
			my _handlers_debug error "\nError processing handlers for ($type), in handler ($last_handler): $errmsg\n[dict get $options -errorinfo]"
			dict incr options -level
			return -options $options $errmsg
		}
	}

	#>>>
	method _debug {msg} { #<<<
		my _handlers_debug debug $msg
	}

	#>>>
	method _handlers_debug {lvl msg} { #<<<
		# Override in derived class
		switch -- $lvl {
			warning -
			error {
				puts stderr "cflib::handlers::handlers_debug([self]): $lvl $msg"
			}
		}
	}

	#>>>

	method _throw_hissy_handler {handler arglist} { #<<<
		puts stderr "\n\nHandlers::throw_hissy: obj: ([self]) taking way too long to complete invoke_handlers for handler: ($handler)\n\targs: ($arglist)\n\n"
	}

	#>>>
}


# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

proc cflib::readfile {fn {mode text}} {
	try {
		set fp	[open $fn r]
		if {$mode eq "binary"} {
			chan configure $fp \
					-translation binary \
					-encoding binary
		}
		set dat	[read $fp]

		return $dat
	} finally {
		if {[info exists fp] && $fp in [chan names]} {
			close $fp
		}
	}
}


# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

proc cflib::writefile {fn data {mode "text"}} {
	try {
		set fp	[open $fn w]
		if {$mode eq "binary"} {
			chan configure $fp \
					-translation binary \
					-encoding binary
		}
		chan puts -nonewline $fp $data
	} finally {
		if {[info exists fp] && $fp in [chan names]} {
			close $fp
		}
	}
}


# vim: ft=tcl foldmethod=marker foldmarker=<<<,>>> ts=4 shiftwidth=4

proc cflib::fullynormalize {fn} {
	set fqfn	[file normalize $fn]

	set patience	20
	set seen		{}
	while {[file type $fqfn] eq "link"} {
		set fqfn	[file normalize [file readlink $fqfn]]
		if {[incr patience -1] <= 0} {
			error "Too many symlinks: $fn"
		}
		if {$fqfn in $seen} {
			error "Circular symlinks: $fn"
		}
	}

	return $fqfn
}


