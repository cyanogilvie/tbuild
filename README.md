tbuild
======

Tbuild is a tool for building Tcl modules (.tm) and single file applications
(using the trofs filesystem).  Originally written as an in-house tool for the
hundreds of little packages and utilities I've built over the years.  It isn't
really what I'd consider release-quality open-source code, but I'd like to
release some of my packages that are built using it, so here it is.

In it's current state it will take a bit of work to set up.  I will be refining
this as time goes on, but currently it still smells like an internal tool.

Example
-------
Projects are defined by a tbuild.proj file in the top-level directory of their
source code, which defines the applications, modules, debs and rpms that are
to be built, and which platforms and libc versions to build them for.

This is an example tbuild.proj, from the crypto package:
~~~tcl
tm "crypto" {
	version		0.5

	requires {}

	files {
		rsa.tcl
		blowfish.tcl
	}

	platform tcl
}

application "blowfish" {
	version 0.1

	runtime cfkit8.6

	requires {
		crypto
	}

	files {
		tools/blowfish.tcl
	}

	platform tcl
}
~~~

Running "tbuild build" in the file containing tbuild.proj will concatenate
rsa.tcl and blowfish.tcl into the file tm/tcl/crypto-0.5.tm and will assemble
the crypto dependency (and any of its dependencies, recursively) together with
the tools/blowfish.tcl file into a single-file application in apps/tcl/blowfish

The apps/tcl/blowfish application requires the cfkit8.6 runtime, which is a
trofs-based tclkit style runtime which is build to run these apps.

License
-------
Licensed under the same terms as the Tcl core.
