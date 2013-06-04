#!/usr/bin/env perl

use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new
(
	module_name         => 'Pony::Object',
	dist_abstract       => 'tiny object system with privacy settings for properties and methods, abstract classes and methods, exception',
	license             => 'perl',
	dist_author         => 'Georgy Bazhukov <gosha@bugov.net>',
	dist_version_from   => 'lib/Pony/Object.pm',
	build_requires =>
	{
		'Test::More' => 0,
	},
	requires =>
	{
		'perl' => '5.010',
		'Module::Load' => 0,
		'Storable' => 0,
		'Carp' => 0,
		'Scalar::Util' => 0,
	},
	meta_merge =>
	{
		resources =>
		{
			repository => 'https://github.com/h15/pony-object',
		},
		keywords => [ qw/Class Object Method OOP Private Protected Abstract Interface Exception/ ],
	},
	add_to_cleanup     => [],
	create_makefile_pl => 'traditional',
);

$builder->create_build_script();

