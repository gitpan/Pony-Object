package Pony::Object;

# "You will never find a more wretched hive of scum and villainy.
#  We must be careful."

use feature ':5.10';
use Storable qw/dclone/;
use Module::Load;
use Carp qw(confess);
use Scalar::Util qw(refaddr);

use constant DEBUG => 0;

BEGIN {
  if (DEBUG) {
    say STDERR "\n[!] Pony::Object DEBUGing mode is turning on!\n";
    
    *{dumper} = sub {
      use Data::Dumper;
      $Data::Dumper::Indent = 1;
      say Dumper(@_);
      say '=' x 79;
    }
  }
}

our $VERSION = 0.08;

# Var: $DEFAULT
#   Use it to redefine default Pony's options.
our $DEFAULT = {
  '' => {
    'withExceptions' => 0,
    'baseClass' => [],
  }
};

# Function: import
#   This function will runs on each use of this module.
#   It changes caller - adds new keywords,
#   makes caller more strict and modern,
#   create from simple package almost normal class.
#   Also it provides some useful methods.
#   
#   Don't forget: it's still OOP with blessed refs,
#   but now it looks better - more sugar for your code.

sub import {
  my $this = shift;
  my $call = caller;
  
  # Modify caller just once.
  # We suppose, that only we can create function ALL.
  return if defined *{$call.'::ALL'};
  
  # Parse parameters.
  my $default = dclone $DEFAULT;
  my $profile;
  for my $prefix (sort {length $b <=> length $a} keys %$DEFAULT) {
    if ($call =~ /^$prefix/) {
      $profile->{$_} = $default->{$prefix}->{$_}
        for grep {not exists $profile->{$_}} keys %{ $default->{$prefix} };
      next;
    }
    last if keys %{$default->{''}} == keys %{$default->{$call}};
  }
  $profile->{isAbstract} = 0; # don't do default object abstract.
  $profile->{isSingleton} = 0; # don't do default object singleton.
  
  $profile = parseParams($call, $profile, @_);
  
  # Keywords, base methods, attributes.
  predefine($call, $profile);
  
  # Pony objects must be strict and modern.
  strict  ->import;
  warnings->import;
  feature ->import(':5.10');
  
  # Base classes and params.
  #parseParams($call, "${call}::ISA", @_);
  prepareClass($call, "${call}::ISA", $profile);
  
  methodsInheritance($call);
  propertiesInheritance($call);
  
  *{$call.'::new'} = sub { importNew($call, @_) };
}


# Function: importNew
#  Constructor for Pony::Objects.
#
# Parameters:
#   $call - Str - caller package.
#
# Returns:
#   self

sub importNew {
  my $call = shift;
  
  if ($call->META->{isAbstract}) {
    confess "Trying to use an abstract class $call";
  } else {
    $call->AFTER_LOAD_CHECK;
  }
  
  # For singletons.
  return ${$call.'::instance'} if defined ${$call.'::instance'};
  
  my $this = shift;
  
  my $obj = dclone { %{${this}.'::ALL'} };
  $this = bless $obj, $this;
  
  ${$call.'::instance'} = $this if $call->META->{isSingleton};
  
  # 'After hook' for user.
  $this->init(@_) if $call->can('init');
  return $this;
}


# Function: parseParams
#   Load all base classes and read class params.
#
# Parameters:
#   $call    - Str      - caller package.
#   $profile - HashRef  - profile of this use.
#   @params  - Array    - import params.
#
# Returns:
#   HashRef - $profile

sub parseParams {
  my ($call, $profile, @params) = @_;
  
  for my $param (@params) {
    given ($param) {
    
      # Define singleton class.
      when (/^-?singleton$/) {
        $profile->{isSingleton} = 1;
        next;
      }
      
      # Define abstract class.
      when (/^-?abstract$/) {
        $profile->{isAbstract} = 1;
        next;
      }
      
      # Features:
      
      # Use exceptions featureset.
      when (/^:exceptions?$/) {
        $profile->{withExceptions} = 1;
        next;
      }
      
      # Don't use exceptions featureset.
      when (/^:noexceptions?$/) {
        $profile->{withExceptions} = 0;
        next;
      }
    }
    
    # Save class' base classes.
    push @{$profile->{baseClass}}, $param;
  }
  
  return $profile;
}


# Function: prepareClass
#   Load all base classes and process class params.
#
# Parameters:
#   $call     - Str       - caller package.
#   $isaRef   - ArrayRef  - ref to @ISA.
#   $profile  - HashRef   - parsed params profile.

sub prepareClass {
  my ($call, $isaRef, $profile) = @_;

  $call->META->{isSingleton} = $profile->{isSingleton} // 0;
  $call->META->{isAbstract} = $profile->{isAbstract} // 0;

  for my $base (@{ $profile->{baseClass} }) {
    next if $call eq $base;
    load $base;
    $base->AFTER_LOAD_CHECK if $base->can('AFTER_LOAD_CHECK');
    push @$isaRef, $base;
  }
}


# Function: predefine
#   Predefine keywords and base methods.
#
# Parameters:
#   $call - Str - caller package.
#   $profile - HashRef

sub predefine {
  my ($call, $profile) = @_;
  
  # Predefine ALL and META.
  %{$call.'::ALL' } = ();
  %{$call.'::META'} = ();
  ${$call.'::META'}{isSingleton} = 0;
  ${$call.'::META'}{isAbstract}  = 0;
  ${$call.'::META'}{abstracts}   = [];
  ${$call.'::META'}{methods}   = {};
  ${$call.'::META'}{symcache}  = {};
  ${$call.'::META'}{checked}   = 0;
  
  #====================
  # Define "keywords".
  #====================
  
  # Access for properties.
  *{$call.'::has'}      = sub { addProperty ($call, @_) };
  *{$call.'::public'}   = sub { addPublic   ($call, @_) };
  *{$call.'::private'}  = sub { addPrivate  ($call, @_) };
  *{$call.'::protected'}= sub { addProtected($call, @_) };
  
  # Try, Catch, Finally.
  # Define them if user wants.
  if ($profile->{withExceptions}) {
    *{$call.'::try'} = sub (&;@) {
      my($try, $catch, $finally) = @_;
      local $@;
      
      # If some one wanna to get some
      # values from try/catch/finally blocks.
      given (wantarray) {
        when (0) {
          my $ret = eval{ $try->() };
          $ret = $catch->($@) if $@;
          $ret = $finally->() if defined $finally;
          return $ret;
        }
        when (1) {
          my @ret = eval{ $try->() };
          @ret = $catch->($@) if $@;
          @ret = $finally->() if defined $finally;
          return @ret;
        }
        default {
          eval{ $try->() };
          $catch->($@) if $@;
          $finally->() if defined $finally;
        }
      }
    };
    *{$call.'::catch'} = sub (&;@) { @_ };
    *{$call.'::finally'} = sub (&) { @_ };
  }
  
  #=========================
  # Define special methods.
  #=========================
  
  # Getters for REFs to special variables %ALL and %META.
  *{$call.'::ALL'}  = sub { \%{ $call.'::ALL' } };
  *{$call.'::META'} = sub { \%{ $call.'::META'} };
  
  # This method provides deep copy
  # for Pony::Objects
  *{$call.'::clone'}  = sub { dclone shift };
  
  # Convert object's data into hash.
  # Uses ALL() to get properties' list.
  *{$call.'::toHash'} = *{$call.'::to_h'} = sub {
    my $this = shift;
    my %hash = map { $_, $this->{$_} } keys %{ $this->ALL() };
    return \%hash;
  };
  
  # Simple Data::Dumper wrapper.
  *{$call.'::dump'} = sub {
    use Data::Dumper;
    $Data::Dumper::Indent = 1;
    Dumper(@_);
  };
  
  *{$call.'::AFTER_LOAD_CHECK'} = sub { checkImplenets($call) };
  
  # Save method's attributes.
  *{$call.'::MODIFY_CODE_ATTRIBUTES'} = sub {
    my ($pkg, $ref, @attrs) = @_;
    my $sym = findsym($call, $pkg, $ref);
    
    $call->META->{methods}->{ *{$sym}{NAME} } = {
      attributes => \@attrs,
      package => $pkg
    };
    
    for ( @attrs ) {
      given ($_) {
        when ('Public'   ) { makePublic   ($call,$pkg,$sym,$ref) }
        when ('Protected') { makeProtected($call,$pkg,$sym,$ref) }
        when ('Private'  ) { makePrivate  ($call,$pkg,$sym,$ref) }
        when ('Abstract' ) { makeAbstract ($call,$pkg,$sym,$ref) }
      }
    }
    return;
  };
}

# Function: methodsInheritance
#   Inheritance of methods.
#
# Parameters:
#   $this - Str - caller package.

sub methodsInheritance {
  my $this = shift;
  
  for my $base ( @{$this.'::ISA'} ) {
    # All Pony-like classes.
    if ($base->can('META')) {
      my $methods = $base->META->{methods};
      
      while (my($k, $v) = each %$methods) {
        $this->META->{methods}->{$k} = $v
          unless exists $this->META->{methods}->{$k};
      }
      
      # Abstract classes.
      if ($base->META->{isAbstract}) {
        my $abstracts = $base->META->{abstracts};
        push @{ $this->META->{abstracts} }, @$abstracts;
      }
    }
  }
}


# Function: checkImplenets
#   Check for implementing abstract methods
#   in our class in non-abstract classes.
#
# Parameters:
#   $this - Str - caller package.

sub checkImplenets {
  my $this = shift;
  
  return if $this->META->{checked};
  $this->META->{checked} = 1;
  
  # Check: does all abstract methods implemented.
  for my $base (@{$this.'::ISA'}) {
    if ( $base->can('META') && $base->META->{isAbstract} ) {
      my $methods = $base->META->{abstracts};
      my @bad;
      
      # Find Abstract methods,
      # which was not implements.
      for my $method (@$methods) {
        # Get Abstract methods.
        push @bad, $method
          if grep { $_ eq 'Abstract' }
            @{ $base->META->{methods}->{$method}->{attributes} };
        
        # Get abstract methods,
        # which doesn't implement.
        @bad = grep { !exists $this->META->{methods}->{$_} } @bad;
      }
      
      if (@bad) {
        my @messages = map
          {"Didn't find method ${this}::$_() defined in $base."}
            @bad;
        push @messages, "You should implement abstract methods before.\n";
        confess join("\n", @messages);
      }
    }
  }
}


# Function: addProperty
#   Guessing access type of property.
#
# Parameters:
#   $this - Str - caller package.
#   $attr - Str - name of property.
#   $attr - Mixed - default value of property.

sub addProperty {
  my ($this, $attr, $value) = @_;
  
  given ($attr) {
    when(/^__/) { return addPrivate(@_) }
    when(/^_/ ) { return addProtected(@_) }
    default     { return addPublic(@_) }
  }
}


# Function: addPublic
#   Create public property with accessor.
#   Save it in special variable ALL.
#
# Parameters:
#   $this  - Str - caller package.
#   $attr  - Str - name of property.
#   $value - Mixed - default value of property.

sub addPublic {
  my ($this, $attr, $value) = @_;
  # Save pair (property name => default value)
  %{ $this.'::ALL' } = ( %{ $this.'::ALL' }, $attr => $value );
  *{$this."::$attr"} = sub : lvalue { my $this = shift; $this->{$attr} };
}


# Function: addProtected
#   Create protected property with accessor.
#   Save it in special variable ALL.
#   Can die on wrong access attempt.
#
# Parameters:
#   $pkg  - Str - caller package.
#   $attr - Str - name of property.
#   $value - Mixed - default value of property.

sub addProtected {
  my ($pkg, $attr, $value) = @_;
  
  # Save pair (property name => default value)
  %{ $pkg.'::ALL' } = ( %{ $pkg.'::ALL' }, $attr => $value );
  
  *{$pkg."::$attr"} = sub : lvalue {
    my $this = shift;
    my $call = caller;
    
    confess "Protected ${pkg}::$attr called"
      unless ( $call->isa($pkg) || $pkg->isa($call) )
        and ( $this->isa($pkg) );
    
    $this->{$attr};
  };
}


# Function: addPrivate
#   Create private property with accessor.
#   Save it in special variable ALL.
#   Can die on wrong access attempt.
#
# Parameters:
#   $pkg  - Str - caller package.
#   $attr - Str - name of property.
#   $value - Mixed - default value of property.

sub addPrivate {
  my ($pkg, $attr, $value) = @_;
  
  # Save pair (property name => default value)
  %{ $pkg.'::ALL' } = ( %{ $pkg.'::ALL' }, $attr => $value );
  
  *{$pkg."::$attr"} = sub : lvalue {
    my $this = shift;
    my $call = caller;
    
    confess "Private ${pkg}::$attr called"
      unless $pkg->isa($call) && ref $this eq $pkg;
    
    $this->{$attr};
  };
}

# Function: makeProtected
#   Function's attribute.
#   Uses to define, that this code can be used
#   only inside this class and his childs.
#
# Parameters:
#   $this - package where Pony::Object were used
#   $pkg - Str - name of package, where this function defined.
#   $symbol - Symbol - reference to perl symbol.
#   $ref - CodeRef - reference to function's code.

sub makeProtected {
  my ($this, $pkg, $symbol, $ref) = @_;
  my $method = *{$symbol}{NAME};
  
  no warnings 'redefine';
  
  *{$symbol} = sub {
    my $this = $_[0];
    my $call = caller;
    
    confess "Protected ${pkg}::$method() called"
      unless ( $call->isa($pkg) || $pkg->isa($call) )
        and ( $this->isa($pkg) );
    
    goto &$ref;
  }
}

# Function: makePrivate
#   Function's attribute.
#   Uses to define, that this code can be used
#   only inside this class. NOT for his childs.
#
# Parameters:
#   $this - package where Pony::Object were used
#   $pkg - Str - name of package, where this function defined.
#   $symbol - Symbol - reference to perl symbol.
#   $ref - CodeRef - reference to function's code.

sub makePrivate {
  my ($this, $pkg, $symbol, $ref) = @_;
  my $method = *{$symbol}{NAME};
  
  no warnings 'redefine';
  
  *{$symbol} = sub {
    my $this = $_[0];
    my $call = caller;
    
    confess "Private ${pkg}::$method() called"
      unless $pkg->isa($call) && ref $this eq $pkg;
    
    goto &$ref;
  }
}


# Function: makePublic
#   Function's attribute.
#   Uses to define, that this code can be used public.
#
# Parameters:
#   $this - package where Pony::Object were used
#   $pkg - Str - name of package, where this function defined.
#   $symbol - Symbol - reference to perl symbol.
#   $ref - CodeRef - reference to function's code.

sub makePublic {
  # do nothing
}


# Function: makeAbstract
#   Function's attribute.
#   Define abstract attribute.
#   It means, that it doesn't conteins realisation,
#   but none abstract class, which will extends it,
#   MUST implement it.
#
# Parameters:
#   $this - package where Pony::Object were used
#   $pkg - Str - name of package, where this function defined.
#   $symbol - Symbol - reference to perl symbol.
#   $ref - CodeRef - reference to function's code.

sub makeAbstract {
  my ( $this, $pkg, $symbol, $ref ) = @_;
  my $method = *{$symbol}{NAME};
  
  # Can't define abstract method
  # in none-abstract class.
  confess "Abstract ${pkg}::$method() defined in non-abstract class"
    unless $this->META->{isAbstract};
  
  # Push abstract method
  # into object meta.
  push @{ $this->META->{abstracts} }, $method;
  
  no warnings 'redefine';
  
  # Can't call abstract method.
  *{$symbol} = sub {
    confess "Abstract ${pkg}::$method() called";
  }
}


# Function: propertiesInheritance
#   This function calls when we need to get
#   properties (with thier default values)
#   form classes which our class extends to our class.
#
# Parameters:
#   $this - Str - caller package.

sub propertiesInheritance {
  my $this = shift;
  my %classes;
  my @classes = @{ $this.'::ISA' };
  my @base;
  
  # Get all parent's properties
  while (@classes) {
    my $c = pop @classes;
    next if exists $classes{$c};
    
    %classes = (%classes, $c => 1);
    
    push @base, $c;
    push @classes, @{ $c.'::ISA' };
  }
  
  for my $base (reverse @base) {
    if ($base->can('ALL')) {
      my $all = $base->ALL();
      
      for my $k (keys %$all) {
        unless (exists ${$this.'::ALL'}{$k}) {
          %{ $this.'::ALL' } = ( %{ $this.'::ALL' }, $k => $all->{$k} );
        }
      }
    }
  }
}


# Function: findsym
#   Get perl symbol by ref.
#
# Parameters:
#   $this - Str - caller package.
#   $pkg - Str - package, where it defines.
#   $ref - CodeRef - reference to method.
#
# Returns:
#   Symbol

sub findsym {
  my ($this, $pkg, $ref) = @_;
  my $symcache = $this->META->{symcache};
  
  return $symcache->{$pkg, $ref} if $symcache->{$pkg, $ref};
  
  my $type = 'CODE';
  
  for my $sym (values %{$pkg."::"}) {
    next unless ref ( \$sym ) eq 'GLOB';
    
    return $symcache->{$pkg, $ref} = \$sym
      if *{$sym}{$type} && *{$sym}{$type} == $ref;
  }
}

1;

__END__

=head1 NAME

Pony::Object is the object system.

=head1 OVERVIEW

Pony::Object is an object system, which provides simple way to use cute objects.

=head1 SYNOPSIS

  # Class: MyArticle (Example)
  #   Abstract class for articles.
  
  package MyArticle;
  use Pony::Object qw(-abstract :exceptions);
  use MyArticle::Exception::IO; # Based on Pony::Object::Throwable class.
    
    protected date => undef;
    protected authors => [];
    public title => '';
    public text => '';
    
    
    # Function: init
    #   Constructor.
    #
    # Parameters:
    #   date - Int
    #   authors - ArrayRef
    
    sub init : Public
      {
        my $this = shift;
        ($this->date, $this->authors) = @_;
      }
    
    
    # Function: getDate
    #   Get formatted date.
    #
    # Returns:
    #   Str
    
    sub getDate : Public
      {
        my $this = shift;
        return $this->dateFormat($this->date);
      }
    
    
    # Function: dateFormat
    #   Convert Unix time to good looking string. Not implemented.
    #
    # Parameters:
    #   date - Int
    #
    # Returns:
    #   String
    
    sub dateFormat : Abstract;
    
    
    # Function: fromPdf
    #   Trying to create article from pdf file.
    #
    # Parameters:
    #   file - Str - pdf file.
    
    sub fromPdf : Public
      {
        my $this = shift;
        my $file = shift;
        
        try {
          open F, $file or
            throw MyArticle::Exception::IO(action => "read", file => $file);
          
          # do smth
          
          close F;
        } catch {
          my $e = shift; # get exception object
          
          if ($e->isa('MyArticle::Exception::IO')) {
            # handler for MyArticle::Exception::IO exceptions
          }
        };
      }
    
  1;

=head1 DESCRIPTION

When some package uses Pony::Object, it becomes strict and modern
(can use perl 5.10 features like as C<say>). Also C<dump> function
is redefined and shows data structure. It's useful for debugging.

=head2 Specific moments

Besides new function C<dump> Pony::Object has other specific moments.

=head3 has

Keyword C<has> declares new fields.
All fields are public. You can also describe object methods via C<has>...
If you want.

  package News;
  use Pony::Object;
  
    # Fields
    has 'title';
    has text => '';
    has authors => [ qw/Alice Bob/ ];
    
    # Methods
    sub printTitle
      {
        my $this = shift;
        say $this->title;
      }

    sub printAuthors
      {
        my $this = shift;
        print @{$this->authors};
      }
  1;

  package main;
  
  my $news = new News;
  $news->printAuthors();
  $news->title = 'Something important';
  $news->printTitle();

Pony::Object fields assigned via "=". For example: $obj->field = 'a'.

=head3 new

Pony::Objects hasn't method C<new>. In fact, of course they has. But C<new> is an
internal function, so you should not use it if you don't want more "fun".
Instead of this Pony::Object has C<init> function, where you can write the same,
what you wish write in C<new>. C<init> is after-hook for C<new>.

  package News;
  use Pony::Object;
    
    has title => undef;
    has lower => undef;
    
    sub init
      {
        my $this = shift;
        $this->title = shift;
        $this->lower = lc $this->title;
      }
    
  1;

  package main;
  
  my $news = new News('Big Event!');
  
  print $news->lower;

=head3 toHash or to_h

Get object's data structure and return this as a hash.

  package News;
  use Pony::Object;
    
    has title => 'World';
    has text => 'Hello';
    
  1;

  package main;
  
  my $news = new News;
  print $news->toHash()->{text};
  print $news->to_h()->{title};

=head3 dump

Return string which shows object's current struct.

  package News;
  use Pony::Object;
  
    has title => 'World';
    has text => 'Hello';
    
  1;

  package main;
  
  my $news = new News;
  $news->text = 'Hi';
  print $news->dump();

Returns

  $VAR1 = bless( {
    'text' => 'Hi',
    'title' => 'World'
  }, 'News' );

=head3 public, protected, private properties

You can use C<has> keyword to define property. If your variable starts with "_", variable becomes 
protected. "__" for private.

  package News;
  use Pony::Object;
  
    has text => '';
    has __authors => [ qw/Alice Bob/ ];
    
    sub getAuthorString
      {
        my $this = shift;
        return join(' ', @{$this->__authors});
      }
    
  1;

  package main;
  
  my $news = new News;
  say $news->getAuthorString();

The same but with keywords C<public>, C<protected> and C<private>.

  package News;
  use Pony::Object;
  
    public text => '';
    private authors => [ qw/Alice Bob/ ];
    
    sub getAuthorString
      {
        my $this = shift;
        return join(' ', @{$this->authors});
      }
    
  1;

  package main;
  
  my $news = new News;
  say $news->getAuthorString();

=head3 Public, Protected, Private methods

Use attributes C<Public>, C<Private> and C<Protected> to define method's access type.

  package News;
  use Pony::Object;
  
    public text => '';
    private authors => [ qw/Alice Bob/ ];
    
    sub getAuthorString : Public
      {
        return shift->joinAuthors(', ');
      }
    
    sub joinAuthors : Private
      {
        my $this = shift;
        my $delim = shift;
        
        return join( $delim, @{$this->authors} );
      }
  1;

  package main;
  
  my $news = new News;
  say $news->getAuthorString();

=head3 Inheritance

You can define base classes via C<use> params.
For example, use Pony::Object 'Base::Class';

  package BaseCar;
  use Pony::Object;
    
    public speed => 0;
    protected model => "Base Car";
    
    sub get_status_line : Public
      {
        my $this = shift;
        my $status = ($this->speed ? "Moving" : "Stopped");
        return $this->model . " " . $status;
      }
    
  1;

  package MyCar;
  # extends BaseCar
  use Pony::Object qw/BaseCar/;
    
    protected model => "My Car";
    protected color => undef;
    
    sub set_color : Public
      {
        my $this = shift;
        ($this->color) = @_;
      }
    
  1;

  use MyCar;
  my $car = new MyCar;
  $car->speed = 20;
  $car->set_color("White");
  print $car->get_status_line();
  # "My Car Moving"

=head3 Singletons

Pony::Object has simple syntax for singletons . You can declare this via C<use> param;

  package Notes;
  use Pony::Object 'singleton';
    
    protected list => [];
    
    sub add : Public
      {
        my $this = shift;
        push @{ $this->list }, @_;
      }
    
    sub show : Public
      {
        my $this = shift;
        say for @{$this->list};
      }
    
    sub flush : Public
      {
        my $this = shift;
        $this->list = [];
      }
    
  1;

  package main;
  use Notes;
  
  my $n1 = new Notes;
  my $n2 = new Notes;
  
  $n1->add(qw/eat sleep/);
  $n1->add('Meet with Mary at 8 o`clock');
  
  $n2->flush;
  
  $n1->show();  # Print nothing.
                # Em... When I should meet Mary? 

=head3 Abstract methods and classes

You can use abstract methods and classes follows way:

  # Let's define simple interface for texts.
  package Text::Interface;
  use Pony::Object -abstract; # Use 'abstract' or '-abstract'
                              # params to define abstract class.
    
    sub getText : Abstract; # Use 'Abstract' attribute to
    sub setText : Abstract; # define abstract method.
    
  1;

  # Now we can define base class for texts.
  # It's abstract too but now it has some code.
  package Text::Base;
  use Pony::Object qw/abstract Text::Interface/;
    
    protected text => '';
    
    sub getText : Public
      {
        my $this = shift;
        return $this->text;
      }
    
  1;

  # In the end we can write Text class.
  package Text;
  use Pony::Object 'Text::Base';
    
    sub setText : Public
      {
        my $this = shift;
        $this->text = shift;
      }
    
  1;

  # Main file.
  package main;
  use Text;
  use Text::Base;
  
  my $textBase = new Text::Base;  # Raises an error!
  
  my $text = new Text;
  $text->setText('some text');
  print $text->getText();   # Returns 'some text';

Don't forget, that perl looking for functions from left to right in list of
inheritance. You should define abstract classes in the end of
Pony::Object param list.

=head3 Exceptions

See L<Pony::Object::Throwable>.

=head3 ALL

If you wanna get all default values of Pony::Object-based class,
you can call C<ALL> method. I don't know why you need them, but you can.

  package News;
  use Pony::Object;
    
    has 'title';
    has text => '';
    has authors => [ qw/Alice Bob/ ];
    
  1;

  package main;
  my $news = new News;
  print for keys %{ $news->ALL() };

=head3 META

One more internal method. It provides access to special hash C<%META>.
You can use this for Pony::Object introspection. It can be changed in next versions.

  my $news = new News;
  say dump $news->META;

=head3 $Pony::Object::DEFAULT

This is a global variable. It defines default Pony::Object's params. For example you can set
C<$Pony::Object::DEFAULT->{''}->{withExceptions} = 1> to enable exceptions by default.
Use it carefully. Use this if you sure, that this is smaller evil.

  # Startup script
  ...
  use Pony::Object;
  
  BEGIN {
    # Use exceptions by default.
    $Pony::Object::DEFAULT->{''}->{withExceptions} = 1;
    # All classes will extends Default::Base.
    $Pony::Object::DEFAULT->{''}->{baseClass} = [qw/Default::Base/];
    # All classes in namespace "Default::NoBase" will not.
    $Pony::Object::DEFAULT->{'Default::NoBase'}->{baseClass} = [];
  }
  ...

One more example:

  # Startup script
  ...
  use Pony::Object;
  
  BEGIN {
    $Pony::Object::DEFAULT->{'My::Awesome::Project'} = {
      withExceptions => 1,
      baseClass => [],
    };
    
    $Pony::Object::DEFAULT->{'My::Awesome::Project::Model'} = {
      withExceptions => 1,
      baseClass => [qw/My::Awesome::Project::Model::Abstract/],
    };
  }
  ...

=head1 SEE

=over

=item Git

L<https://github.com/h15/pony-object>

=back

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2011 - 2013, Georgy Bazhukov.

This program is free software, you can redistribute it and/or modify it under
the terms of the Artistic License version 2.0.

=cut
