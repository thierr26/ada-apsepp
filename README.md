# ada-apsepp


## General information

Apsepp is a personal [Ada 2012](https://www.ada2012.org) project. It's still in
very early infancy.


## Compilation instructions


### Compiler availability and installation

You can compile this project with GNAT and gprbuild. These tools are available
in [GNAT Community edition](https://www.adacore.com/community). They're also
packaged in most GNU/Linux distributions. On [Debian
GNU/Linux](https://www.debian.org/) for example, you can install GNAT and
gprbuild with this command (as root):
```
apt-get install gnat gprbuild
```


#### Compilation with gprbuild

After cloning the repository, change directory to the repository root and issue
this command:
```
gprbuild -p -P apsepp_test.gpr
```

It creates an executable (test program) in the bin subdirectory. Use this
command to run it:
```
bin/apsepp_test
```

You can add a `-X` switch in the `gprbuild` command to require a specific build
mode:
- Debugging information, and assertions enabled (default build mode):
```
gprbuild -p -XAPSEPP_BUILD_MODE=debug_info_and_assertions apsepp_test.gpr
```

- Build with debugging information:
```
gprbuild -p -XAPSEPP_BUILD_MODE=debug_info apsepp_test.gpr
```

- Build with optimizations:
```
gprbuild -p -XAPSEPP_BUILD_MODE=optimizations apsepp_test.gpr
```

You can remove the files created by the compilation process in the bin and obj
subdirectories with this command:
```
gprclean -r -Papsepp_test.gpr
```

If you want to build the demo programs instead of the test program, please
substitute apsepp_test.gpr with apsepp_demo.gpr in the `gprbuild` command.

#### Compilation with make

A makefile is provided in the repository and if you have [GNU
Make](https://www.gnu.org/software/make/) installed, you can perform the
compilation, cleaning, etc. operations with `make` commands. Change directory
to the repository root and issue this command to see a documentation about the
targets and options defined in the makefile:
```
make help
```

Here are some examples of `make` commands:
```
make       # Builds project apsepp_test.gpr.

make test  # Builds project apsepp_test.gpr and runs bin/apsepp_test.

make clean # Removes most of the files created by the compilation process in
           # the obj subdirectory.

make cov   # Rebuilds, runs bin/apsepp_test and generates an HTML coverage
           # report. lcov must be installed (on Debian: apt-get install lcov).

make test PROJ=apsepp_demo PRG=apsepp_demo_output_sink_as_shared_instance
           # Builds project apsepp_demo.gpr and runs
           # bin/apsepp_demo_output_sink_as_shared_instance
```


## Author

[Thierry Rascle](mailto:thierr26@free.fr)


## License

This project is licensed under the MIT License. For more information, please
refer to the [LICENSE](LICENSE) file.
