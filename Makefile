# Issue a "make help" command to see a documentation for the targets defined in
# this makefile.

# -----------------------------------------------------------------------------

# Executables location.
bin_dir = bin

# Default values for options.
default_proj = apsepp_test.gpr
default_build_mode = debug_info_and_assertions
default_prof =
default_prg = $(bin_dir)/apsepp_test

# Project building and cleaning commands (variables appearing in the right hand
# sides are initialized farther down).
build_command = gprbuild $(xswi) -p -P $(PROJ)
clean_command = gprclean -q -c -r $(PROJ)

# Object files location.
obj_dir = obj

# -----------------------------------------------------------------------------

# Coverage analysis related constants (some of the variables appearing in the
# right hand sides are initialized farther down).

# lcov executable (--quiet option always used).
lcov_prg = lcov --quiet

# lcov capture command leader.
lcov_c = $(lcov_prg) --capture --directory $(obj_dir)

# Coverage analysis output location.
lcov_out_dir = lcov

# Coverage baseline file.
cov_base = $(lcov_out_dir)/$(prg_name)_base.info

# Test run coverage file.
cov_run = $(lcov_out_dir)/$(prg_name)_run.info

# Cumulated coverage data file.
cov_cum = $(lcov_out_dir)/$(prg_name)_cum.info

# Cumulated coverage data file (with some data removed).
cov_filtered = $(lcov_out_dir)/$(prg_name)_filtered.info

# genhtml executable.
genhtml_prg = genhtml

# HTML coverage report location.
html_cov_dir = $(lcov_out_dir)/html

# HTML coverage report index.
html_cov_index = $(html_cov_dir)/index.html

# HTML coverage report generation command.
html_cov_command = $(genhtml_prg) $(branchswi) $(html_cov_prefix) --legend \
  --title "$(prg_name) ($(BUILD_MODE))" --output-directory $(html_cov_dir) \
  $(cov_filtered)

# Path to Firefox application (or empty).
firefox_path = $(shell which firefox)

# -----------------------------------------------------------------------------

invalid = is not allowed. Run $(MAKE) help for documentation.

# -----------------------------------------------------------------------------

# Set PROJ variable if unset.
ifndef PROJ
  PROJ = $(default_proj)
endif

# -----------------------------------------------------------------------------

# Set BUILD_MODE variable if unset.
ifndef BUILD_MODE
  BUILD_MODE = $(default_build_mode)
endif

# Build the -X switch for gprbuild and gprclean.
xswi = -XBUILD_MODE=$(BUILD_MODE)

# -----------------------------------------------------------------------------

# Set PROF variable if unset.
ifndef PROF
  PROF = $(default_prof)
endif

# -----------------------------------------------------------------------------

# Set PRG variable if unset.
ifndef PRG
  PRG = $(default_prg)
endif

# Build prg_with_path variable ($(PRG) or $(bin_dir)/$(PRG) if PRG is just a
# file name).
ifeq ($(PRG), $(notdir $(PRG)))
  prg_with_path = $(bin_dir)/$(PRG)
else
  prg_with_path = $(PRG)
endif

# Build prg_name variable ($(PRG) without path and extension).
prg_name = $(basename $(notdir $(PRG)))

# -----------------------------------------------------------------------------

# Set PREF variable if unset.
ifndef PREF
  PREF = $(dir $(CURDIR))
endif

# Build the html_cov_prefix variable (prefix option for genhtml).
ifeq (, $(PREF))
  html_cov_prefix = --no-prefix
else
  html_cov_prefix = --prefix $(PREF)
endif

# -----------------------------------------------------------------------------

# Check BRANCH_COV variable and build the branch coverage switch for genhtml.

ifdef BRANCH_COV
  ifeq (, $(filter no yes, $(BRANCH_COV)))
    $(error BRANCH_COV=$(BRANCH_COV) $(invalid))
  endif
  ifeq ($(BRANCH_COV), yes)
    branchswi = --branch-coverage
  else
    branchswi = --no-branch-coverage
  endif
else
  branchswi = --no-branch-coverage
endif

# -----------------------------------------------------------------------------

# Check BROWSER_START variable and build the open_html_cov_report flag.

ifdef BROWSER_START
  ifeq (, $(filter no yes, $(BROWSER_START)))
    $(error BROWSER_START=$(BROWSER_START) $(invalid))
  endif
  ifeq ($(BROWSER_START), yes)
    open_html_cov_report = yes
  else
    open_html_cov_report = no
  endif
else
  open_html_cov_report = yes
endif

# -----------------------------------------------------------------------------

.PHONY: build clean test cov

# -----------------------------------------------------------------------------

# If the .ONESHELL special target appears anywhere in the makefile then all
# recipe lines for each target will be provided to a single invocation of the
# shell.
.ONESHELL:

# -----------------------------------------------------------------------------

.executables_dir:
	@mkdir -p $(bin_dir)

.run_test_program:
	@$(PROF) $(prg_with_path)

.coverage_analysis_output_dir:
	@rm -rf $(lcov_out_dir)
	@mkdir -p $(lcov_out_dir)

.build_for_cov:
	@$(build_command) \
	  -f -cargs -fprofile-arcs -ftest-coverage -largs -fprofile-arcs

.cov_baseline: .build_for_cov .coverage_analysis_output_dir
	@$(lcov_c) --initial --output-file $(cov_base)

.cov_test_run: .cov_baseline .run_test_program
	@$(lcov_c) --output-file $(cov_run)

.cov_cum: .cov_test_run
	@$(lcov_prg) --add-tracefile $(cov_base) --add-tracefile $(cov_run) \
	  --output-file $(cov_cum)
	@$(lcov_prg) --remove $(cov_cum) '$(CURDIR)/obj/*' \
	  --remove $(cov_cum) '/usr/*' \
	  --output-file $(cov_filtered)

.html_cov_report: .cov_cum
	@$(html_cov_command)

.clean_cov:
	@rm -f $(cov_base) $(cov_run) $(cov_cum) $(cov_filtered) \
	  $(obj_dir)/*.gcno $(obj_dir)/*.gcda

.html_cov_firefox: .html_cov_report
ifeq (yes, $(open_html_cov_report))
  ifneq (, $(firefox_path))
	@firefox -new-window $(html_cov_index)
	@echo 'HTML coverage report opened in new Firefox window.'
  else
	@echo 'Firefox not found.'
  endif
endif

# -----------------------------------------------------------------------------

build:
	@$(build_command)

compile:
	@$(build_command) -f -U $(FILES)

clean: .executables_dir .clean_cov
	@$(clean_command)

test: build .run_test_program

cov: .html_cov_report .html_cov_firefox

# -----------------------------------------------------------------------------

help:

	@echo
	@echo Targets:
	@echo
	@echo
	@echo '  clean [P]                 - Cleanup project (keeps executables'
	@echo '                              and HTML coverage report).'
	@echo
	@echo '  build [P] [B]             - Build project (creates executables'
	@echo '                              in "$(bin_dir)" subdirectory).'
	@echo
	@echo '  compile [P] [B] [F]       - Compile specific file(s) or all'
	@echo '                              sources if [F] is not provided. No'
	@echo '                              binding or linking.'
	@echo
	@echo '  test [B] [PROFILER] [PRG] - Build $(default_proj) and run test'
	@echo '                              program.'
	@echo
	@echo '  cov [B] [PRG] [BR] [BRO]  - Analyse coverage (builds'
	@echo '                              $(default_proj) for coverage, runs'
	@echo '                              test program and generates HTML'
	@echo '                              coverage report in "$(html_cov_dir)"'
	@echo '                              subdirectory). lcov and genhtml must'
	@echo '                              be in the path. New Firefox window'
	@echo '                              opened if Firefox found (can be'
	@echo '                              disabled by [BRO]).'
	@echo
	@echo
	@echo [P] can be nothing or one of:
	@echo '  PROJ=apsepp_test (default)'
	@echo '  PROJ=apsepp      (does not include test program)'
	@echo
	@echo [B] can be nothing or one of:
	@echo '  BUILD_MODE=debug_info_and_assertions (default)'
	@echo '  BUILD_MODE=debug_info'
	@echo '  BUILD_MODE=optimizations'
	@echo
	@echo [F] can be nothing or like:
	@echo '  FILES=path/to/a/source/file'
	@echo '  FILES="path/to/a/source/file path/to/another/source/file"'
	@echo
	@echo [PRG] can be nothing or like:
	@echo '  PRG=apsepp_test                (default, runs $(default_prg))'
	@echo '  PRG=<simple_prog_name>         (runs'
	@echo '                                  $(bin_dir)/<simple_prog_name>)'
	@echo '  PRG="<prog_path --with --opt>" (runs'
	@echo '                                  <prog_path --with --opt>))'
	@echo
	@echo [PROFILER] can be nothing or like:
	@echo '  PROF=<nothing>                      (default)'
	@echo '  PROF=valgrind                       (runs the test program'
	@echo '                                       through valgrind (must be in'
	@echo '                                       in the path))'
	@echo '  PROF="<profiler_path --with --opt>" (runs the test program'
	@echo '                                       through'
	@echo '                                       <profiler_path --with --opt>'
	@echo
	@echo [BR] can be nothing or one of:
	@echo '  BRANCH_COV=no  (default, does not include branch coverage'
	@echo '                  in coverage report)'
	@echo '  BRANCH_COV=yes (includes branch coverage in coverage report)'
	@echo
	@echo [BRO] can be nothing or one of:
	@echo '  BROWSER_START=yes (default, attempts to open HTML coverage report'
	@echo '                     in a new Firefox window)'
	@echo '  BROWSER_START=no  (does not attempt to open HTML coverage report)'
