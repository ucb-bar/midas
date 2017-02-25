#!/usr/bin/env python

import sys
import os
import tempfile
import subprocess
import argparse
import fm_regex

def initialize_arguments(args):
  """ initilize translator arguments """
  parser = argparse.ArgumentParser(
    description = 'run formality for macros')
  parser.add_argument('--conf', type=file, required=True,
    help="""macro conf file from FIRRTL (e.g. <design>.conf)""")
  parser.add_argument('--paths', type=file, required=True,
    help="""macro path analaysis file from Strober's compiler (d.g. <design>.macro.paths) """)
  parser.add_argument('--ref', nargs='+',
    help="""reference verilog file""")
  parser.add_argument('--impl', nargs='+',
    help="""implementation verilog file""")
  parser.add_argument('--match', type=str, required=True,
    help="match file to be appended")

  """ parse the arguments """
  res = parser.parse_args(args)

  return res.conf, res.paths, res.match, res.ref, res.impl

def read_conf_file(f):
  mems = list()
  try:
    for line in f:
      tokens = line.split()
      assert tokens[0] == 'name'
      mems.append(tokens[1])

  finally:
    f.close()

  return mems

def read_path_file(f):
  paths = dict()
  try:
    for line in f:
      tokens = line.split()
      module = tokens[0]
      path = tokens[1]
      if not module in paths:
        paths[module] = list()
      paths[module].append(path)

  finally:
    f.close()

  return paths

def read_match_file(match_file):
  gate_names = dict()
  with open(match_file, 'r') as f:
    for line in f:
      tokens = line.split()
      gate_names[tokens[0]] = tokens[1]

  return gate_names

def write_tcl(tcl_file, report_file, mem_name, ref_v_files, impl_v_files):
  with open(tcl_file, 'w') as f:
     """ No errors from unresolved modules """
     f.write("set_app_var hdlin_unresolved_modules black_box\n")

     """ Read reference verilog files """
     for ref in ref_v_files:
       f.write("read_verilog -r %s -work_library WORK\n" % ref)

     """ Set top of reference """
     f.write("set_top r:/WORK/%s\n" % mem_name)

     """ Read implementation verilog files """
     for impl in impl_v_files:
       f.write("read_verilog -i %s -work_library WORK\n" % impl)

     """ Set top of implementation """
     f.write("set_top i:/WORK/%s\n" % mem_name)

     """ Match """
     f.write("match\n")

     """ Report match points """
     f.write("report_matched_points > %s\n" % report_file)

     """ Report unmatch points """
     # f.write("report_unmatched_points >> %s\n" % report_file)

     """ Finish """
     f.write("exit\n")

  return

def append_match_file(report_file, match_file, mem, paths, gate_names):
  """ construct macro name mapping for the formality report """
  macro_map = list()

  ref_was_matched = False

  with open(report_file, 'r') as f:
    for line in f:
      if ref_was_matched:
        impl_matched = fm_regex.impl_regex.search(line)
        if impl_matched:
          impl_name = impl_matched.group(1).replace("/", ".")
          impl_name = impl_name.replace(mem + ".", "")
          ff_matched = fm_regex.ff_regex.match(impl_name)
          reg_matched = fm_regex.reg_regex.match(impl_name)
          mem_matched = fm_regex.mem_regex.match(impl_name)
          if mem_matched:
            impl_name = mem_matched.group(1) + "[" + mem_matched.group(2) + "]" +\
                                               "[" + mem_matched.group(3) + "]"
          elif reg_matched:
            impl_name = reg_matched.group(1) + "[" + reg_matched.group(2) + "]"
          elif ff_matched:
            impl_name = ff_matched.group(1)

          macro_map.append((ref_name, impl_name))
          ref_was_matched = False

      else:
        ref_matched = fm_regex.ref_regex.search(line)
        if ref_matched:
          ref_name = ref_matched.group(2).replace("/", ".")
          ref_name = ref_name.replace(mem + ".", "")
          ff_matched = fm_regex.ff_regex.match(ref_name)
          reg_matched = fm_regex.reg_regex.match(ref_name)
          mem_matched = fm_regex.mem_regex.match(ref_name)
          if mem_matched:
            ref_name = mem_matched.group(1) + "[" + mem_matched.group(2) + "]" +\
                                              "[" + mem_matched.group(3) + "]"
          elif reg_matched:
            ref_name = reg_matched.group(1) + "[" + reg_matched.group(2) + "]"
          elif ff_matched:
            ref_name = ff_matched.group(1)

          ref_was_matched = True

  """ append the name mapping to the match file """
  with open(match_file, 'a') as f:
    for path in paths:
      for ref_name, impl_name in macro_map:
        ref_full_name = path + "." + ref_name
        impl_tokens = impl_name.split(".")
        if len(impl_tokens) == 1:
          """ Top-level ports """
          impl_full_name = path + "." + impl_tokens[0]
        else:
          """ Memory elements """
          impl_mod_path = path + "." + impl_tokens[0]
          if impl_mod_path in gate_names:
            impl_mod_path = gate_names[impl_mod_path]
          impl_full_name = impl_mod_path + "." + ".".join(impl_tokens[1:])
        f.write("%s %s\n" % (ref_full_name, impl_full_name))

  return

if __name__ == '__main__':
  """ parse the arguments """
  conf_file, path_file, match_file, ref_files, impl_files = initialize_arguments(sys.argv[1:])

  """ read conf file """
  mems = read_conf_file(conf_file)

  """ read path file """
  paths = read_path_file(path_file)

  """ read match file """
  gate_names = read_match_file(match_file)

  """ create temp dir """
  dir_path = tempfile.mkdtemp()

  for mem in mems:
    """ TCL file path """
    tcl_file = os.path.join(dir_path, mem + ".tcl")

    """ report file path """
    report_file = os.path.join(dir_path, mem + ".rpt")

    """ generate TCL script for formality """
    write_tcl(tcl_file, report_file, mem, ref_files, impl_files)

    """ execute formality """
    assert subprocess.call(["fm_shell", "-f", tcl_file]) == 0

    """ append mappings to the match file """
    append_match_file(report_file, match_file, mem, paths[mem], gate_names)
