# Compile DRAMSim2
dramsim_o := $(foreach f, \
                $(patsubst %.cpp, %.o, $(wildcard $(midas_dir)/dramsim2/*.cpp)), \
                $(GEN_DIR)/$(notdir $(f)))
$(dramsim_o): $(GEN_DIR)/%.o: $(midas_dir)/dramsim2/%.cpp
	$(CXX) $(CXXFLAGS) -DNO_STORAGE -DNO_OUTPUT -Dmain=nomain -c -o $@ $<

ifeq ($(PLATFORM),zynq)
host = arm-xilinx-linux-gnueabi
endif

$(platform_gmp): $(gmp_src_dir)
	mkdir -p $(platform_gmp_build_dir)
	cd $(platform_gmp_build_dir) && \
	../configure --prefix=$(platform_gmp_install_dir) --host=$(host) && \
	$(MAKE) && $(MAKE) install

# Compile utility code
lib_files := mm mm_dramsim2 $(if $(filter $(CXX),cl),,midas_context)
lib_cc    := $(addprefix $(util_dir)/, $(addsuffix .cc, $(lib_files)))
lib_o     := $(addprefix $(GEN_DIR)/, $(addsuffix .o, $(lib_files)))

$(lib_o): $(GEN_DIR)/%.o: $(util_dir)/%.cc
	$(CXX) $(CXXFLAGS) -c -o $@ $<

lib       := $(GEN_DIR)/libmidas.a

$(lib): $(lib_o) $(dramsim_o)
	$(AR) rcs $@ $^
