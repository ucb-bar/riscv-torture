#########################################################################################
# run torture rules
#########################################################################################
.PHONY: torture torture-overnight

torture: $(output_dir) $(sim)
	$(MAKE) -C $(base_dir)/tools/torture/output clean
	$(MAKE) -C $(base_dir)/tools/torture R_SIM=$(sim) gen rtest
	cp -r $(base_dir)/tools/torture/output $(output_dir)/torture
	rm $(output_dir)/torture/Makefile

TORTURE_ONIGHT_OPTIONS :=
torture-overnight: $(output_dir) $(sim)
	$(MAKE) -C $(base_dir)/tools/torture R_SIM=$(sim) OPTIONS=$(TORTURE_ONIGHT_OPTIONS) rnight
