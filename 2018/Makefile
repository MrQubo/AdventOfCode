SEPARATOR := ===================================================================

.PHONY: run%
run%:
	+$(MAKE) -C day$* build || true
	+echo $(SEPARATOR) && echo && \
	$(MAKE) -C day$* --quiet run && \
	echo && echo $(SEPARATOR)
	+$(MAKE) -C day$* clean || true
