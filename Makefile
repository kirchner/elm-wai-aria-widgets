docs:
	elm make --optimize --docs=documentation.json

.PHONY: examples

examples:
	$(MAKE) -C examples
