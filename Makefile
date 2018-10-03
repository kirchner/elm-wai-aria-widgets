docs:
	elm make --optimize --docs=docs.json

.PHONY: examples

examples:
	$(MAKE) -C examples
