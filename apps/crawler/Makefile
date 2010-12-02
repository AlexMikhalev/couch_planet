all: ebin doc log
	(cd src;$(MAKE))

debug: ebin doc log
	(cd src;$(MAKE) debug)

clean:
	rm -rf log/sasl/*
	rm -f erl_crash.dump
	(cd src;$(MAKE) clean)

ebin:
	@mkdir -p ebin

doc:
	@mkdir -p doc

log:
	@mkdir -p log/sasl
