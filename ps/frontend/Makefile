build:
	spago build
	rm -rf  ../../android/app/src/main/java/generated/*
	cp -u -a output/pskt/. ../../android/app/src/main/java/generated
	cd ../../android && gradle packageDebug

init:
	mkdir -p ../../android/app/src/main/java/generated
	cd ../../android && git clone https://github.com/csicar/pskt-foreigns app/src/main/java/foreigns

debug:
	pscid --censor-codes UserDefinedWarning -I "frontend;common;backend"
