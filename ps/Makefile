build:
	spago build
	cp -u -a output/pskt/. ../android/app/src/main/java/generated
	cd ../android && git clone https://github.com/csicar/pskt-foreigns app/src/main/java/foreigns && gradle packageDebug

debug:
	pscid --censor-codes UserDefinedWarning
