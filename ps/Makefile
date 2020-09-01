build:
	spago build
	cp -u -a output/pskt/. ../android/app/src/main/java/generated
	cd ../android && gradle packageDebug

debug:
	pscid --censor-codes UserDefinedWarning
