build:
	spago build
	rm -rf  ../android_base/app/src/main/java/generated/*
	cp -u -a output/pskt/. ../android_base/app/src/main/java/generated
	cd ../android_base && gradle packageDebug
	adb install ../android_base/app/build/outputs/apk/debug/app-debug.apk
	adb disconnect
	adb kill-server

init:
	mkdir -p ../android_base/app/src/main/java/generated
	cd ../android_base && git clone https://github.com/csicar/pskt-foreigns app/src/main/java/foreigns

debug:
	pscid --censor-codes UserDefinedWarning -I "src;../common;backend"
