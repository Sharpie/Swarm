.Darwin_build:
	cd $(SRC_SWARM) && ./autogen.sh
	cd $(SRC_SWARM) && ./configure \
		--enable-openstep \
		--without-jdkdir
	cd ${SRC_SWARMOSX} && xcodebuild \
		-configuration Release \
		-sdk ${SDK} \
		-parallelizeTargets \
		-project ${PROJECT}.xcodeproj \
		-alltargets
	cd ${SRC_SWARMOSX} && xcodebuild \
		-configuration Debug \
		-sdk ${SDK} \
		-parallelizeTargets \
		-project ${PROJECT}.xcodeproj \
		-alltargets
	touch $@

Darwin_install: ${PRODUCT}
${PRODUCT}: .Darwin_install
.Darwin_install: ${XCODE_TEMPLATES}
	test ! -d ${PRODUCT} || rm -rf ${PRODUCT}
	cp -R ${SRC_SWARMOSX}/build/Release/$(shell basename ${PRODUCT}) ${PRODUCT}
	touch $@

${XCODE_TEMPLATES}: $(SRC_SWARM)/etc/XcodeTemplate
	sudo mkdir -p $@
	sudo rsync -a $</ $@/

Darwin_x:
	open ${SRC_SWARMOSX}/${PROJECT}.xcodeproj

Darwin_clean:
	cd ${SRC_SWARMOSX} && xcodebuild \
		-configuration Debug \
		-alltargets \
		clean
	cd ${SRC_SWARMOSX} && xcodebuild \
		-configuration Release \
		-alltargets \
		clean
	test ! -d ${SRC_SWARMOSX}/build || rm -rf ${SRC_SWARMOSX}/build
	test ! -f $(SRC_SWARM)/Makefile || $(MAKE) -C $(SRC_SWARM) super-clean
	cd $(SRC_SWARM) && svn update
