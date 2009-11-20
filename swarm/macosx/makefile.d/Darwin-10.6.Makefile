.Darwin_build:
	cd .. && ./autogen.sh
	cd .. && ./configure --enable-openstep --without-jdkdir
	cd ${BASE} && xcodebuild \
		-configuration Release \
		-sdk ${SDK} \
		-parallelizeTargets \
		-project ${PROJECT}.xcodeproj \
		-alltargets
	cd ${BASE} && xcodebuild \
		-configuration Debug \
		-sdk ${SDK} \
		-parallelizeTargets \
		-project ${PROJECT}.xcodeproj \
		-alltargets
	touch $@

Darwin_install: ${PRODUCT}
${PRODUCT}: .Darwin_install
.Darwin_install:
	test ! -d ${PRODUCT} || rm -rf ${PRODUCT}
	cp -R ${BASE}/build/Release/$(shell basename ${PRODUCT}) ${PRODUCT}
	touch $@

Darwin_x:
	open ${BASE}/${PROJECT}.xcodeproj

Darwin_clean:
	cd ${BASE} && xcodebuild \
		-configuration Debug \
		-alltargets \
		clean
	cd ${BASE} && xcodebuild \
		-configuration Release \
		-alltargets \
		clean
	@
	test ! -f swarm/Makefile || $(MAKE) -C swarm distclean
	-find . -depth -name 'build' -exec rm -rf {} \;
	-find swarm -depth -name '*.cache' -exec rm -rf {} \;
	-find swarm -name '*.xm' -exec rm -f {} \;
	-find swarm -name '*.elc' -exec rm -f {} \;
	-find swarm -name 'aclocal.m4' -exec rm -f {} \;
	-find swarm/tests -name '*.in' -exec rm -f {} \;
	-find swarm/src -name 'Makefile.in' -exec rm -f {} \;
	-find swarm/tools -name 'Makefile.in' -exec rm -f {} \;
	-find swarm/libobjc/ -name 'Makefile.in' -exec rm -f {} \;
	-rm -f swarm/{,m4/}Makefile.in
	-rm -f swarm/{etc,COM,java}/Makefile.in
	-rm -f swarm/src/activity/activity_*
	-rm -f swarm/src/collections/collections_*
	-rm -f swarm/src/SwarmEnvironment_getters.*
	-rm -f swarm/src/defobj/modulemap{,.c}
	-rm -f swarm/{swarmconfig.h.in,configure}
	-rm -f swarm/{libobjc,avcall}/configure
	-rm -f swarm/libobjc/config.h.in
	@
	-rmdir ${BASE}/build/{${PROJECT}.build/,}Release
	-rmdir ${BASE}/build/{${PROJECT}.build/,}Debug
	-rmdir ${BASE}/build/${PROJECT}.build
	-rmdir ${BASE}/build
	@
	svn update
