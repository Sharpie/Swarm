DIRS=defobj collections activity random tkobjc swarmobject simtools analysis \
     space 

all:    headers libs
clean:
	- for f in $(DIRS); do (cd $$f; $(MAKE) clean); done
headers:
	- for f in $(DIRS); do (cd $$f; $(MAKE) install-headers); done
libs:
	- for f in $(DIRS); do (cd $$f; $(MAKE) install-lib); done
