AUTOMAKE_OPTIONS = foreign

.PHONY: rpmdist rpmpgpdist swarm-rpm swarm-hdf5-rpm swarm-rpm-pgp swarm-hdf5-rpm-pgp

rpmdist: swarm-rpm swarm-hdf5-rpm

rpmpgpdist: swarm-rpm-pgp swarm-hdf5-rpm-pgp

swarm-rpm:
	rpm -ba swarm.spec

swarm-hdf5-rpm:
	rpm -ba swarm-hdf5.spec

swarm-rpm-pgp:
	rpm -ba --sign swarm.spec

swarm-hdf5-rpm-pgp:
	rpm -ba --sign swarm-hdf5.spec

all:

