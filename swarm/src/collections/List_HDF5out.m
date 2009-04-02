- (void)hdf5OutDeep: (id <HDF5>)hdf5Obj
{
  id <Index> li = [self begin: getCZone (getZone (self))];
  id member;
  unsigned rn;
  
  [hdf5Obj storeTypeName: [self getTypeName]];
  for (member = [li next], rn = 0;
       [li getLoc] == Member;
       member = [li next], rn++)
    {
      if (member)
	{
	  id itemGroup;
	  char buf[DSIZE (unsigned) + 1];
	  
	  sprintf (buf, "%u", rn);
	  
	  itemGroup = [[(id <HDF5>)[[[HDF5 createBegin: getCZone (getZone (self))]
			   setParent: hdf5Obj]
			  setWriteFlag: YES]
			 setName: buf]
			createEnd];
	  
	  [member hdf5OutDeep: itemGroup];
	  [itemGroup drop];
	}
    }
  [li drop];
}

- (void)hdf5OutShallow: (id <HDF5>)hdf5Obj
{
  if (![self allSameClass])
    raiseEvent (SaveError,
                "shallow HDF5 serialization on Collections must be same type");
  else
    {
      id memberProto = [self getFirst];
      id hdf5CompoundType = [[(id <HDF5CompoundType>)[HDF5CompoundType createBegin: getCZone (getZone (self))]
                               setPrototype: memberProto]
                              createEnd];
      
      id hdf5ObjDataset =
        [[(id <HDF5>)[[[[(id <HDF5>)[HDF5 createBegin: getCZone (getZone (self))]
               setName: [hdf5Obj getHDF5Name]]
              setParent: hdf5Obj]
             setWriteFlag: YES]
            setCompoundType: hdf5CompoundType]
           setCount: [self getCount]]
          createEnd];
      
      [hdf5ObjDataset storeTypeName: [self getTypeName]];
      [hdf5ObjDataset storeComponentTypeName: [memberProto getTypeName]];
      {
        id <Index> li = [self begin: getCZone (getZone (self))];
        unsigned rn;
        id member;
        
        for (member = [li next], rn = 0;
             [li getLoc] == Member;
             member = [li next], rn++)
	  {
	    [hdf5ObjDataset numberRecord: rn];
	    [hdf5ObjDataset selectRecord: rn];
	    [member hdf5OutShallow: hdf5ObjDataset];
          }
        [li drop];
      }
      [hdf5ObjDataset writeRowNames];
      [hdf5ObjDataset writeLevels];
      [hdf5ObjDataset drop];
      [hdf5CompoundType drop];
    }
}
