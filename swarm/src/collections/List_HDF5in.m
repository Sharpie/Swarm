- hdf5In: hdf5Obj
{
  if ([hdf5Obj getDatasetFlag])
    {
      Class class = [hdf5Obj getClass];
      unsigned i, c_count = [hdf5Obj getCount];
      
      for (i = 0; i < c_count; i++)
        {
          id obj =
            ([class respondsTo: M(isJavaProxy)]
             ? SD_JAVA_INSTANTIATE (SD_JAVA_FINDJAVACLASS (class))->object
             : [class create: getZone (self)]);
          
          [hdf5Obj selectRecord: i];
          [hdf5Obj shallowLoadObject: obj];
          [(id) self addLast: obj];
        }
    }
  else
    {
      int process_object (id component)
        {
          [(id) self addLast: hdf5In ([self getZone], component)];
          return 0;
        }
      [hdf5Obj iterate: process_object];
    }
  return self;
}
