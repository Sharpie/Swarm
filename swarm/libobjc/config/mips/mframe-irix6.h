#define MFRAME_STACK_STRUCT 1
#define MFRAME_SMALL_STRUCT 16
#define MFRAME_STRUCT_BYREF 0
#define MFRAME_ARGS_SIZE 136
#define MFRAME_RESULT_SIZE 16 

inline static BOOL
refstructp (const char *types)
{
  return ((*types == _C_STRUCT_B || *types ==_C_UNION_B)
          && objc_sizeof_type (types) > MFRAME_SMALL_STRUCT);
}

inline static void **
mframe_get_struct_addr_ptr (arglist_t args, const char *types)
{
  return refstructp (types)
    ? *(void **) args + 1
    : NULL;
}
    
#define MFRAME_GET_STRUCT_ADDR(ARGS, TYPES) \
({ void **ptr = mframe_get_struct_addr_ptr (ARGS, TYPES); \
   ptr ? *ptr : NULL; })

#define MFRAME_SET_STRUCT_ADDR(ARGS, TYPES, ADDR) \
( { void **ptr = mframe_get_struct_addr_ptr (ARGS, TYPES); \
    if (ptr) *ptr = (ADDR); } )
     
#define MFRAME_ARGS int

#define MFRAME_INIT_ARGS(CUM, RTYPE)  (CUM) = 20

#define MFRAME_ARG_ENCODING(CUM, TYPE, STACK, DEST) \
({  \
  const char* type = (TYPE); \
  unsigned align = objc_alignof_type (type); \
  unsigned size = objc_sizeof_type (type); \
  BOOL structref = refstructp (type); \
  unsigned offset = structref ? 4 : (CUM); \
  \
  offset = ROUND (offset, align); \
  (TYPE) = objc_skip_typespec (type); \
  \
  if (*(TYPE) == '+') \
    { \
      (TYPE)++; \
    } \
  sprintf ((DEST), "%.*s%d", (int) ((TYPE)-type), type, offset); \
  while (isdigit ((int) *(TYPE))) \
    { \
      (TYPE)++; \
    } \
  (DEST)=&(DEST)[strlen (DEST)]; \
  if (!structref) \
    offset += size; \
 (CUM) = offset; \
})

