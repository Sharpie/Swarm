#define MFRAME_STACK_STRUCT 1
#define MFRAME_SMALL_STRUCT 16
#define MFRAME_STRUCT_BYREF 0
#define MFRAME_ARGS_SIZE 136
#define MFRAME_RESULT_SIZE 16 

inline static void **
mframe_get_struct_addr_ptr (arglist_t args, const char *types)
{
  return (((*types == _C_STRUCT_B || *types ==_C_UNION_B)
    && objc_sizeof_type (types) > MFRAME_SMALL_STRUCT)
   ? ((void **) args + 1)
   : NULL);
}
    
#define MFRAME_GET_STRUCT_ADDR(ARGS, TYPES) \
({ void **ptr = mframe_get_struct_addr_ptr (ARGS, TYPES); \
   ptr ? *ptr : NULL; })

#define MFRAME_SET_STRUCT_ADDR(ARGS, TYPES, ADDR) \
( { void **ptr = mframe_get_struct_addr_ptr (ARGS, TYPES); \
    if (ptr) *ptr = (ADDR); } )
     
#define MFRAME_ARGS int

#define MFRAME_INIT_ARGS(CUM, RTYPE)	\
((CUM) = (*(RTYPE)==_C_STRUCT_B || *(RTYPE)==_C_UNION_B || \
    *(RTYPE)==_C_ARY_B) ? sizeof(void*) : 0)

#define MFRAME_ARG_ENCODING(CUM, TYPE, STACK, DEST) \
({  \
  const char* type = (TYPE); \
  int align = objc_alignof_type(type); \
  int size = objc_sizeof_type(type); \
\
  (CUM) = ROUND((CUM), align); \
  (TYPE) = objc_skip_typespec(type); \
  sprintf((DEST), "%.*s%d", (int) ((TYPE)-type), type, (CUM)); \
  if (*(TYPE) == '+') \
    { \
      (TYPE)++; \
    } \
  while (isdigit((int) *(TYPE))) \
    { \
      (TYPE)++; \
    } \
  (DEST)=&(DEST)[strlen(DEST)]; \
  if ((*type==_C_STRUCT_B || *type==_C_UNION_B || *type==_C_ARY_B)) \
    { \
      (STACK) = (CUM) + ROUND(size, align); \
    } \
  else \
    { \
      (STACK) = (CUM) + size; \
    } \
  ((((CUM) & 01) && ((size+3)/4) > 1) && (CUM)++); \
  (CUM) += ((size+3)/4); \
})

