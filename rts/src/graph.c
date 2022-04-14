// typedef enum {
//   APP,
//   OP,
//   FUN,
//   PAP,
//   THUNK,
//   LIT,
//   NULL
// } node_type;

// // Top-level function node
// typedef struct {
//   int arity;
//   int ap_args;
//   void* location;
//   struct node* args;
// } fun;

// // Thunk (indirection) node
// typedef struct {
//   node* refs;
// } thunk;

// // Number literal
// typedef struct {
//   int num;
// } lit_num;

// // Application node
// typedef struct {
//   node*  lhs;
//   node*  rhs;
// } app;

// // Primitive operations
// typedef enum {ADD,SUB,DIV} prim_ops;

// // Primitive operation node
// typedef struct {
//   prim_ops op;
//   int arity;
// } prim_op;

// typedef union {
//   fun       fun;
//   fun       pap;
//   thunk     thunk;
//   lit_num   lit_num;
//   app       app;
//   int       null;
// } node_const;

// // Nodes
// typedef struct {
//   node_type type;
//   node_const node;
// } node;

// node mk_fun(int arity, void* location) {
//   // TODO
// }

// node mk_pap(int arity, void* location, int ap_args, node* args) {
//   // TODO
// }

