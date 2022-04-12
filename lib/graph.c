enum node_type {FUN,PAP,THUNK,LIT,NULL};

struct fun {
  int arity;
  void* location;
  struct node* args;
};

struct thunk {
  struct node* refs;
};

struct lit {
  int num;
};

union node_const {
  struct fun   fun;
  struct fun   pap;
  struct thunk thunk;
  struct lit   lit;
  int    null;
};

struct node {
  enum node_type type;
  union node_const;
};


