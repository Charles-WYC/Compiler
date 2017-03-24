/*
 * color.h - Data structures and function prototypes for coloring algorithm
 *             to determine register allocation.
 */

#include "liveness.h"
typedef struct COL_result_ *COL_result;
typedef struct G_node2_ *G_node2;
typedef struct G_nodeList2_ *G_nodeList2;
typedef struct Live_moveList2node_ *Live_moveList2node;
typedef struct Live_moveList2_ *Live_moveList2;
typedef struct Stringlist_ *Stringlist;
typedef enum { PRECOLORED, SIMPLIFYWORKLIST, FREEZEWORKLIST, SPILLWORKLIST, SPILLEDNODES, COALESCEDNODES, COLOREDNODES, SELECTSTACK,DEFAULT1 } KIND1;
typedef enum{ COALESCEDMOVES, CONSTRAINTMOVES, FROZENMOVES, WORKLISTMOVES, ACTIVEMOVES ,DEFAULT2}KIND2;

struct COL_result_ {Temp_map coloring; Temp_tempList spills;};

struct G_node2_{
	G_node node;
	KIND1 kind;
};

struct G_nodeList2_{
  G_node2 value;
  G_nodeList2 pre;
  G_nodeList2 next;
  
};

struct Live_moveList2node_{
	Live_moveList move;
	KIND2 kind;
};

struct Live_moveList2_{
	Live_moveList2node value;
	 Live_moveList2 pre;
	 Live_moveList2 next;
};
struct Stringlist_{
	string node;
	Stringlist pre;
	Stringlist next;
};
COL_result COL_color(Live_graph ig, Temp_map initial, Temp_tempList regs);
G_node2 G_Node2(G_node node);
Live_moveList2node Live_MoveList2node(Live_moveList move);
G_nodeList2 G_NodeList2(G_node2 node, G_nodeList2 pre, G_nodeList2 next);
Live_moveList2 Live_MoveList2(Live_moveList2node value, Live_moveList2 pre, Live_moveList2 next);

