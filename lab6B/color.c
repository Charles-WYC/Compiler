#include <stdio.h>
#include "util.h"
#include "symbol.h"
#include "temp.h"
#include "tree.h"
#include "absyn.h"
#include "frame.h"
#include "color.h"
#include "table.h"


static G_nodeList2 precolored = NULL;
static G_nodeList2 simplifyWorklist = NULL;
static G_nodeList2 freezeWorklist = NULL;
static G_nodeList2 spillWorklist = NULL;
static G_nodeList2 spilledNodes  = NULL;
static G_nodeList2 coalescedNodes = NULL;
static G_nodeList2 coloredNodes = NULL;
static G_nodeList2 selectStack = NULL;

static Live_moveList2 coalescedMoves = NULL;
static Live_moveList2 constraintMoves = NULL;
static Live_moveList2 frozenMoves = NULL;
static Live_moveList2 worklistMoves = NULL;
static Live_moveList2 activeMoves = NULL;

static int length;
static int K;
static int* degree = NULL;
static G_nodeList2* adjList = NULL;
static bool* adjSet = NULL;
static TAB_table moveList = NULL;
static TAB_table alias = NULL;
static Temp_map color = NULL;
static TAB_table G_nodeMapG_node2 = NULL;
static Temp_tempList registers = NULL;

void initAdjSet(int n){
	adjSet = checked_malloc(n*n*sizeof(bool));
	int i;
	for(i=0;i<n*n;i++){
		adjSet[i] = FALSE;
	}
}
void initAdjList(int n){
	adjList = checked_malloc(n*sizeof(G_nodeList2));
	int i;
	for(i = 0;i < n;i++){
		adjList[i] = NULL;
	}
}
void initDegree(int n){
	degree = checked_malloc(n*sizeof(int));
	int i;
	for(i = 0;i < n;i++){
		degree[i] = 0;
	}
}
void init(int n, Temp_map inital,Temp_tempList regs){
        precolored = NULL;
        simplifyWorklist = NULL;
        freezeWorklist = NULL;
        spillWorklist = NULL;
        spilledNodes  = NULL;
        coalescedNodes = NULL;
        coloredNodes = NULL;
        selectStack = NULL;
        coalescedMoves = NULL;
        constraintMoves = NULL;
        frozenMoves = NULL;
        worklistMoves = NULL;
        activeMoves = NULL;
	length = n;
	K = lengthOfTempList(regs);
	initAdjSet(n);
	initAdjList(n);
	initDegree(n);
	moveList = TAB_empty();
	alias = TAB_empty();
	color = inital;
	G_nodeMapG_node2 = TAB_empty();
        registers = regs;
}
G_node2 G_Node2(G_node node){
	G_node2 g_node2 = checked_malloc(sizeof(*g_node2));
	g_node2->node = node;
	g_node2->kind = DEFAULT1;
        TAB_enter(G_nodeMapG_node2, node, g_node2);
	return g_node2;
}
G_nodeList2 G_NodeList2(G_node2 node,G_nodeList2 pre,G_nodeList2 next){
  G_nodeList2 g_nodeList2 = checked_malloc(sizeof(*g_nodeList2));
  g_nodeList2->value = node;
  g_nodeList2->pre = pre;
  g_nodeList2->next = next;
  return g_nodeList2;
}
Live_moveList2node Live_MoveList2node(Live_moveList move){
	Live_moveList2node live_moveList2node = checked_malloc(sizeof(*live_moveList2node));
	live_moveList2node->move = move;
	live_moveList2node->kind = DEFAULT2;
	return live_moveList2node;
}
Live_moveList2 Live_MoveList2(Live_moveList2node value, Live_moveList2 pre, Live_moveList2 next){
  Live_moveList2 live_moveList2 = checked_malloc(sizeof(*live_moveList2));
  live_moveList2->value = value;
  live_moveList2->pre = pre;
  live_moveList2->next = next;
  return live_moveList2;
}
static bool isEmpty1(G_nodeList2 set){
  return set==NULL;
}
static bool isEmpty2(Live_moveList2 set){
  return set==NULL;
}

static bool isContain1(G_nodeList2*,G_node2);
static bool isContain2(Live_moveList2*,Live_moveList2node);

static G_nodeList2 unionSet1(G_nodeList2 set1_,G_nodeList2 set2_){
  G_nodeList2 head = NULL,tail = NULL,set1=set1_,set2=set2_;
  while(set1!=NULL){
    G_nodeList2 node = G_NodeList2(set1->value,tail,NULL);
    if(tail==NULL){
      head = tail = node;
    }else{
      tail = tail->next = node;
    }
    set1 = set1->next;
  }
  while(set2!=NULL){
    G_nodeList2 node = G_NodeList2(set2->value,tail,NULL);
    //a bug at here
    if(!isContain1(&set1_,set2->value)){
      if(tail==NULL){
        head = tail = node;
      }else{
        tail = tail->next = node;
      }
    }
    set2 = set2->next;
  }
  return head;
}
static Live_moveList2 unionSet2(Live_moveList2 set1_, Live_moveList2 set2_){
  Live_moveList2 head = NULL, tail = NULL,set1 = set1_,set2=set2_;
	while (set1 != NULL){
		Live_moveList2 node = Live_MoveList2(set1->value, tail, NULL);
		if (tail == NULL){
			head = tail = node;
		}
		else{
			tail = tail->next = node;
		}
		set1 = set1->next;
	}
	while (set2 != NULL){
		Live_moveList2 node = Live_MoveList2(set2->value, tail, NULL);
                if(!isContain2(&set1_,set2->value)){
                  if (tail == NULL){
                    head = tail = node;
                  }
                  else{
                    tail = tail->next = node;
                  }
                }
		set2 = set2->next;
	}
	return head;
}
static void append1(G_nodeList2* set,G_node2 node){
	/*
	 * node<------->(node2<----->node1)==set
	 */
	if (*set == precolored){
		node->kind = PRECOLORED;
	}
	else if (*set == simplifyWorklist){
		node->kind = SIMPLIFYWORKLIST;
	}
	else if (*set == freezeWorklist){
		node->kind = FREEZEWORKLIST;
	}
	else if (*set == spillWorklist){
		node->kind = SPILLWORKLIST;
	}
	else if (*set == spilledNodes){
		node->kind = SPILLEDNODES;
	}
	else if (*set == coalescedNodes){
		node->kind = COALESCEDNODES;
	}
	else if (*set == coloredNodes){
		node->kind = COLOREDNODES;
	}
	else if (*set == selectStack){
		node->kind = SELECTSTACK;
	}
	G_nodeList2 g_nodeList2 = G_NodeList2(node, NULL, NULL);
	if (*set == NULL){
		*set = g_nodeList2;
	}
	else{
		(*set)->pre = g_nodeList2;
		g_nodeList2->next = (*set);
		(*set) = g_nodeList2;
	}
}
static void append2(Live_moveList2* set, Live_moveList2node node){
	/*
	* node<------->(node2<----->node1)==set
	*/
	if (*set == coalescedMoves){
		node->kind = COALESCEDMOVES;
	}
	else if (*set == constraintMoves){
		node->kind = CONSTRAINTMOVES;
	}
	else if (*set == frozenMoves){
		node->kind = FROZENMOVES;
	}
	else if (*set == worklistMoves){
		node->kind = WORKLISTMOVES;
	}
	else if (*set == activeMoves){
		node->kind = ACTIVEMOVES;
	}
	Live_moveList2 live_moveList2 = Live_MoveList2(node, NULL, NULL);
	if (*set == NULL){
		*set = live_moveList2;
	}
	else{
		(*set)->pre = live_moveList2;
		live_moveList2->next = (*set);
		(*set) = live_moveList2;
	}
}
static bool isContain1(G_nodeList2* set, G_node2 node){
  assert(set&&node);
	if (set == &precolored){
		return node->kind == PRECOLORED;
	}
	else if (set == &simplifyWorklist){
		return node->kind == SIMPLIFYWORKLIST;
	}
	else if (set == &freezeWorklist){
		return node->kind == FREEZEWORKLIST;
	}
	else if (set == &spillWorklist){
		return node->kind == SPILLWORKLIST;
	}
	else if (set == &spilledNodes){
		return node->kind == SPILLEDNODES;
	}
	else if (set == &coalescedNodes){
		return node->kind == COALESCEDNODES;
	}
	else if (set == &coloredNodes){
		return node->kind == COLOREDNODES;
	}
	else if (set == &selectStack){
		return node->kind == SELECTSTACK;
	}
        G_nodeList2 set_ = *set;
	while (set_ != NULL){
		if (set_->value == node){
			return TRUE;
		}
		set_ = set_->next;
	}
	return FALSE;
}
static bool isContain2(Live_moveList2* set, Live_moveList2node node){
  assert(set&&node);
	if (set == &coalescedMoves){
		return node->kind == COALESCEDMOVES;
	}
	else if (set == &constraintMoves){
		return node->kind == CONSTRAINTMOVES;
	}
	else if (set == &frozenMoves){
		return node->kind == FROZENMOVES;
	}
	else if (set == &worklistMoves){
		return node->kind == WORKLISTMOVES;
	}
	else if (set == &activeMoves){
		return node->kind == ACTIVEMOVES;
	}
        Live_moveList2 set_ = *set;
	while (set_ != NULL){
		if (set_->value == node){
			return TRUE;
		}
		set_ = set_->next;
	}
	return FALSE;
}
static G_nodeList2 diffSet1(G_nodeList2 set1_,G_nodeList2 set2_){
  G_nodeList2 head = NULL, tail=NULL,set1=set1_;
	while (set1 != NULL){
		if (!isContain1(&set2_, set1->value)){
			if (tail == NULL){
				head = tail = G_NodeList2(set1->value, NULL, NULL);
			}
			else{
				G_nodeList2 node = G_NodeList2(set1->value, tail, NULL);
				tail = tail->next = node;
			}
		}
		set1 = set1->next;
	}
	return head;
}
static Live_moveList2 interSet2(Live_moveList2 set1, Live_moveList2 set2){
	Live_moveList2 head=NULL, tail=NULL;
	while (set1 != NULL){
		if (isContain2(&set2, set1->value)){
			if (tail == NULL){
				head = tail = Live_MoveList2(set1->value, NULL, NULL);
			}
			else{
				Live_moveList2 node = Live_MoveList2(set1->value, tail, NULL);
				tail = tail->next = node;
			}
		}
		set1 = set1->next;
	}
	return head;
}
static void delete1(G_nodeList2* set_, G_node2 node){
	assert(*set_);
	G_nodeList2 set = *set_;
	while (set != NULL){
		if (set->value == node){
			if (set->pre != NULL){
				set->pre->next = set->next;
			}
			if (set->next != NULL){
				set->next->pre = set->pre;
			}
			break;
		}
		set = set->next;
	}
	if ((*set_)->value == node){
          *set_ = (*set_)->next;
	}
}
static void delete2(Live_moveList2* set_, Live_moveList2node node){
	assert(*set_);
	Live_moveList2 set = *set_;
	while (set != NULL){
		if (set->value == node){
			if (set->pre != NULL){
				set->pre->next = set->next;
			}
			if (set->next != NULL){
				set->next->pre = set->pre;
			}
			break;
		}
		set = set->next;
	}
	if ((*set_)->value == node){
          *set_ = (*set_)->next;
	}
}
static void delete3(Stringlist * set_, string node){
	assert(*set_);
	Stringlist set = *set_;
	while (set != NULL){
		if (set->node == node){
			if (set->pre != NULL){
				set->pre->next = set->next;
			}
			if (set->next != NULL){
				set->next->pre = set->pre;
			}
			break;
		}
		set = set->next;
	}
	if ((*set_)->node == node){
          *set_ = (*set_)->next;
	}
}
static G_node2 peek1(G_nodeList2* set){
  assert(*set);
  G_node2 node = (*set)->value;
  delete1(set, node);
  return node;
}
static G_node2 pop1(G_nodeList2* stack){
	return peek1(stack);
}
static Live_moveList2node peek2(Live_moveList2* set){
	if (*set != NULL){
		Live_moveList2node node = (*set)->value;
		delete2(set, node);
		return node;
	}
	return NULL;
}
static void push(G_nodeList2* stack, G_node2 node){
	append1(stack, node);
}
static bool isLink(int m, int n){
	return adjSet[m*length + n];
}
static void addEdge_(int m,int n){
	adjSet[m*length + n] = TRUE;
}
static void addEdge(G_node2 node1, G_node2 node2){
	int m = node1->node->mykey;
	int n = node2->node->mykey;
	if (m != n&&!isLink(m,n)){
		addEdge_(m, n);
		addEdge_(n, m);
		if (node1->kind != PRECOLORED){
			append1(&adjList[m], node2);
		}
		if (node2->kind != PRECOLORED){
			append1(&adjList[n], node1);
		}
	}
}
static Live_moveList2 NodeMoves(G_node2 node){
	return interSet2(TAB_look(moveList, node), unionSet2(activeMoves, worklistMoves));
}
static bool moveRelated(G_node2 node){
	return !isEmpty2(NodeMoves(node));
}
static void makeWorklist(G_nodeList2 initial){
	while (initial != NULL){
		G_node2 g_node2 = initial->value;
		int pos = g_node2->node->mykey;
		if (degree[pos] >= K){
			append1(&spillWorklist, g_node2);
		}
		else if (moveRelated(g_node2)){
			append1(&freezeWorklist, g_node2);
		}
		else{
			append1(&simplifyWorklist, g_node2);
		}
		initial = initial->next;
	}
}
static G_nodeList2 adjacent(G_node2 node){
	return diffSet1(adjList[node->node->mykey], unionSet1(selectStack, coalescedNodes));
}
static void enableMoves(G_nodeList2 g_nodeList2){
	while (g_nodeList2 != NULL){
		Live_moveList2 live_moveList2 = NodeMoves(g_nodeList2->value);
		while (live_moveList2 != NULL){
			if (live_moveList2->value->kind == ACTIVEMOVES){
				delete2(&activeMoves, live_moveList2->value);
				append2(&worklistMoves, live_moveList2->value);
			}
			live_moveList2 = live_moveList2->next;
		}
		g_nodeList2 = g_nodeList2->next;
	}
}
static void decrementDegree(G_node2 node){
	int d = degree[node->node->mykey];
	degree[node->node->mykey]--;
	if (d == K){
		enableMoves(unionSet1(adjacent(node), G_NodeList2(node, NULL, NULL)));
		delete1(&spillWorklist, node);
		if (moveRelated(node)){
			append1(&freezeWorklist, node);
		}
		else{
			append1(&simplifyWorklist, node);
		}
	}
}
static void simplify(){
	if (simplifyWorklist != NULL){
		G_node2 node = peek1(&simplifyWorklist);
		push(&selectStack, node);
		G_nodeList2 g_nodeList = adjacent(node);
		while (g_nodeList != NULL){
			decrementDegree(g_nodeList->value);
			g_nodeList = g_nodeList->next;
		}
	}
}
static G_node2 getAlias(G_node2 node){
  assert(node);
	if (isContain1(&coalescedNodes, node)){
		getAlias(TAB_look(alias, node));
	}
	return node;
}
static void addWorkList(G_node2 node){
	if (!isContain1(&precolored,node) && !moveRelated(node) && degree[node->node->mykey] < K){
		delete1(&freezeWorklist, node);
		append1(&simplifyWorklist, node);
	}
}
static bool OK(G_node2 t, G_node2 r){
	return degree[t->node->mykey] < K || isContain1(&precolored, t) || isLink(t->node->mykey, r->node->mykey);
}
static bool conservative(G_nodeList2 nodes){
	int k = 0;
	while (nodes != NULL){
		if (degree[nodes->value->node->mykey] >= K){
			k++;
		}
		nodes = nodes->next;
	}
	return k < K;
}
static void combine(G_node2 u, G_node2 v){
	if (isContain1(&freezeWorklist,v)){
		delete1(&freezeWorklist, v);
	}
	else{
		delete1(&spillWorklist, v);
	}
	append1(&coalescedNodes, v);
	TAB_enter(alias, v, u);
	TAB_enter(moveList,u,unionSet2(TAB_look(moveList, u), TAB_look(moveList, v)));
	enableMoves(G_NodeList2(v, NULL, NULL));
	G_nodeList2 g_nodeList2 = adjacent(v);
	while (g_nodeList2 != NULL){
		addEdge(g_nodeList2->value, u);
		decrementDegree(g_nodeList2->value);
		g_nodeList2 = g_nodeList2->next;
	}
	if (degree[u->node->mykey] >= K&&isContain1(&freezeWorklist,u)){
		delete1(&freezeWorklist, u);
		append1(&spillWorklist, u);
	}
}
static void coalesce(){
	Live_moveList2node  node = peek2(&worklistMoves);
        assert(node->move->dst&&node->move->src);
	G_node2 x = getAlias(TAB_look(G_nodeMapG_node2, node->move->dst));
	G_node2 y = getAlias(TAB_look(G_nodeMapG_node2, node->move->src));
	G_node2 u = x;
	G_node2 v = y;
	if (isContain1(&precolored, y)){
		u = y;
		v = x;
	}
	if (u == v){
		append2(&coalescedMoves, node);
		addWorkList(u);
	}
	else if (isContain1(&precolored, v) || isLink(u->node->mykey,v->node->mykey)){
		append2(&constraintMoves, node);
		addWorkList(u);
		addWorkList(v);
	}
	else {
		bool bFlag = TRUE;
		if (isContain1(&precolored, u)){
			G_nodeList2 g_nodeList2 = adjacent(v);
			while (g_nodeList2 != NULL){
				if (!OK(g_nodeList2->value, u)){
					bFlag = FALSE;
					break;
				}
				g_nodeList2 = g_nodeList2->next;
			}
		}
		else{
			bFlag = FALSE;
		}
		if (bFlag || (!isContain1(&precolored, u) && conservative(unionSet1(adjacent(u), adjacent(v))))){
			append2(&coalescedMoves, node);
			combine(u, v);
			addWorkList(u);
		}
		else{
			append2(&activeMoves, node);
		}
	}
}
static void freezeMoves(G_node2 u){
	Live_moveList2 live_moveList2 = NodeMoves(u);
	while (live_moveList2 != NULL){
		Live_moveList2node m = live_moveList2->value;
		G_node2 x = TAB_look(G_nodeMapG_node2,m->move->dst);
		G_node2 y = TAB_look(G_nodeMapG_node2, m->move->src);
		G_node2 v = getAlias(y);
		if (getAlias(y) == getAlias(u)){
			v = getAlias(x);
		}
		delete2(&activeMoves, m);
		append2(&frozenMoves, m);
		if (isEmpty2(NodeMoves(v)) && degree[v->node->mykey] < K){
			delete1(&freezeWorklist, v);
			append1(&simplifyWorklist, v);
		}
		live_moveList2 = live_moveList2->next;
	}
}
static void freeze(){
	G_node2 node = peek1(&freezeWorklist);
	append1(&simplifyWorklist, node);
	freezeMoves(node);
}
static void selectSpill(){
	G_node2 m = peek1(&spillWorklist);
	append1(&simplifyWorklist, m);
	freezeMoves(m);
}
static Stringlist StringList(string node,Stringlist pre,Stringlist next){
	Stringlist stringlist = checked_malloc(sizeof(*stringlist));
	stringlist->node = node;
	stringlist->pre = pre;
	stringlist->next = next;
	return stringlist;
}
static Stringlist allColors(){
	Stringlist head = NULL,tail = NULL;
	Temp_tempList regs = registers;
	while(regs!=NULL){
		string node = Temp_look(color,regs->head);
		Stringlist temp  = StringList(node,tail,NULL);
		if(tail == NULL){
			head = tail = temp;
		}else{
			tail = tail->next = temp;
		}
                regs = regs->tail;
	}
	return head;
}
static bool isEmpty3(Stringlist stringlist){
	return stringlist == NULL;
}
static void assignColors(){
	while (!isEmpty1(selectStack)){
		G_node2 n = pop1(&selectStack);
		Stringlist okColors = allColors();
		G_nodeList2 g_nodeList2 = adjList[n->node->mykey];
		while (g_nodeList2){
			G_node2 w = g_nodeList2->value;
                        getAlias(w);
                        G_nodeList2 tempNodeList = unionSet1(coloredNodes,precolored);
                        if(isEmpty3(okColors)){
                          break;
                        }
			if (isContain1(&tempNodeList,getAlias(w))){
				string strColor = Temp_look(color, getAlias(w)->node->info);
				delete3(&okColors, strColor);
			}
			g_nodeList2 = g_nodeList2->next;
		}
		if (isEmpty3(okColors)){
			append1(&spilledNodes, n);
		}
		else{
			append1(&coloredNodes, n);
			Temp_enter(color, n->node->info, okColors->node);
		}
	}
	G_nodeList2 g_nodeList2 = coalescedNodes;
	while (g_nodeList2 != NULL){
		Temp_enter(color, g_nodeList2->value->node->info, Temp_look(color, getAlias(g_nodeList2->value)->node->info));
		g_nodeList2 = g_nodeList2->next;
	}
}
COL_result COL_color(Live_graph ig, Temp_map inital, Temp_tempList regs){
	G_graph graph = ig->graph;
	Live_moveList moves = ig->moves;
	G_nodeList g_nodeList = G_nodes(graph);
	init(graph->nodecount, inital,regs);
	G_nodeList2 g_nodeList2 = NULL;
	while (g_nodeList != NULL){
		G_node g_node = g_nodeList->head;
                G_node2 g_node2 = G_Node2(g_node);
                if (Temp_look(inital,g_node->info)!=NULL){
                  append1(&precolored,g_node2);
                }else{
			append1(&g_nodeList2, g_node2);
		}
		g_nodeList = g_nodeList->tail;
	}
	g_nodeList = G_nodes(graph);
	//initial adjSet and adjList
	while (g_nodeList != NULL){
		G_node g_node = g_nodeList->head;
		G_node2 g_node2 = TAB_look(G_nodeMapG_node2, g_node);
		G_nodeList adjNodeList = G_adj(g_node);

		while (adjNodeList != NULL){
			G_node otherG_node = adjNodeList->head;
			G_node2 otherG_node2 = TAB_look(G_nodeMapG_node2, otherG_node);
			addEdge(g_node2, otherG_node2);
			adjNodeList = adjNodeList->tail;
		}
		g_nodeList = g_nodeList->tail;
	}
	//initial moveList and worklistMoves
	while (moves != NULL){
          Live_moveList2node live_moveList2node = NULL;
          assert(moves->dst!=NULL&&moves->src!=NULL);

          live_moveList2node = Live_MoveList2node(moves);
          append2(&worklistMoves, live_moveList2node);
          G_node2 dst = TAB_look(G_nodeMapG_node2, moves->dst);
          G_node2 src = TAB_look(G_nodeMapG_node2, moves->src);
          TAB_enter(moveList, dst, unionSet2(TAB_look(moveList, dst), Live_MoveList2(live_moveList2node,NULL,NULL)));
          TAB_enter(moveList, src, unionSet2(TAB_look(moveList, src), Live_MoveList2(live_moveList2node, NULL, NULL)));
          moves = moves->tail;
	}
	makeWorklist(g_nodeList2);
	do{
		if (!isEmpty1(simplifyWorklist)){
                  simplify();
		}
		else if (!isEmpty2(worklistMoves)){
			coalesce();
		}
		else if (!isEmpty1(freezeWorklist)){
			freeze();
		}
		else{
			selectSpill();
                }
	} while (!isEmpty1(simplifyWorklist)||!isEmpty2(worklistMoves)||!isEmpty1(freezeWorklist)||!isEmpty1(spillWorklist));
	assignColors();
	COL_result col_result = checked_malloc(sizeof(*col_result));
	col_result->coloring = Temp_layerMap(color,F_temp2Name());
	Temp_tempList spills = NULL;
	while (spilledNodes != NULL){
		spills = Temp_TempList(spilledNodes->value->node->info,spills);
		spilledNodes = spilledNodes->next;
	}
	col_result->spills = spills;
	return col_result;
}
