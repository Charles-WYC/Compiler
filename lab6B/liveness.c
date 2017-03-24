#include <stdio.h>
#include "util.h"
#include "symbol.h"
#include "temp.h"
#include "tree.h"
#include "absyn.h"
#include "frame.h"
#include "liveness.h"
#include "table.h"


static bool isMember(Temp_tempList set,Temp_temp elem){
  while(set!=NULL){
    if(set->head==elem)
      return TRUE;
    set = set->tail;
  }
  return FALSE;
}
static int setSize(Temp_tempList temp_tempList){
  int size = 0;
  for(;temp_tempList;temp_tempList=temp_tempList->tail)size++;
  return size;
}
static Temp_tempList differenceSet(Temp_tempList set1_,Temp_tempList set2_){
  Temp_tempList re_head = NULL,re_tail = NULL,set1=set1_;
  while(set1!=NULL){
    if(!isMember(set2_,set1->head)){
      Temp_tempList temp = Temp_TempList(set1->head,NULL);
      if(re_tail==NULL){
        re_head = re_tail = temp;
      }else{
        re_tail->tail = temp;
        re_tail = re_tail->tail;
      }
    }
    set1 = set1->tail;
  }
  return re_head;
}
static Temp_tempList unionSet(Temp_tempList set1_,Temp_tempList set2_){
  Temp_tempList re_head = NULL,re_tail = NULL,set1 = set1_,set2=set2_;
  while(set1!=NULL){
    Temp_tempList temp = Temp_TempList(set1->head,NULL);
    if(re_tail==NULL){
      re_head = re_tail = temp;
    }else{
      re_tail = re_tail->tail = temp;
    }
    set1 = set1->tail;
  }
  while(set2!=NULL){
    Temp_tempList temp = Temp_TempList(set2->head,NULL);
    if(!isMember(set1_,set2->head)){
      if(re_tail == NULL){
        re_head = re_tail = temp;
      }else{
        re_tail = re_tail->tail = temp;
      }
    }
    set2 = set2->tail;
  }
  return re_head;
}
static G_nodeList reverseList(G_nodeList g_nodeList){
  G_nodeList re = NULL;
  while(g_nodeList!=NULL){
    re = G_NodeList(g_nodeList->head,re);
    g_nodeList = g_nodeList->tail;
  }
  return re;
}
static G_node getNodeByTemp(TAB_table table,G_graph graph,Temp_temp temp){
  G_node g_node = TAB_look(table,temp);
  if(g_node==NULL){
    g_node = G_Node(graph,temp);
    TAB_enter(table,temp,g_node);
  }
  return g_node;
}
static void enterLiveMap(G_table t,G_node flowNode,Temp_tempList temp){
  G_enter(t,flowNode,temp);
}
static Temp_tempList lookupLiveMap(G_table t,G_node flownode){
  return (Temp_tempList)G_look(t,flownode);
}
Live_moveList Live_MoveList(G_node src,G_node dst,Live_moveList tail){
  Live_moveList live_moveList = checked_malloc(sizeof(*live_moveList));
  live_moveList->src = src;
  live_moveList->dst = dst;
  live_moveList->tail = tail;
  return live_moveList;
}
Temp_temp Live_gtemp(G_node n){
  Temp_temp info = (Temp_temp)n->info;
  return info;
}
Live_graph Live_liveness(G_graph flow){
  /*
   *initalize return value;
   */
  Live_graph live_graph = checked_malloc(sizeof(*live_graph));
  G_graph g_graph = G_Graph();
  Live_moveList live_moveList = NULL;
  live_graph->graph = g_graph;
  live_graph->moves = live_moveList;

  G_table g_table_in = G_empty();
  G_table g_table_out = G_empty();

  G_nodeList g_nodeList = reverseList(G_nodes(flow));
  G_nodeList p = g_nodeList;
  bool hasChanged = FALSE;
  while(p!=NULL||((p=g_nodeList)&&hasChanged&&(hasChanged=FALSE)==FALSE)){
    G_node g_node = p->head;
    Temp_tempList oldTempList_in = lookupLiveMap(g_table_in,g_node);
    Temp_tempList oldTempList_out = lookupLiveMap(g_table_out,g_node);
    Temp_tempList newTempList_in = unionSet(FG_use(g_node),differenceSet(oldTempList_out,FG_def(g_node)));
    Temp_tempList newTempList_out = NULL;
    G_nodeList succNodeList = G_succ(g_node);
    while(succNodeList!=NULL){
      G_node succNode = succNodeList->head;
      newTempList_out = unionSet(newTempList_out,lookupLiveMap(g_table_in,succNode));
      succNodeList = succNodeList->tail;
    }
    if(setSize(oldTempList_in)!=setSize(newTempList_in)){
      hasChanged = TRUE;
      enterLiveMap(g_table_in,g_node,newTempList_in);
    }
    if(setSize(oldTempList_out)!=setSize(newTempList_out)){
      hasChanged = TRUE;
      enterLiveMap(g_table_out,g_node,newTempList_out);
    }
    p = p->tail;
  }
  TAB_table Temp2Node = TAB_empty();
  while(p!=NULL){
    G_node g_node = p->head;
    Temp_tempList defTempList = FG_def(g_node);
    Temp_tempList useTempList = FG_use(g_node);
    Temp_tempList tempList = lookupLiveMap(g_table_in, g_node);
    AS_instr tmp_instr = g_node->info;
    if(FG_isMove(g_node)){
      tempList = differenceSet(tempList, useTempList);
      live_moveList = Live_MoveList(getNodeByTemp(Temp2Node,g_graph,defTempList->head),getNodeByTemp(Temp2Node,g_graph,useTempList->head),live_moveList);
      assert(live_moveList->src&&live_moveList->dst);
    }
    while(defTempList!=NULL){
      Temp_temp defTemp = defTempList->head;
      G_node def_node = getNodeByTemp(Temp2Node,g_graph,defTemp);
      while(tempList!=NULL){
        Temp_temp temp_temp = tempList->head;
        G_node g_node = getNodeByTemp(Temp2Node, g_graph, temp_temp);
        G_addEdge(def_node, g_node);
        G_addEdge(g_node, def_node);
        tempList = tempList->tail;
      }
      defTempList = defTempList->tail;
    }
    p = p->tail;
  }
  return live_graph;
}

