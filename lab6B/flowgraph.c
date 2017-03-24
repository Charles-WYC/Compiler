#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "util.h"
#include "symbol.h"
#include "temp.h"
#include "tree.h"
#include "absyn.h"
#include "frame.h"
#include "flowgraph.h"
#include "errormsg.h"
#include "table.h"



Temp_tempList FG_def(G_node n){
  AS_instr as_instr = G_nodeInfo(n);
  assert(as_instr);
  switch(as_instr->kind){
  case I_OPER:
    if(as_instr->u.OPER.jumps!=NULL){
      return NULL;
    }
    return as_instr->u.OPER.dst;
  case I_LABEL:
    return NULL;
  case I_MOVE:
    return as_instr->u.MOVE.dst;
  default:
    assert(0);
  }
}
Temp_tempList FG_use(G_node n){
  AS_instr as_instr = G_nodeInfo(n);
  assert(as_instr);
  switch(as_instr->kind){
  case I_OPER:
    if(as_instr->u.OPER.jumps!=NULL||strcmp(as_instr->u.OPER.assem,"call `s0\n")==0){
      return NULL;
    }
    return as_instr->u.OPER.src;
  case I_LABEL:
    return NULL;
  case I_MOVE:
    return as_instr->u.MOVE.src;
  default:
    assert(0);
  }
}
bool FG_isMove(G_node n){
  AS_instr as_instr = G_nodeInfo(n);
  assert(as_instr);
  if(as_instr->kind==I_MOVE&&strcmp(as_instr->u.MOVE.assem,"mov `s0, `d0\n")==0){
    return TRUE;
  }
  return FALSE;
}
G_graph FG_AssemFlowGraph(AS_instrList il ,F_frame f){
  AS_instr as_instr = NULL;
  G_graph g_graph = G_Graph();
  G_node pre = NULL;
  G_nodeList g_nodeList = NULL;
  TAB_table tab_table = TAB_empty();
  Temp_label temp_label = NULL;
  while(il!=NULL){
    as_instr = il->head;
    G_node g_node = G_Node(g_graph,as_instr);
    if(pre!=NULL){
      G_addEdge(pre,g_node);
    }
    switch(as_instr->kind){
    case I_OPER:
      if(as_instr->u.OPER.jumps!=NULL){
        g_nodeList = G_NodeList(g_node,g_nodeList);
      }
    case I_MOVE:
      /*if(temp_label!=NULL){
        TAB_enter(tab_table,temp_label,g_node);
        temp_label = NULL;
      }*/
      pre = g_node;
      break;
    case I_LABEL:
      temp_label = as_instr->u.LABEL.label;
      TAB_enter(tab_table,temp_label,g_node);
      temp_label = NULL;
      break;
    default:
      assert(0);
    }
    il = il->tail;
  }
  while(g_nodeList!=NULL){
    G_node g_node = g_nodeList->head;
    AS_instr as_instr = G_nodeInfo(g_node);
    assert(as_instr&&as_instr->u.OPER.jumps);
    Temp_labelList temp_labelList =  as_instr->u.OPER.jumps->labels;
    while(temp_labelList!=NULL){
      Temp_label temp_label = temp_labelList->head;
      G_node g_destNode = TAB_look(tab_table,temp_label);
      assert(g_destNode);
      if(!G_goesTo(g_node,g_destNode)){
        G_addEdge(g_node,g_destNode);
      }
      temp_labelList = temp_labelList->tail;
    }
    g_nodeList = g_nodeList->tail;
  }
  return g_graph;
}
