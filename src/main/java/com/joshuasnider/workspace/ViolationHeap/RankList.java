/**
 * A set of ViolationNodes grouped into lists of
 * equal ranked nodes.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.violationheap;

import java.util.ArrayList;
import java.util.List;

public class RankList {

  private List<List<ViolationNode>> set;

  public RankList(List<ViolationNode> nodes) {
    set = new ArrayList<List<ViolationNode>>();
    for (ViolationNode node : nodes) {
      insert(node);
    }
  }

  /**
   * Take a ranked list of nodes, merge them together, and return the root.
   */
  public ViolationNode mergeIntoHeap() {
    ViolationNode head = null;
    ViolationNode tail = null;
    ArrayList<ViolationNode> all = new ArrayList<ViolationNode>();
    for (List<ViolationNode> sublist : set) {
      all.addAll(sublist);
    }
    for (ViolationNode node : all) {
      if (head == null) {
        head = node;
        head.right = node;
        tail = node;
      } else {
        tail.right = node;
        tail = node;
      }
    }
    tail.right = head;
    return head;
  }

  /**
   * Take a list of ranks of nodes and add one more node to it.
   */
  private void insert(ViolationNode node) {
    //Pre-condition: list.size()>node.rank and each element is either empty or contains nodes.
    int rank = node.rank;
    while (set.size() <= rank) {
      set.add(new ArrayList<ViolationNode>());
    }
    List<ViolationNode> sublist = set.get(rank);
    switch (sublist.size()) {
      case 0:
      case 1:
        sublist.add(node);
        break;
      case 2:
        ViolationNode n = ViolationHeap.threeWayJoin(node, sublist.get(0), sublist.get(1));
        set.set(rank, new ArrayList<ViolationNode>());
        insert(n);
        break;
      default:
        String err =
          "Something mysterious went wrong with insertIntoJoinList";
        throw new RuntimeException(err);
    }
  }
}
