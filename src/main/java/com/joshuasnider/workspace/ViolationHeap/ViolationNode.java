/**
 * A node in a violation heap, which contains
 *  a key value, a rank, and three pointers to
 *  other nodes.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.violationheap;

public class ViolationNode {

    public ViolationNode child;//Points to the rightmost child
    public ViolationNode left;//This is null for everyone in the root list.
    public ViolationNode right;
    public int rank;
    //An integer for each node representing its rank.
    public int key;

    public ViolationNode(int k) {
      ViolationHeap.totalCount++;
      key = k;
      rank = 0;
    }

    public boolean onRootList() {
      //This is terrible code.
      ViolationHeap.totalCount++;
      return left == null && right.left == null && right.child != this;
    }

    /**
     * A node is active if it's the first or second child of its parent.
     */
    public boolean isActive() {
      ViolationHeap.totalCount++;
      return this.right.child == this || this.right.right.child == this.right;
    }

    /**
     * Return the parent of this node or null if there is none.
     */
    public ViolationNode getParent() {
      ViolationHeap.totalCount++;
      if (onRootList()) {
        ViolationHeap.totalCount++;
        return null;
      }
      ViolationNode walk = this;
      while (walk.right.child != walk) {
        ViolationHeap.totalCount++;
        walk = walk.right;
      }
      return walk.right;
    }

    public void spliceIntoPosition(ViolationNode x) {
      //WTF are the pre and post conditions for this?
      //Past self, you really let me down.
      ViolationHeap.totalCount++;
      ViolationNode parent = x.getParent();
      ViolationNode temp = this;
      temp.left = x.left;
      temp.right = x.right;
      if (x.left != null) {
        ViolationHeap.totalCount++;
        x.left.right=temp;
      }
      if (x.right == parent) {
        ViolationHeap.totalCount++;
        parent.child = temp;
      } else {
        ViolationHeap.totalCount++;
        x.right.left = temp;
      }
    }

    public ViolationNode getLeftMostChildDestr() {
      //Post-condition:The left links on all of this's children are removed.
      //Returns the leftmost child.
      ViolationNode walk=this.child;
      ViolationHeap.totalCount++;
      while (walk != null && walk.left != null) {
        ViolationHeap.totalCount++;
        ViolationNode temp = walk.left;
        walk.left = null;
        walk = temp;
      }
      return walk;
    }

    /**
     * Recalculate our rank and update it if necessary or ordered to.
     * @return Whether our rank changed.
     */
    public boolean updateRank(boolean force) {
      ViolationHeap.totalCount++;
      int calcRank = (this.child != null) ? this.child.rank : -1;
      calcRank += (this.child != null && this.child.left != null) ? this.child.left.rank : -1;
      calcRank = (int)Math.ceil(calcRank / 2.0) + 1;
      if (calcRank < rank || force) {
        ViolationHeap.totalCount++;
        rank = calcRank;
        return true;
      }
      return false;
    }
  }
