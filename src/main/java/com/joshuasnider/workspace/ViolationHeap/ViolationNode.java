/**
 * A node in a violation heap, which contains
 *  a key value, a rank, and three pointers to
 *  other nodes.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.violationheap;

public class ViolationNode implements Comparable {

    public ViolationNode child;//Points to the rightmost child
    public ViolationNode left;//This is null for everyone in the root list.
    public ViolationNode right;
    public int rank;
    //An integer for each node representing its rank.
    public int key;

    public ViolationNode(int k) {
      key = k;
      rank = 0;
    }

    /**
     * Add the given node to our list of children.
     */
    public void addChild(ViolationNode node) {
      if (child != null) {
        child.right = node;
      }
      node.right = this;
      node.left = child;
      child = node;
    }

    public int compareTo(Object o) {
      int result = 0;
      if (o instanceof ViolationNode) {
        ViolationNode other = (ViolationNode) o;
        if (key < other.key) {
          result = -1;
        } else if (key > other.key) {
          result = 1;
        }
      }
      return result;
    }

    public boolean onRootList() {
      //This is terrible code.
      return left == null && right.left == null && right.child != this;
    }

    public boolean hasChild() {
      return child != null;
    }

    /**
     * A node is active if it's the first or second child of its parent.
     */
    public boolean isActive() {
      return this.right.child == this || this.right.right.child == this.right;
    }

    /**
     * Return the parent of this node or null if there is none.
     */
    public ViolationNode getParent() {
      if (onRootList()) {
        return null;
      }
      ViolationNode walk = this;
      while (walk.right.child != walk) {
        walk = walk.right;
      }
      return walk.right;
    }

    /**
     * Ensure that the active child with the larger rank is the last child.
     */
    public void sortActiveChildren() {
      if (child != null && child.left != null) {
        ViolationNode firstchild = child;
        ViolationNode secndchild = child.left;
        if (firstchild.rank < secndchild.rank) {
          firstchild.right = secndchild;
          secndchild.right = this;
          child = secndchild;
          firstchild.left = secndchild.left;
          if (firstchild.left != null) {
            firstchild.left.right = firstchild;
          }
          secndchild.left = firstchild;
        }
      }
    }

    public void spliceIntoPosition(ViolationNode x) {
      //WTF are the pre and post conditions for this?
      //Past self, you really let me down.
      ViolationNode parent = x.getParent();
      ViolationNode temp = this;
      temp.left = x.left;
      temp.right = x.right;
      if (x.left != null) {
        x.left.right=temp;
      }
      if (x.right == parent) {
        parent.child = temp;
      } else {
        x.right.left = temp;
      }
    }

    public ViolationNode getLeftMostChildDestr() {
      //Post-condition:The left links on all of this's children are removed.
      //Returns the leftmost child.
      ViolationNode walk=this.child;
      while (walk != null && walk.left != null) {
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
      int calcRank = (this.child != null) ? this.child.rank : -1;
      calcRank += (this.child != null && this.child.left != null) ? this.child.left.rank : -1;
      calcRank = (int)Math.ceil(calcRank / 2.0) + 1;
      if (calcRank < rank || force) {
        rank = calcRank;
        return true;
      }
      return false;
    }
  }
