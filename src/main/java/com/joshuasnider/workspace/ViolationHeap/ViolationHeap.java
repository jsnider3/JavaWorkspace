/**
 * My implementation of a violation heap,
 *  which is a data structure similar to
 *  a Fibonacci heap.
 *
 * @author Josh Snider
 */

package com.joshuasnider.workspace.violationheap;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class ViolationHeap {
  static int totalCount = 0;
  public ViolationNode root;

  /**
   * The node with the lowest key.
   * This is identical to the first node in the root list.
   */
  public ViolationNode findMin() {
    totalCount++;
    return root;
  }

  public void insert(ViolationNode ins) {
    //A single node x is inserted into the root list of h.
    //The rank of x is initially set to zero.
    ins.left = null;
    totalCount++;
    if (root == null) {
      totalCount++;
      root = ins;
      ins.right = ins;
    }
    //Insert the node into the array.
    ins.right = root.right;
    root.right = ins;
    if (ins.key < root.key) {
      //Move the root as needed.
      totalCount++;
      root = ins;
    }
  }

  /**
   * Merge this and another heap into one, by combining their root lists.
   */
  public void union(ViolationHeap other) {
    totalCount++;
    ViolationNode temp = root.right;
    root.right = other.root.right;
    other.root.right = temp;
    root = root.key < other.root.key ? root : other.root;
    other.root = root;
  }

  /**
   * Does this violation heap contain nodes?
   */
  public boolean isEmpty() {
    return root == null;
  }

  public void decreaseKey(int n, ViolationNode x) {
    totalCount++;
    if (n < 0) {
      totalCount++;
      throw new RuntimeException("This is decreaseKey, not increaseKey.");
    }
    x.key -= n;//Subtract n from the key of x.
    if(x.onRootList()){//If x is a root
      totalCount++;
      if(x.key<root.key){//and its new value is smaller than the minimum, make it the root and stop.
        totalCount++;
        root=x;
      }
    }
    //If x is an active node whose new value is not smaller than its parent, stop.
    else if(!x.isActive()||x.key<x.getParent().key)
    {
      totalCount++;
      //Otherwise, cut the subtree of x and glue in its position the subtree with
      //the larger rank between its active children.
      ViolationNode parent=x.getParent();
      if(x.child==null){
        totalCount++;
        if(x.left!=null){
          totalCount++;
          x.left.right=x.right;
        }
        if(x.right==parent){
          totalCount++;
          x.right.child=x.left;
        }
        else{
          totalCount++;
          x.right.left=x.left;
        }
      }
      else if(x.child.left==null||x.child.rank>=x.child.left.rank){
        totalCount++;
        ViolationNode temp=x.child;
        if(x.child.left!=null){
          totalCount++;
          x.child.left.right=x;
        }
        x.child=temp.left;
        temp.spliceIntoPosition(x);
      }
      else{
        totalCount++;
        ViolationNode temp=x.child.left;
        x.child.left=temp.left;
        if(temp.left!=null){
          totalCount++;
          temp.left.right=x.child;
        }
        temp.spliceIntoPosition(x);
      }
      x.updateRank(true);
      //Recalculate the rank of x using (1).
      //Promote x's subtree as a tree in h, and make x the first root if its new
      //value is smaller than the minimum.
      this.insert(x);
      /* Propagate rank updates by traversing the path of ancestors of x's old position,
       * as long as the visited node is active and as long as its recalculated rank using
       * (1) is smaller than its old rank.*/
      while (parent != null && parent.isActive()) {
        //TODO might need to do something involving critical nodes.
        totalCount++;
        parent.updateRank(false);
        parent = parent.getParent();
      }
    }
  }

  /**
   * Delete the minimum of the heap.
   * @return The deleted minimum.
   */
  public ViolationNode deleteMin() {
    ViolationNode oldRoot = root;
    root = promoteRootsChildren();
    if (!isEmpty()) {
      List<ViolationNode> allNodes = splitRootList();
      RankList rankedList = new RankList(allNodes);
      root = rankedList.mergeIntoHeap();
      root = findNewMin();
    }
    return oldRoot;
  }

  /**
   * DESTRUCTIVELY, Create an array containing all nodes of the root list.
   */
  public List<ViolationNode> splitRootList() {
    List<ViolationNode> allNodes = new ArrayList<ViolationNode>();
    ViolationNode temp;
    for (ViolationNode walk = root; walk != root || allNodes.isEmpty();
        walk = temp) {
      totalCount++;
      allNodes.add(walk);
      temp = walk.right;
      walk.right = null;
    }
    return allNodes;
  }



  public static ViolationNode threeWayJoin(ViolationNode z,
      ViolationNode z1, ViolationNode z2) {
    //Pre-condition: z.rank=z1.rank=z2.rank
    //Post-condition: The node with the lowest key gains the others as its last two children.
    //The one with the larger rank is the last child.
    /*3-way-join(z, z1, z2)
      Assume w.l.o.g. that z's value is not larger than that of z1 and z2.
    */
    //Make sure that z is the smallest and that z1.rank>z2.rank.
    totalCount++;
    if (z.key > z1.key) {
      totalCount++;
      ViolationNode temp = z;
      z = z1;
      z1 = temp;
    }
    if (z.key > z2.key) {
      totalCount++;
      ViolationNode temp = z;
      z = z2;
      z2 = temp;
    }
    //Ensure that the active child of z with the larger rank is the last child.
    if (z.child != null && z.child.left != null) {
      totalCount++;
      ViolationNode firstchild = z.child;
      ViolationNode secndchild = z.child.left;
      if (firstchild.rank < secndchild.rank) {
        totalCount++;
        firstchild.right = secndchild;
        secndchild.right = z;
        z.child = secndchild;
        firstchild.left = secndchild.left;
        if (firstchild.left != null) {
          totalCount++;
          firstchild.left.right = firstchild;
        }
        secndchild.left = firstchild;
      }
    }
    //Make z1 and z2 the last two children of z by linking both subtrees to z.
    if (z.child != null) {
      totalCount++;
      z.child.right = z2;
    }
    z2.right = z1;
    z1.right = z;
    z2.left = z.child;
    z1.left = z2;
    z.child = z1;
    z.left = null;
    //increment rz
    z.updateRank(true);
    return z;
  }

  private ViolationNode promoteRootsChildren() {
    //Fails apparently if there is only one root but it has children.
    //Pre-condition: The root is not null.
    //Post-condition: All of the root's children are added to the root list. The root is also removed.
    //There are four things I need to find rightchild, leftchild, nextroot, and lastroot.
    //If root.right=root, then nextroot=leftchild and lastroot=rightchild.
    totalCount++;
    ViolationNode leftchild = root.getLeftMostChildDestr();
    ViolationNode rightchild = root.child;
    ViolationNode nextroot = root.right;
    ViolationNode last = lastOfRootList();
    if (rightchild == null && nextroot == root) {
      totalCount++;
      return null;
    }
    else if (nextroot == root) {
      totalCount++;
      nextroot = leftchild;
      last = rightchild;
    }
    else if (rightchild == null) {
      totalCount++;
      rightchild = last;
      leftchild = nextroot;
    }
    rightchild.right = nextroot;
    last.right = leftchild;
    return last;
  }

  private ViolationNode findNewMin() {
    //Pre-condition: The old root has been removed and replaced by an arbitrary member of the root list.
    //Post-condition: The structure is unchanged.
    //Return: The node with the lowest key in the root list is found and returned.
    ViolationNode min = root;
    totalCount++;
    for (ViolationNode walk = root.right; walk != root; walk = walk.right) {
      totalCount++;
      if (walk.key < min.key) {
        totalCount++;
        min = walk;
      }
    }
    return min;
  }

  /**
   * Get the last member of the root list.
   */
  public ViolationNode lastOfRootList() {
    ViolationNode walk = root;
    totalCount++;
    while (walk.right != root) {
      totalCount++;
      walk = walk.right;
    }
    return walk;
  }

  public static void main(String[] args) throws FileNotFoundException,
      UnsupportedEncodingException {
    int numkeys = 100000;
    List<Integer> keys = new ArrayList<Integer>(numkeys);
    ViolationHeap heap = new ViolationHeap();
    List<ViolationNode> nodes = new ArrayList<ViolationNode>(numkeys);
    Random rng = new Random();
    int[] inserts = new int[numkeys];
    int[] deckeys = new int[numkeys];
    int[] delMins = new int[numkeys];
    int runs = 10000;
    for (int j = 0; j < runs; j++) {
      for (int i = 0; i < numkeys; i++) {
        keys.add(rng.nextInt(2000) + 5000);
        nodes.add(new ViolationNode(keys.get(i)));
        int temp = ViolationHeap.totalCount;
        heap.insert(nodes.get(i));
        inserts[i] += (ViolationHeap.totalCount - temp);
      }
      int guard = numkeys;
      for (int i = 0; i < guard; i++) {
        int key = rng.nextInt(nodes.size());
        int dec = 1 + rng.nextInt(19);
        while (keys.get(key) - dec < 0)
          key = rng.nextInt(nodes.size());
        int temp = ViolationHeap.totalCount;
        heap.decreaseKey(dec, nodes.get(key));
        deckeys[numkeys - i - 1] += (ViolationHeap.totalCount - temp);
        temp = ViolationHeap.totalCount;
        ViolationNode rem = heap.deleteMin();
        delMins[numkeys-i-1]+=(ViolationHeap.totalCount-temp);
        int index = nodes.indexOf(rem);
        nodes.remove(rem);
        keys.remove(index);
      }
      System.out.println("Run " + j + " finished");
    }
    PrintWriter writer1 = new PrintWriter("VHAveragedOutputInsert.txt", "UTF-8");
    PrintWriter writer2 = new PrintWriter("VHAveragedOutputDecKey.txt", "UTF-8");
    PrintWriter writer3 = new PrintWriter("VHAveragedOutputDelMin.txt", "UTF-8");
    writer1.println("nodes, steps");
    writer2.println("nodes, steps");
    writer3.println("nodes, steps");
    for (int i = 0; i < numkeys; i++) {
      writer1.println(i + 1 + "," + (double)inserts[i]/runs);
      writer2.println(i + 1 + "," + (double)deckeys[i]/runs);
      writer3.println(i + 1 + "," + (double)delMins[i]/runs);
    }
    writer1.close();
    writer2.close();
    writer3.close();
    System.out.println("Finished");
  }
}
