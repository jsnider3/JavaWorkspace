package com.joshuasnider.workspace.violationheap;

import static org.junit.Assert.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import org.junit.Test;

public class VHTest {

  @Test
  public void makeHeap() {
    ViolationHeap empty = new ViolationHeap();
    assertTrue(empty.root == null && empty.findMin() == null);
  }

  @Test
  public void singleton() {
    ViolationHeap singleton = new ViolationHeap();
    ViolationNode ins = new ViolationNode(5);
    singleton.insert(ins);
    assertTrue(singleton.root == ins && ins.right == ins &&
               ins.left == null);
  }

  @Test
  public void secondElement() {
    ViolationHeap dos = new ViolationHeap();
    ViolationNode first = new ViolationNode(5);
    dos.insert(first);
    ViolationNode second = new ViolationNode(4);
    dos.insert(second);
    assertTrue(dos.root == second && second.right == first &&
               first.right == second);
  }

  @Test
  public void decKey() {
    ViolationHeap dos = new ViolationHeap();
    ViolationNode first = new ViolationNode(5);
    dos.insert(first);
    ViolationNode second = new ViolationNode(4);
    dos.insert(second);
    dos.decreaseKey(2, first);
    assertTrue(dos.root == first && second.right == first &&
               first.right == second);
  }

  @Test
  public void decKeyWithFour() {
    ViolationHeap dos = new ViolationHeap();
    int numNodes=4;
    ViolationNode[] nodes = new ViolationNode[numNodes];
    for (int i = 0; i < numNodes; i++) {
      nodes[i] = new ViolationNode(i + 1);
      dos.insert(nodes[i]);
    }
    dos.decreaseKey(10, nodes[3]);
    dos.deleteMin();
    dos.decreaseKey(3, dos.root.child);
    assertTrue(dos.root.key == -1 && dos.root.right.key == 1 &&
               dos.root.right.child.key == 3);
    dos.decreaseKey(3, dos.root.right.child);
    assertTrue(dos.root.child == null && dos.root.right.child == null &&
               dos.root.right.right.child == null);
  }

  @Test
  public void decKeyWithTwentySeven() {
    ViolationHeap dos = new ViolationHeap();
    int numNodes = 27;
    ViolationNode[] nodes = new ViolationNode[numNodes];
    for(int i = 0; i < numNodes; i++) {
      nodes[i] = new ViolationNode(i + 1);
      dos.insert(nodes[i]);
    }
    dos.deleteMin();
    assertTrue(checkHeap(dos) == 26);
    dos.decreaseKey(19, dos.root.child);
    assertTrue(checkHeap(dos) == 26);
    dos.decreaseKey(3, dos.root.right.child);
    assertTrue(checkHeap(dos) == 26);
    dos.decreaseKey(1, dos.root.right.child.left.left);
    assertTrue(checkHeap(dos) == 26);
  }

  @Test
  public void decKeyWithMany() {
    ViolationHeap dos = new ViolationHeap();
    int numNodes = 999;
    ViolationNode[] nodes = new ViolationNode[numNodes];
    for(int i = 0; i < numNodes; i++) {
      nodes[i] = new ViolationNode(i + 1);
      dos.insert(nodes[i]);
    }
    dos.deleteMin();
    dos.decreaseKey(999, dos.root.child.child.child.child.child.child);
    assertTrue(checkHeap(dos) == 998);
  }

  @Test
  public void delMin() {
    ViolationHeap dos = new ViolationHeap();
    ViolationNode first = new ViolationNode(5);
    dos.insert(first);
    ViolationNode second = new ViolationNode(4);
    dos.insert(second);
    dos.deleteMin();
    assertTrue(dos.root == first && first.right == first &&
               first.left == null);
  }

  @Test
  public void testGetParent() {
    ViolationHeap big = new ViolationHeap();
    int numNodes=9;
    ViolationNode[] array = new ViolationNode[numNodes];
    for (int i = numNodes; i > 0; i--) {
      array[i - 1] = new ViolationNode(i);
      big.insert(array[i - 1]);
    }
    big.deleteMin();
    assertTrue(big.root.getParent() == null);
    assertTrue(big.root.child.getParent() == big.root);
    assertTrue(big.root.child.left.getParent() == big.root);
    assertTrue(big.root.right.child.getParent() == big.root.right);
  }

  @Test
  public void delMinEmpty() {
    ViolationHeap dos = new ViolationHeap();
    ViolationNode first = new ViolationNode(5);
    dos.insert(first);
    dos.deleteMin();
    assertTrue(dos.root == null);
  }

  @Test
  public void threeWay1() {
    ViolationNode first = new ViolationNode(3);
    first.rank = 1;
    ViolationNode second = new ViolationNode(4);
    second.rank = 1;
    ViolationNode third = new ViolationNode(5);
    third.rank = 1;
    ViolationHeap.threeWayJoin(first, second, third);
    assertTrue(first.child == second && second.left == third &&
               second.right == first);
  }

  @Test
  public void threeWay2() {
    ViolationNode first = new ViolationNode(3);
    first.rank = 1;
    ViolationNode second = new ViolationNode(4);
    second.rank = 1;
    ViolationNode third = new ViolationNode(5);
    third.rank = 1;
    ViolationHeap.threeWayJoin(second, first, third);
    assertTrue(first.child == second && second.left == third &&
               second.right == first);
  }

  @Test
  public void insertNine() {
    ViolationHeap big = new ViolationHeap();
    ViolationNode[] array = new ViolationNode[9];
    for (int i = 9; i > 0; i--) {
      array[i - 1] = new ViolationNode(i);
      big.insert(array[i - 1]);
    }
    int cnt = 1;
    for (ViolationNode temp = big.findMin(); temp.right != big.root;
         temp = temp.right) {
      assertTrue(temp.left == null);
      cnt++;
    }
    assertTrue(cnt == 9);
  }

  @Test
  public void delMinWithNine() {
    ViolationHeap big = new ViolationHeap();
    ViolationNode[] array = new ViolationNode[9];
    for (int i = 9; i > 0; i--) {
      array[i - 1]=new ViolationNode(i);
      big.insert(array[i - 1]);
    }
    big.deleteMin();
    int cnt = 1;
    for (ViolationNode temp = big.findMin(); temp.right != big.root;
         temp = temp.right) {
      assertTrue(temp.left == null);
      assertTrue(temp.child == null || (temp.key <= temp.child.key));
      assertTrue((temp.rank == 1 && temp.child != null) ||
                 (temp.rank == 0 && temp.child == null));
      cnt++;
    }
    assertTrue(cnt == 4);
  }

  @Test
  public void delMinWithThirty() {
    ViolationHeap big = new ViolationHeap();
    ViolationNode[] array = new ViolationNode[30];
    for (int i = 30; i > 0; i--) {
      array[i - 1] = new ViolationNode(i);
      big.insert(array[i - 1]);
    }
    big.deleteMin();
    assertTrue(checkHeap(big) == 29);
    for (ViolationNode temp = big.findMin(); temp.right != big.root;
         temp = temp.right) {
      assertTrue(temp.left == null);
      assertTrue(temp.child == null || (temp.key <= temp.child.key));
      assertTrue((temp.rank > 0 && temp.child != null) ||
                (temp.rank == 0 && temp.child == null));
    }
    big.deleteMin();
    for (ViolationNode temp = big.findMin(); temp.right != big.root;
         temp=temp.right) {
      assertTrue(temp == big.findMin() || temp.key >= big.findMin().key);
      assertTrue(temp.left == null);
      assertTrue(temp.child == null || (temp.key <= temp.child.key));
      assertTrue((temp.rank > 0 && temp.child != null) ||
                (temp.rank == 0 && temp.child == null));
    }
    assertTrue(checkHeap(big) == 28);
  }

  public static void replicateResult(String filename) {
    try {
      Scanner br = new Scanner(new File(filename));
      String line;
      ArrayList<ViolationNode> nodes = new ArrayList<ViolationNode>();
      ViolationHeap heap = new ViolationHeap();
      while (br.hasNextLine()) {
        line = br.nextLine();
        Scanner sc = new Scanner(line);
        int command = sc.nextInt();
        System.out.println(line);
        switch (command) {
          case 1:
            ViolationNode node = new ViolationNode(sc.nextInt());
            nodes.add(node);
            heap.insert(node);
            break;
          case 2:
            int dec = sc.nextInt();
            ViolationNode target = nodes.get(sc.nextInt());
            heap.decreaseKey(dec, target);
            break;
          case 3:
            ViolationNode rem = heap.deleteMin();
            nodes.remove(rem);
            break;
        }
        sc.close();
        checkHeap(heap);
      }
      br.close();
    } catch (FileNotFoundException ex) {
      fail(ex.toString());
    }
  }

  /**
   * Assert that the heap is in a valid state.
   * @return Size of the heap.
   */
  public static int checkHeap(final ViolationHeap heap) {
    int count = 0;
    ArrayList<ViolationNode> accum = new ArrayList<ViolationNode>();
    for (ViolationNode walk = heap.root; walk != null; walk = walk.right) {
      if (count >= 1 && walk == heap.root)
        break;
      assertTrue(walk.left == null);
      assertTrue(walk.key >= heap.root.key);
      assertTrue(walk.child == null || walk.child.right == walk);
      count += checkHeapHelper(walk,accum);
    }
    return count;
  }

  /**
   * Assert that the subtree of a heap at a given node is in a valid state.
   * @return Size of the subheap.
   */
  public static int checkHeapHelper(final ViolationNode heap,
      List<ViolationNode> accum) {
    int parentkey = heap.key;
    int count = 1;
    assertTrue(accum.indexOf(heap) == -1);
    accum.add(heap);
    if (heap.child == null) {
      return 1;
    } else {
      for (ViolationNode walk = heap.child; walk != null; walk=walk.left) {
        assertTrue(walk.right != null);
        assertTrue(walk.key >= parentkey);
        if (!(walk.left == null || walk.left.right == walk)) {
          assertTrue(walk.left == null);
          assertTrue(walk.left.right == walk);
        }
        assertTrue(walk.child == null || walk.child.right == walk);
        count += checkHeapHelper(walk, accum);
      }
    }
    return count;
  }
}
