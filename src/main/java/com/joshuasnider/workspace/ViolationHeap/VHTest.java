import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

import org.junit.Test;

public class VHTest {

  @Test
  public void makeHeap() {
    ViolationHeap empty = new ViolationHeap();
    assertTrue(empty.root==null&&empty.findMin()==null);
  }

  @Test
  public void singleton() {
    ViolationHeap singleton = new ViolationHeap();
    ViolationNode ins = new ViolationNode(5);
    singleton.insert(ins);
    assertTrue(singleton.root==ins&&ins.right==ins&&ins.left==null);
  }

  @Test
  public void secondElement() {
    ViolationHeap dos = new ViolationHeap();
    ViolationNode first = new ViolationNode(5);
    dos.insert(first);
    ViolationNode second = new ViolationNode(4);
    dos.insert(second);
    assertTrue(dos.root==second&&second.right==first&&first.right==second);
  }

  @Test
  public void decKey() {
    ViolationHeap dos = new ViolationHeap();
    ViolationNode first = new ViolationNode(5);
    dos.insert(first);
    ViolationNode second = new ViolationNode(4);
    dos.insert(second);
    dos.decreaseKey(2,first);
    assertTrue(dos.root==first&&second.right==first&&first.right==second);
  }

  @Test
  public void decKeyWithFour() throws Exception {
    ViolationHeap dos = new ViolationHeap();
    int numNodes=4;
    ViolationNode[] nodes = new ViolationNode[numNodes];
    for(int i=0;i<numNodes;i++){
      nodes[i]=new ViolationNode(i+1);
      dos.insert(nodes[i]);
    }
    dos.decreaseKey(10,nodes[3]);
    //assert(dos.root==nodes[3]);
    dos.deleteMin();
    dos.decreaseKey(3,dos.root.child);
    assertTrue(dos.root.key==-1&&dos.root.right.key==1&&dos.root.right.child.key==3);
    dos.decreaseKey(3,dos.root.right.child);
    assertTrue(dos.root.child==null&&dos.root.right.child==null&&dos.root.right.right.child==null);
  }

  @Test
  public void decKeyWithTwentySeven() throws Exception {
    ViolationHeap dos = new ViolationHeap();
    int numNodes=27;
    ViolationNode[] nodes = new ViolationNode[numNodes];
    for(int i=0;i<numNodes;i++){
      nodes[i]=new ViolationNode(i+1);
      dos.insert(nodes[i]);
    }
    dos.deleteMin();
    checkHeap(dos);
    dos.decreaseKey(19,dos.root.child);
    checkHeap(dos);
    dos.decreaseKey(3,dos.root.right.child);
    checkHeap(dos);
    dos.decreaseKey(1,dos.root.right.child.left.left);
    checkHeap(dos);
  }

  @Test
  public void decKeyWithMany() throws Exception {
    ViolationHeap dos = new ViolationHeap();
    int numNodes=999;
    ViolationNode[] nodes = new ViolationNode[numNodes];
    for(int i=0;i<numNodes;i++){
      nodes[i]=new ViolationNode(i+1);
      dos.insert(nodes[i]);
    }
    dos.deleteMin();
    dos.decreaseKey(999,dos.root.child.child.child.child.child.child);
    checkHeap(dos);
  }

  @Test
  public void delMin() throws Exception  {
    ViolationHeap dos = new ViolationHeap();
    ViolationNode first = new ViolationNode(5);
    dos.insert(first);
    ViolationNode second = new ViolationNode(4);
    dos.insert(second);
    dos.deleteMin();
    assertTrue(dos.root==first&&first.right==first&&first.left==null);
  }

  @Test
  public void testGetParent() throws Exception  {
    ViolationHeap big = new ViolationHeap();
    int numNodes=9;
    ViolationNode[] array = new ViolationNode[numNodes];
    for(int i=numNodes;i>0;i--){
      array[i-1]=new ViolationNode(i);
      big.insert(array[i-1]);
    }
    big.deleteMin();
    assertTrue(big.root.getParent()==null);
    assertTrue(big.root.child.getParent()==big.root);
    assertTrue(big.root.child.left.getParent()==big.root);
    assertTrue(big.root.right.child.getParent()==big.root.right);
  }

  @Test
  public void delMinEmpty() throws Exception {
    ViolationHeap dos = new ViolationHeap();
    ViolationNode first = new ViolationNode(5);
    dos.insert(first);
    dos.deleteMin();
    assertTrue(dos.root==null);
  }

  @Test
  public void threeWay1() {
    ViolationNode first = new ViolationNode(3);
    first.rank=1;
    ViolationNode second = new ViolationNode(4);
    second.rank=1;
    ViolationNode third = new ViolationNode(5);
    third.rank=1;
    ViolationHeap.threeWayJoin(first,second,third);
    assertTrue(first.child==second&&second.left==third&&second.right==first);
  }

  @Test
  public void threeWay2() {
    ViolationNode first = new ViolationNode(3);
    first.rank=1;
    ViolationNode second = new ViolationNode(4);
    second.rank=1;
    ViolationNode third = new ViolationNode(5);
    third.rank=1;
    ViolationHeap.threeWayJoin(second,first,third);
    assertTrue(first.child==second&&second.left==third&&second.right==first);
  }

  @Test
  public void insertNine() throws Exception {
    ViolationHeap big = new ViolationHeap();
    ViolationNode[] array = new ViolationNode[9];
    for(int i=9;i>0;i--){
      array[i-1]=new ViolationNode(i);
      big.insert(array[i-1]);
    }
    boolean result=true;
    int cnt=1;
    for(ViolationNode temp=big.findMin();temp.right!=big.root;temp=temp.right){
      result&=(temp.left==null);
      cnt++;
    }
    assertTrue(result&&(cnt==9));
  }
  @Test
  public void delMinWithNine() throws Exception {
    ViolationHeap big = new ViolationHeap();
    ViolationNode[] array = new ViolationNode[9];
    for(int i=9;i>0;i--){
      array[i-1]=new ViolationNode(i);
      big.insert(array[i-1]);
    }
    big.deleteMin();
    boolean result=true;
    int cnt=1;
    for(ViolationNode temp=big.findMin();temp.right!=big.root;temp=temp.right){
      result&=(temp.left==null);
      result&=(temp.child==null||(temp.key<=temp.child.key));
      result&=((temp.rank==1&&temp.child!=null)||(temp.rank==0&&temp.child==null));
      cnt++;
    }
    result&=(cnt==4);
    assertTrue(result);
  }
  @Test
  public void delMinWithThirty() throws Exception {
    ViolationHeap big = new ViolationHeap();
    ViolationNode[] array = new ViolationNode[30];
    for(int i=30;i>0;i--){
      array[i-1]=new ViolationNode(i);
      big.insert(array[i-1]);
    }
    boolean result=true;
    big.deleteMin();
    assertTrue(checkHeap(big)==29);
    for(ViolationNode temp=big.findMin();temp.right!=big.root;temp=temp.right){
      result&=(temp.left==null);
      result&=(temp.child==null||(temp.key<=temp.child.key));
      result&=((temp.rank==1&&temp.child!=null)||(temp.rank==0&&temp.child==null));
    }
    big.deleteMin();
    for(ViolationNode temp=big.findMin();temp.right!=big.root;temp=temp.right){
      result&=(temp==big.findMin()||temp.key>=big.findMin().key);
      result&=(temp.left==null);
      result&=(temp.child==null||(temp.key<=temp.child.key));
      result&=((temp.rank==1&&temp.child!=null)||(temp.rank==0&&temp.child==null));
    }
    assert(result&&checkHeap(big)==28);
  }

  public static void replicateResult(String filename) throws Exception{
    Scanner br = new Scanner(new File(filename));
    String line;
    ArrayList<ViolationNode> nodes = new ArrayList<ViolationNode>();
    ViolationHeap heap = new ViolationHeap();
    int cnt=0;
    boolean wait=false;
    while (br.hasNextLine()) {
      line=br.nextLine();
      Scanner sc = new Scanner(line);
      int command =sc.nextInt();
      System.out.println(line);
      if(line.equals("2 4 51"))
        System.out.println("Prepare to crash.");
      switch(command){
      case 1:
        ViolationNode node=new ViolationNode(sc.nextInt());
        nodes.add(node);
        heap.insert(node);
        break;
      case 2:
        int dec=sc.nextInt();
        ViolationNode target = nodes.get(sc.nextInt());
        heap.decreaseKey(dec,target);
        break;
      case 3:
        ViolationNode rem=heap.deleteMin();
        nodes.remove(rem);
      default:
      }
      sc.close();
      checkHeap(heap);
    }
    br.close();
  }

  public static int checkHeap(final ViolationHeap heap){
    int count=0;
    ArrayList<ViolationNode> accum = new ArrayList<ViolationNode>();
    for(ViolationNode walk=heap.root;walk!=null;walk=walk.right){
      if(count>=1&&walk==heap.root)
        break;
      assertTrue(walk.left==null);
      assertTrue(walk.key>=heap.root.key);
      assertTrue(walk.child==null||walk.child.right==walk);
      count+=checkHeapHelper(walk,accum);
    }
    return count;
  }
  public static int checkHeapHelper(final ViolationNode heap,ArrayList<ViolationNode> accum){
    int parentkey=heap.key;
    int count=1;
    assertTrue(accum.indexOf(heap)==-1);
    accum.add(heap);
    if(heap.child==null)
      return 1;
    else{
      for(ViolationNode walk=heap.child;walk!=null;walk=walk.left){
        assertTrue(walk.right!=null);
        assertTrue(walk.key>=parentkey);
        if(!(walk.left==null||walk.left.right==walk)){
          assertTrue(walk.left==null);
          assertTrue(walk.left.right==walk);
        }
        assertTrue(walk.child==null||walk.child.right==walk);
        count+=checkHeapHelper(walk,accum);
      }
    }
    return count;
  }
}
