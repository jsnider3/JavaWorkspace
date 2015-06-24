import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Random;

public class ViolationHeap {
	static int totalCount=0;
	public ViolationNode root;
	
	public ViolationNode findMin(){//– find-min(h): Return the first root of h.
		totalCount++;
		return root;
	}
	
	public void insert(ViolationNode ins){
		//A single node x is inserted into the root list of h.
		//The rank of x is initially set to zero. 
		ins.left=null;
		totalCount++;
		if(root==null){
			totalCount++;
			root=ins;
			ins.right=ins;
		}
		ins.right=root.right;//Insert the node into the array.
		root.right=ins;
		if(ins.key<root.key){//Move the root as needed.
			totalCount++;
			root=ins;
		}
	}
	
	public void union(ViolationHeap other){
		//The root lists of h1 and h2 are combined in a new list
		totalCount++;
		ViolationNode temp=root.right;
		root.right=other.root.right;
		other.root.right=temp;
		root=root.key<other.root.key?root:other.root;
		other.root=root;
	}
	
	public void decreaseKey(int n,ViolationNode x){
		totalCount++;
		if(n<0){
			totalCount++;
			throw new RuntimeException("This is decreaseKey, not increaseKey.");
		}
		x.key-=n;//Subtract n from the key of x.
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
			x.updateRank(true);//Recalculate the rank of x using (1).
			//Promote x’s subtree as a tree in h, and make x the first root if its new
			//value is smaller than the minimum.
			this.insert(x);
			/* Propagate rank updates by traversing the path of ancestors of x’s old position,
			 * as long as the visited node is active and as long as its recalculated rank using
			 * (1) is smaller than its old rank.*/
			while(parent!=null&&parent.isActive())
			{
				//TODO might need to do something involving critical nodes.
				totalCount++;
				parent.updateRank(false);
				parent=parent.getParent();
			}
		}
	}

	public ViolationNode deleteMin(){
		/*
		//Edge cases:
		//root has no children.
		*/
		//Make each of its subtrees a tree in h and remove from h the first root
		totalCount++;
		final ViolationNode oldRoot=root;
		root=promoteRootsChildren();
		if(root!=null){
			totalCount++;
			//DESTRUCTIVELY, Create an array containing all nodes of the root list.
			ArrayList<ViolationNode> allNodes = new ArrayList<ViolationNode>();
			ViolationNode temp;
			for(ViolationNode walk=root;walk!=root||allNodes.isEmpty();walk=temp){
				totalCount++;
				allNodes.add(walk);
				temp=walk.right;//This is the most likely node to have a nullPointerException.
				walk.right=null;
			}
			//Repeatedly 3-way-join trees of equal rank until no three trees of
			//the same rank remain.
			ArrayList<ArrayList<ViolationNode>> list = new ArrayList<ArrayList<ViolationNode>>();
			for(ViolationNode node:allNodes){
				totalCount++;
				insertIntoJoinList(list,node);
			}
			//Get the nodes back out of the datastructure.
			root=extractNodes(list);
			/*Finally, the root with the new minimum value is moved to the first position in the root list.*/
			root=findNewMin();
		}
		return oldRoot;
	}
	
	private ViolationNode extractNodes(ArrayList<ArrayList<ViolationNode>> list){
		totalCount++;
		ViolationNode head=null;
		ViolationNode tail=null;
		ArrayList<ViolationNode> all=new ArrayList<ViolationNode>();
		for(ArrayList<ViolationNode> sublist:list){
			totalCount++;
			all.addAll(sublist);
		}
		for(ViolationNode node:all){
			totalCount++;
			if(head==null){
				totalCount++;
				head=node;
				head.right=node;
				tail=node;
			}else{
				totalCount++;
				tail.right=node;
				tail=node;
			}
		}
		tail.right=head;
		return head;
	}
	
	private void insertIntoJoinList(ArrayList<ArrayList<ViolationNode>> list,ViolationNode node){
		//Pre-condition: list.size()>node.rank and each element is either empty or contains nodes.
		totalCount++;
		int rank=node.rank;
		while(list.size()<=rank){
			totalCount++;
			list.add(new ArrayList<ViolationNode>());
		}
		ArrayList<ViolationNode> sublist=list.get(rank);
		switch(sublist.size()){
			case 0:
			case 1:
				sublist.add(node);
				break;
			case 2: 
				ViolationNode n=threeWayJoin(node,sublist.get(0),sublist.get(1));
				list.set(rank,new ArrayList<ViolationNode>());
				insertIntoJoinList(list,n);
				break;
			default: throw new RuntimeException("Something mysterious went wrong with insertIntoJoinList");
		}
	}
	
	public static ViolationNode threeWayJoin(ViolationNode z, ViolationNode z1, ViolationNode z2){
		//Pre-condition: z.rank=z1.rank=z2.rank
		//Post-condition: The node with the lowest key gains the others as its last two children.
		//The one with the larger rank is the last child.
		/*3-way-join(z, z1, z2)
			Assume w.l.o.g. that z’s value is not larger than that of z1 and z2.
		*/
		//Make sure that z is the smallest and that z1.rank>z2.rank.
		totalCount++;
		if(z.key>z1.key){
			totalCount++;
			ViolationNode temp=z;
			z=z1;
			z1=temp;
		}
		if(z.key>z2.key){
			totalCount++;
			ViolationNode temp=z;
			z=z2;
			z2=temp;
		}
		//Ensure that the active child of z with the larger rank is the last child.
		if(z.child!=null&&z.child.left!=null){
			totalCount++;
			ViolationNode firstchild=z.child;
			ViolationNode secndchild=z.child.left;
			if(firstchild.rank<secndchild.rank){
				totalCount++;
				firstchild.right=secndchild;
				secndchild.right=z;
				z.child=secndchild;
				firstchild.left=secndchild.left;
				if(firstchild.left!=null){
					totalCount++;
					firstchild.left.right=firstchild;
				}
				secndchild.left=firstchild;
			}
		}
		//Make z1 and z2 the last two children of z by linking both subtrees to z.
		if(z.child!=null){
			totalCount++;
			z.child.right=z2;
		}
		z2.right=z1;
		z1.right=z;
		z2.left=z.child;
		z1.left=z2;
		z.child=z1;
		z.left=null;
		//increment rz
		z.updateRank(true);
		return z;
	}
	
	private ViolationNode promoteRootsChildren(){
		//Fails apparently if there is only one root but it has children.
		//Pre-condition: The root is not null.
		//Post-condition: All of the root's children are added to the root list. The root is also removed.
		//There are four things I need to find rightchild, leftchild, nextroot, and lastroot.
		//If root.right=root, then nextroot=leftchild and lastroot=rightchild.
		totalCount++;
		ViolationNode leftchild=root.getLeftMostChildDestr();
		ViolationNode rightchild=root.child;
		ViolationNode nextroot=root.right;
		ViolationNode last=lastOfRootList();
		if(rightchild==null&&nextroot==root){
			totalCount++;
			return null;
		}
		else if(nextroot==root){
			totalCount++;
			nextroot=leftchild;
			last=rightchild;
		}
		else if(rightchild==null){
			totalCount++;
			rightchild=last;
			leftchild=nextroot;
		}
		rightchild.right=nextroot;
		last.right=leftchild;
		return last;
	}
	
	private ViolationNode findNewMin(){
		//Pre-condition: The old root has been removed and replaced by an arbitrary member of the root list.
		//Post-condition: The structure is unchanged.
		//Return: The node with the lowest key in the root list is found and returned.
		ViolationNode min=root;
		totalCount++;
		for(ViolationNode walk=root.right;walk!=root;walk=walk.right){
			totalCount++;
			if(walk.key<min.key){
				totalCount++;
				min=walk;
			}
		}
		return min;
	}
	
	public ViolationNode lastOfRootList(){
		ViolationNode walk=root;
		totalCount++;
		while(walk.right!=root){
			totalCount++;
			walk=walk.right;
		}
		return walk;
	}
	
	public static void main(String[] args) throws Exception{
		int numkeys=100000;
		ArrayList<Integer> keys=new ArrayList<Integer>(numkeys);
		ViolationHeap heap=new ViolationHeap();
		ArrayList<ViolationNode> nodes = new ArrayList<ViolationNode>(numkeys);
		Random rng = new Random();
		//PrintWriter writer = new PrintWriter("VHOutputWithTimes4th.txt", "UTF-8");
		//PrintStream writer =System.out;
		int[] inserts = new int[numkeys];
		int[] deckeys= new int[numkeys];
		int[] delMins=new int[numkeys];
		int runs=10000;
		for(int j=0;j<runs;j++){
		for(int i=0;i<numkeys;i++){
			keys.add(rng.nextInt(2000)+5000);//i);//
			nodes.add(new ViolationNode(keys.get(i)));
			int temp=ViolationHeap.totalCount;
			//writer.println("Insert "+keys.get(i));
			heap.insert(nodes.get(i));
			//writer.println("With "+i+" nodes an insert takes "+(ViolationHeap.totalCount-temp)+" steps.");
			inserts[i]+=(ViolationHeap.totalCount-temp);
		}
		int guard=numkeys;
		for(int i=0;i<guard;i++){
			int key=rng.nextInt(nodes.size());
			int dec=1+rng.nextInt(19);
			while(keys.get(key)-dec<0)
				key=rng.nextInt(nodes.size());
			//writer.println("decreaseKey "+dec+" on "+key+"th node");
			int temp=ViolationHeap.totalCount;
			heap.decreaseKey(dec,nodes.get(key));
			//writer.println("With "+(guard-i)+" nodes a decreaseKey takes "+(ViolationHeap.totalCount-temp)+" steps.");
			deckeys[numkeys-i-1]+=(ViolationHeap.totalCount-temp);
			//CSVHTest.checkHeap(heap);
			//writer.println("deleteMin");
			temp=ViolationHeap.totalCount;
			ViolationNode rem=heap.deleteMin();
			//writer.println("With "+(guard-i)+" nodes a deleteMin takes "+(ViolationHeap.totalCount-temp)+" steps.");
			delMins[numkeys-i-1]+=(ViolationHeap.totalCount-temp);
			int index=nodes.indexOf(rem);
			nodes.remove(rem);
			keys.remove(index);
			//assertTrue(CSVHTest.checkHeap(heap)==numkeys-i-1);
			//if(i%1000==0)
				//System.out.println(guard-i);
		}
		System.out.println("Run "+j+" finished");
		}
		PrintWriter writer1 = new PrintWriter("VHAveragedOutputInsert.txt", "UTF-8");
		PrintWriter writer2 = new PrintWriter("VHAveragedOutputDecKey.txt", "UTF-8");
		PrintWriter writer3 = new PrintWriter("VHAveragedOutputDelMin.txt", "UTF-8");
		writer1.println("nodes,steps");
		writer2.println("nodes,steps");
		writer3.println("nodes,steps");
		for(int i=0;i<numkeys;i++){
			//inserts[i]=inserts[i]/runs;
			writer1.println(i+1+","+(double)inserts[i]/runs);
			//deckeys[i]=deckeys[i]/runs;
			writer2.println(i+1+","+(double)deckeys[i]/runs);
			//delMins[i]=delMins[i]/runs;
			writer3.println(i+1+","+(double)delMins[i]/runs);
		}
		writer1.close();
		writer2.close();
		writer3.close();
		System.out.println("Finished");
		//System.out.println(CSVHTest.checkHeap(heap));
	}
}
