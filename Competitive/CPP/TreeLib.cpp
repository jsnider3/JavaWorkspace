//Author: Josh Snider
//See HackerRank -> Data Structures -> Tree

/*  
Node is defined as  

struct node
{
    int data;
    node* left;
    node* right;
};

*/

/*
* Get max height of the tree.
*/
int
height(node *root)
{
  int max = 0;
  if (root != nullptr) {
    int leftHeight = height(root->left) + 1;
    int rightHeight = height(root->right) + 1;
    max = std::max(leftHeight, rightHeight);
  }
  return max;
}

/*
* Print tree in normal order.
*/
void
Inorder(node *root) {
  if (root != nullptr) {
    Inorder(root->left);
    std::cout << root->data << " ";
    Inorder(root->right);
  }
}

/*
* Insert into binary search tree.
*/
node*
insert(node *root, int value)
{
  if (root == nullptr) {
    root = (node*)malloc(sizeof(node));
    root->left = nullptr;
    root->right = nullptr;
    root->data = value;
  } else if (value < root->data) {
    root->left = insert(root->left, value);
  } else if (value > root->data) {
    root->right = insert(root->right, value);
  }
  return root;
}

/*
* Print tree in level order.
*/
void
LevelOrder(node *root)
{
  std::vector<node*> from;
  std::vector<node*> to;
  from.push_back(root);
  while (from.size() > 0) {
    for (const node *n : from) {
      if (n != nullptr) {
        std::cout << n->data << " ";
        to.push_back(n->left);
        to.push_back(n->right);
      }
    }
    from = to;
    to = std::vector<node*>();
  }
}

/*
* Print tree in post order.
*/
void
Postorder(node *root) {
  if (root != nullptr) {
    Postorder(root->left);
    Postorder(root->right);
    std::cout << root->data << " ";
  }
}

/*
* Print tree in preorder.
*/
void
Preorder(node *root) {
  if (root != nullptr) {
    std::cout << root->data << " ";
    Preorder(root->left);
    Preorder(root->right);
  }
}

void
top_view(node *root)
{
  //Passes, but buggy if tree is seriously unbalanced.
  std::vector<int> lefties;
  node *leftit = root;
  while (leftit != nullptr) {
    lefties.insert(lefties.begin(),leftit->data); 
    leftit = leftit->left;
  }
  node *rightit = root->right;
  while (rightit != nullptr) {
    lefties.push_back(rightit->data); 
    rightit = rightit->right;
  }    
  for (const int num : lefties) {
    std::cout << num << " ";
  }
}
