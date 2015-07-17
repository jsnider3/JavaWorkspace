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
* Determine if value is in tree.
*/
bool
contains(node *root, int value)
{
  bool found = false;
  if (root == nullptr) {
  } else if (value < root->data) {
    found = contains(root->left, value);
  } else if (value > root->data) {
    found = contains(root->right, value);
  } else if (value == root->data) {
    found = true;
  }
  return found;
}

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
* Lowest common ancestor of two values in the tree.
*/
node*
lca(node * root, int v1,int v2)
{
  if (contains(root->left, v1) && contains(root->left, v2)) {
    return lca(root->left, v1, v2);
  } else if (contains(root->right, v1) && contains(root->right, v2)) {
    return lca(root->right, v1, v2);
  } else {
    return root;
  } 
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
  while (!from.empty()) {
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
