/*
  Author: Joshua Snider
  NOTE: These are all extremely functional and very C-ish,
    they are not a good example of idiomatic C++.

  See HackerRank -> Data Structures -> Linked List.
  Print elements of a linked list on console 
  head pointer input could be NULL as well for empty list
  Node is defined as 
  struct Node
  {
     int data;
     Node *next;
     Node *prev;
  }
*/

/*
* Reverse a doubly linked list.
*/
Node*
Reverse(Node* head)
{
  auto *iter = head;
  Node *back = nullptr;
  while (head != nullptr) {
      auto *temp = head->next;
      head->next = back;
      head->prev = temp;
      back = head;
      head = temp;
  }
  return back;
}

/*
* Insert Node in a doubly sorted linked list 
*/
Node*
SortedInsert(Node *head, int data)
{
  Node *new_node = (Node*)malloc(sizeof(Node));
  new_node->data = data;
  if (head == nullptr) {
    return new_node;
  } else if (head->data >= data) {
    new_node->next = head;
    head->prev = new_node;
    return new_node;
  } else {
    Node *iter = head;
    while (iter->next != nullptr && iter->next->data < data) {
      iter = iter->next;
    }
    new_node->next = iter->next;
    iter->next = new_node;
    new_node->prev = iter;
    if (new_node->next != nullptr) {
      new_node->next->prev = new_node;
    }
    return head;
  }
}

