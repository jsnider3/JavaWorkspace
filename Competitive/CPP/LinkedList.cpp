/*
  Author: Joshua Snider
  NOTE: These are all extremely functional and very C-ish,
    they are not a good example of idiomatic C++.

  See HackerRank -> Data Structures -> Linked List.
  Node is defined as 
  struct Node
  {
     int data;
     struct Node *next;
  }
*/

/*
* Compare two lists.
* Lists are equal if they have the same length and equal data
* in each position.
*/
bool
CompareLists(Node *headA, Node* headB)
{
  bool equal;
  if (headA == nullptr && headB != nullptr) {
    equal = false;
  } else if (headA != nullptr && headB == nullptr) {
    equal = false;
  } else if (headA == nullptr && headB == nullptr) {
    equal = true;
  } else {
    equal = headA->data == headB->data && CompareLists(headA->next, headB->next);
  }
  return equal;
}

/*
* Remove node at given position.
*/
Node*
Delete(Node *head, int position)
{
  if (position == 0) {
    return head->next;
  } else {
    head->next = Delete(head->next, position - 1);
    return head;
  }
}

/*
* Get Nth element from the end in a linked list.
* Given len(list) > N.
*/
int
GetNode(Node *head, int positionFromTail)
{
  auto *iter = head;
  int len = 0;
  while (iter != nullptr) {
    len++;
    iter = iter->next;
  }
  int positionFromHead = len - positionFromTail - 1;
  iter = head;
  for (int x = 0; x < positionFromHead; x++) {
    iter = iter->next;
  }
  return iter->data;
}

/*
* Check linked list for loops.
*/
bool
HasCycle(Node *head)
{
  if (head == nullptr) {
    return false;
  }
  auto *slow = head;
  auto *fast = head->next;
  while (fast != nullptr) {
    if (slow == fast) {
      return true;
    } else {
      slow = slow->next;
      fast = fast->next;
      if (fast != nullptr) {
        fast = fast->next;
      }
    }
  }
  return false;
}

/*
* Add to end of linked list.
*/
Node*
Insert(Node *head, int data)
{
  if (head == nullptr) {
    Node *end = (Node*)malloc(sizeof(Node));
    end->next = nullptr;
    end->data = data;
    return end;
  } else {
    head->next = Insert(head->next, data);
    return head;
  }
}

/*
* Add at given position of linked list.
*/
Node*
InsertNth(Node *head, int data, int position)
{
  if (position <= 0) {
    Node *ins = (Node*)malloc(sizeof(Node));
    ins->data = data;
    ins->next = head;
    return ins;
  } else {
    head->next = InsertNth(head->next, data, position - 1);
    return head;
  }
}

/*
* Merge two disjoint sorted lists A and B as one linked list.
*/
Node*
MergeLists(Node *headA, Node* headB)
{
  if (headA == nullptr) {
    return headB;
  } else if (headB == nullptr) {
    return headA;
  }
  Node *min;
  if (headA->data < headB->data) {
    min = headA;
    headA = headA->next;
  } else {
    min = headB;
    headB = headB->next;
  }
  Node *iter = min;
  while (headA != nullptr && headB != nullptr) {
    if (headA->data < headB->data) {
      iter->next = headA;
      iter = iter->next;
      headA = headA->next;
    } else {
      iter->next = headB;
      iter = iter->next;
      headB = headB->next;
    }
  }
  if (headA == nullptr) {
    iter->next = headB;
  } else {
    iter->next = headA;
  }
  return min;
}

/*
* Add to start of linked list.
*/
Node*
Prepend(Node *head,int data)
{
  Node *newhead = (Node*)malloc(sizeof(Node));
  newhead->next=head;
  newhead->data = data;
  return newhead;
}

/*
* Print a linked list.
*/
void
Print(Node *head)
{
  if (head != nullptr) {
    std::cout << head->data << std::endl;
    Print(head->next);
  }
}

/*
* Removes all duplicate elements from a sorted linked list.
*/
Node*
RemoveDuplicates(Node *head)
{
  auto *iter = head;
  while (iter != nullptr) {
    auto *next = iter->next;
    if (next != nullptr && iter->data == next->data) {
      iter->next = next->next;
    } else {
      iter = next;
    }
  }
  return head;
}

/*
* Reverse a linked list in-place and return the new head.
*/
Node*
Reverse(Node *head)
{
  auto *iter = head;
  Node *back = nullptr;
  while (head != nullptr) {
      auto *temp = head->next;
      head->next = back;
      back = head;
      head = temp;
  }
  return back;
}

/*
* Print a linked list in reverse.
*/
void
ReversePrint(Node *head)
{
  if (head != nullptr) {
    ReversePrint(head->next);
    std::cout << head->data << std::endl;
  }
}
